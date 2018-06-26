loadNumberingPlan <- function(filename)
{
  library(readxl)
  
  dt = read_excel(filename)
  
  if ('FG' %in% names(dt))
  {
    dt$prefix = paste("+44", dt$SABC, dt$`D/DE`, dt$FG, sep = "")
  }
  else
  {
    dt$prefix = paste("+44", dt$SABC, dt$`D/DE`, sep = "")
  }
  
  dt$operator = as.character(dt$`Communications Provider`)
  return (tbl_df(dt[,c("prefix", "operator")]))
}

loadAllNumberingPlans <- function()
{
  library(dplyr)
  library(tidyr)
  library(lubridate)
  
  numPlansDir = "~/Downloads/ofcom-numbering"
  
  all_nums = c()
  for (filename in list.files(path = numPlansDir, pattern = "*.xlsx"))
  {
    all_nums = bind_rows(all_nums, loadNumberingPlan(file.path(numPlansDir, filename)))
  }
  
  return (all_nums)
}

buildGroupCodes <- function(idc, countryCode, telPattern, telReplacement)
{
  codes = idc %>%
    filter(Country.Code==countryCode) %>%
    select("Country", "Area.Code") %>%
    filter(Area.Code!="")
  
  if (!is.null(telPattern))
  {
    codes$Area.Code = str_replace(codes$Area.Code, pattern = telPattern, replacement = telReplacement)
  }
  else
  {
    codes$Area.Code = paste(rep_len(telReplacement, length(codes$Area.Code)), codes$Area.Code, sep = "")
  }
  
  return (data.frame("operator" = codes$Country, "prefix" = codes$Area.Code, stringsAsFactors = FALSE))
}

loadInternationalDiallingCodes <- function()
{
  #See https://www.aggdata.com/free/international-calling-codes
  idc = read.csv("~/Downloads/globalareacodes.csv",header = TRUE, stringsAsFactors = FALSE)
  
  idc$Area.Code[idc$Area.Code=="684"] = "(1+)684"
  idc$Area.Code[idc$Area.Code=="(1)649"] = "(1+)649"
  
  nonNorthAmerica = idc %>%
    select("Country", "Country.Code") %>%
    distinct() %>%
    filter(!(Country.Code %in% c(1, 7, 212, 252, 262, 269, 290, 34, 358, 39, 44, 47, 61, 672, 90)))
  
  nonNorthAmerica$Country.Code = paste("+", as.character(nonNorthAmerica$Country.Code), sep = "")
  
  df = buildGroupCodes(idc, 1, "^\\(1\\+\\)", "+1")
  df = bind_rows(df, buildGroupCodes(idc, 7, "^\\(8\\)", "+78"))
  df = bind_rows(df, buildGroupCodes(idc, 212, "^\\(0\\)", "+212"))
  df = bind_rows(df, buildGroupCodes(idc, 252, NULL, "+252"))
  df = bind_rows(df, data.frame("operator"="French Reunion", "prefix"="+262", stringsAsFactors = FALSE))
  df = bind_rows(df, buildGroupCodes(idc, 269, NULL, "+269"))
  df = bind_rows(df, data.frame("operator"="Saint Helena", "prefix"="+290", stringsAsFactors = FALSE))
  df = bind_rows(df, data.frame("operator"="Spain", "prefix"="+34", stringsAsFactors = FALSE))
  df = bind_rows(df, data.frame("operator"="Finland", "prefix"="+35", stringsAsFactors = FALSE))
  df = bind_rows(df, data.frame("operator"="Spain", "prefix"="+39", stringsAsFactors = FALSE))
  df = bind_rows(df, data.frame("operator"="United Kingdom", "prefix"="+44", stringsAsFactors = FALSE))
  df = bind_rows(df, data.frame("operator"="Norway", "prefix"="+47", stringsAsFactors = FALSE))
  df = bind_rows(df, data.frame("operator"="Australia", "prefix"="+61", stringsAsFactors = FALSE))
  df = bind_rows(df, data.frame("operator"="Antartica", "prefix"="+672", stringsAsFactors = FALSE))
  df = bind_rows(df, buildGroupCodes(idc, 90, "^(\\(0\\))?", "+90"))
  
  return (df)
}

lookupOperator <- function(telNums, operatorTable)
{
  l = str_length(operatorTable$prefix)
  
  result = c()
  for (i in max(l):min(l))
  {
    lookup = left_join(telNums, operatorTable, by = c("prefix"="prefix"))
    if (nrow(lookup) > 0)
    {
      moreResults = filter(lookup, !is.na(lookup$operator))
      if (nrow(moreResults) > 0)
        result = bind_rows(result, moreResults)
      
      telNums = filter(lookup, is.na(lookup$operator))
      if (nrow(telNums) == 0)
        break
    }
  }
  
  return (result)
}

extractTelNumber <- function(telNums)
{
  return (str_replace(telNums, "^tel:", ""))
}

extractPrefix <- function(telNums)
{
  return (str_extract(telNums, "(^\\+447.{4})|(^\\+44(2|3|5|8|9).{5})|(^\\+4[^4])|(\\+[2-9].)|\\+1"))
}

loadPcapData <- function(filename)
{
  library(dplyr)
  library(tidyr)
  library(lubridate)
  
  dt = read.csv(filename, header = TRUE)
  
  dt$Timestamp = as.character(dt$Timestamp) %>% as.POSIXct
  
  #dt$hourOfDay = hour(dt$Timestamp)
  
  dt$N_Telnum = extractTelNumber(dt$N_Telnum)
  dt$P_Telnum = extractTelNumber(dt$P_Telnum)
  dt$G_Telnum = extractTelNumber(dt$G_Telnum)
  dt$D_Telnum = extractTelNumber(dt$D_Telnum)
  
  dt$N_Prefix = extractPrefix(dt$N_Telnum)
  dt$P_Prefix = extractPrefix(dt$P_Telnum)
  dt$G_Prefix = extractPrefix(dt$G_Telnum)
  dt$D_Prefix = extractPrefix(dt$D_Telnum)
  
  return (tbl_df(dt))
}

addOperators <- function(df, operatorTable)
{
  df = left_join(df, operatorTable, by=c("N_Prefix"="prefix"))
  df = rename(df, N_Op=operator)
  
  df = left_join(df, operatorTable, by=c("P_Prefix"="prefix"))
  df = rename(df, P_Op=operator) 
  
  df = left_join(df, operatorTable, by=c("G_Prefix"="prefix"))
  df = rename(df, G_Op=operator)
  
  df = left_join(df, operatorTable, by=c("D_Prefix"="prefix"))
  df = rename(df, D_Op=operator)
  
  return (df)
}

loadAllPcapData <- function()
{
  library(dplyr)
  library(tidyr)
  library(stringr)

  df = loadPcapData("~/Downloads/PCAP Trace Files/all_files.csv")
  all_nums = bind_rows(loadAllNumberingPlans(), loadInternationalDiallingCodes())
  
  df$N_Op = sapply(df$N_Telnum, function(x) lookupCountryCode(x, all_nums))
  df$P_Op = sapply(df$P_Telnum, function(x) lookupCountryCode(x, all_nums))
  df$G_Op = sapply(df$G_Telnum, function(x) lookupCountryCode(x, all_nums))
  #df = addOperators(df, all_nums)
  
  return (df)
}