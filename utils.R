loadNumberingPlan <- function(filename)
{
  library(readxl)
  
  print(filename)
  dt = read_xlsx(filename, guess_max = 50000)
  print(names(dt))
  
  result = c()
  
  if ('FG' %in% names(dt))
  {
    fgNums = dt[!is.na(dt$FG),]
    if (nrow(fgNums) > 0)
    {
      print(sprintf("FG rows = %d", nrow(fgNums)))
      fgNums$prefix = paste("+44", fgNums$SABC, fgNums$'D/DE', fgNums$FG, sep = "")
      result = bind_rows(result, fgNums)
    }
    dt = dt[is.na(dt$FG),]
  }
    
  if ('D/DE' %in% names(dt))
  {
    deNums = dt[!is.na(dt$'D/DE'),]
    if (nrow(deNums) > 0)
    {
      print(sprintf("D/DE rows = %d", nrow(deNums)))
      deNums$prefix = paste("+44", deNums$SABC, deNums$'D/DE', sep = "")
      result = bind_rows(result, deNums)
    }
    dt = dt[is.na(dt$'D/DE'),]
  }
  
  if ('SABC' %in% names(dt))
  {
    sabcNums = dt[!is.na(dt$SABC),]
    if (nrow(sabcNums) > 0)
    {
      print(sprintf("SABC rows = %d", nrow(sabcNums)))
      sabcNums$prefix = paste("+44", sabcNums$SABC, sep = "")
      result = bind_rows(result, sabcNums)
    }
    dt = dt[is.na(dt$SABC),]
  }
  
  print(sprintf("Unprocessed rows = %d", nrow(dt)))
  
  result$operator = as.character(result$`Communications Provider`)
  return (mutate(result[,c("prefix", "operator")], "org"="Ofcom"))
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
  
  return (mutate(df, "org"="ITU"))
}

lookupOperator <- function(telNums, operatorTable, isDebug = FALSE)
{
  l = str_length(operatorTable$prefix)
  
  result = c()
  for (i in max(l):min(l))
  {
    if (isDebug) print(sprintf("trying prefix: %d", i))
    
    i_prefix = telNums %>% mutate(prefix = substr(telNums$number, 1, i))
    
    lookup = left_join(i_prefix, operatorTable, by = c("prefix"="prefix"))
    if (nrow(lookup) > 0)
    {
      moreResults = filter(lookup, !is.na(lookup$operator))
      if (nrow(moreResults) > 0)
      {
        if (isDebug) print("Found results:")
        result = bind_rows(result, moreResults)
      }
      
      telNums = select(filter(lookup, is.na(lookup$operator)), -c(prefix, operator))
      if (nrow(telNums) == 0)
        break
    }
  }
  
  if (is.null(result)) return (NULL)
  
  return (select(result, -number))
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
  
  #dt$N_Prefix = extractPrefix(dt$N_Telnum)
  #dt$P_Prefix = extractPrefix(dt$P_Telnum)
  #dt$G_Prefix = extractPrefix(dt$G_Telnum)
  #dt$D_Prefix = extractPrefix(dt$D_Telnum)
  
  return (tbl_df(dt))
}

addOperators <- function(df, operatorTable, telNumField, prefixField, opField)
{
  result = data.frame("id"=df$id, "number"=df[[telNumField]]) %>%
    lookupOperator(operatorTable)

  if (is.null(result)) return (df)
  
  names(result) = sub("operator", opField, names(result))
  names(result) = sub("prefix", prefixField, names(result))
  
  return (left_join(df, result, by="id"))
}

loadAllPcapData <- function()
{
  library(dplyr)
  library(tidyr)
  library(stringr)

  df = loadPcapData("~/Downloads/PCAP Trace Files/all_files.csv") %>%
    mutate(id = row_number())
  
  opTable = bind_rows(loadAllNumberingPlans(), loadInternationalDiallingCodes())
  
  df = addOperators(df, opTable, "N_Telnum", "N_Prefix", "N_Op")
  df = addOperators(df, opTable, "P_Telnum", "P_Prefix", "P_Op")
  df = addOperators(df, opTable, "G_Telnum", "G_Prefix", "G_Op")
  df = addOperators(df, opTable, "D_Telnum", "D_Prefix", "D_Op")
  
  return (df)
}