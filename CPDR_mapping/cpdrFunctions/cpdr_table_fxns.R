# =====================================================================================
# "cpdr_table_fxns.R" file                                                            |
#           Generates tables or summary tables of cpdr data                           |
#           Generates tables of census data and cpdr measures data                    |
#                                                                                     |
#                                                                                     |   
# =====================================================================================
#tmp_df is dataframe for disease registry used
#tbl_list is the field name list for variables of interest from the disease registry to be examined (maybe deprecate)

#getSummaryTbl(tmp_df = cpdr_index, tbl_list = list("Sex", "Race", "Ethnicity", "ageGrp", "VitalStatus", "DocumentTypeNme"))
getSummaryTbl <- function(tmp_df = NULL, tbl_list = tbl_list){
  tbl_list <- list("Sex", "Race", "Ethnicity", "ageGrp", "VitalStatus", "DocumentTypeNme")
  #sapply(tbl_list, FUN = summary)
  #For calculating age within function if not already calculated
  # length(initdata_tmp[which(nchar(as.character(initdata_tmp$DateOfBirth))==6),]$DateOfBirth)
  # tmp_df[which(nchar(as.character(tmp_df$DateOfBirth))==6),]$DateOfBirth <- paste(tmp_df[which(nchar(as.character(tmp_df$DateOfBirth))==6),]$DateOfBirth, "01", sep = "")
  # tmp_df$DateOfBirth <- as.POSIXlt.character(as.character(tmp_df$DateOfBirth), format = "%Y-%m-%d", tz = "Etc/GMT+8")
  # tmp_df$age <- year(as.Date(Sys.Date(), format = "%Y-%m-%d")) - year(tmp_df$DateOfBirth)
  summaryTbl <- lst()
  for(i in 1:length(tbl_list)){
    tmp_summaryTbl <- cbind(as.data.frame(table(tmp_df[as.character(tbl_list[[i]])], useNA = "ifany"))
                            , as.data.frame(round(prop.table(table(tmp_df[as.character(tbl_list[[i]])], useNA = "ifany"))*100, 2))[2])
    tmp_summaryTbl$Var1 <- as.data.frame(table(tmp_df[as.character(tbl_list[[i]])], useNA = "ifany"))[,1]
    colnames(tmp_summaryTbl) <- c("Var1", "Freq", "Percent")
    summaryTbl[[i]] <- rbind(data.frame(Var1 = as.character(tbl_list[[i]])
                                        , Freq = NA
                                        , Percent = NA)
                             , tmp_summaryTbl)
  }
  summaryTbl <- do.call(rbind, summaryTbl)
  colnames(summaryTbl)[colnames(summaryTbl)=="Var1"] <- "Characteristic"
  colnames(summaryTbl)[colnames(summaryTbl)=="Freq"] <- "Count"
  summaryTbl <- as.data.frame(summaryTbl, stringAsFactors = TRUE)
  # summaryTbl <- as.data.frame(summaryTbl)
  summaryTbl[,2:3] <- sapply(summaryTbl[,2:3], as.character)
  # summaryTbl[summaryTbl$Characteristic %in% NA,]$Characteristic <- "Not Applicable"
  # summaryTbl <- as.data.table(summaryTbl)
  str(summaryTbl)
  return(summaryTbl)
}

#xlsx_formats from tidyxl package
#Opening up xlsx file
writeSummaryTbl <- function(summaryTbl = summaryTbl, fileOutput = file.path(getwd())){
  x <- tidyxl::xlsx_cells(path = file.path(fileOutput, paste0("cpdr_summaryTbls", format(Sys.Date(), format = "%Y%m%d"), ".xlsx")), sheets = 'cpdr_summaryTbl20190802')
  #x <- tidyxl::xlsx_cells(path = file.path("D:", "Peter", "Data", "CPDR_registry_summary", "cpdr_summaryTbls20190802.xlsx"), sheets = 'cpdr_summaryTbl20190802')x <- tidyxl::xlsx_cells(path = file.path("D:", "Peter", "Data", "CPDR_registry_summary", "cpdr_summaryTbls20190802.xlsx"), sheets = 'cpdr_summaryTbl20190802')
  dplyr::glimpse(x)
  formats <- xlsx_formats(x)
  formats$local$font$bold
}

#need to give cutoff from deadline for calculation. Check DateConfirmed, may need to move to plot functions, use mean(column, na.rm = TRUE)
# getTimelinessTbl <- function(tmp_df = NULL, rpt_fclty = NULL){
#   filter(tmp_df, ReportingFacilityNme %in% rpt_fclty)
#   difftime(tmp_df$DateLoaded, tmp_df$DateOfDiagnosis, units = "days")
# }