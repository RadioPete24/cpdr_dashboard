#_____________________________________________________________________________________
########## "deidentify_fxn.R" file                                                ####
#            backend calculations for de-identifying data in cpdr tables              \
#           documentation found from DHCS website DDG:                                \
#           www.dhcs.ca.gov/dataandstats/Documents/DHCS-DDG-V2.0-120116.pdf           \  
#          -  Assesses 'quasi-identifiers' to determine if certain data can be        \
#           reported.                                                                 \  
#           Input: Census table, merged with demographics of measure table            \
#           Output: filtered population for legal and Public Affairs review           \  
#_____________________________________________________________________________________\
#
# This is the server logic of a Shiny web application for the California Parkinson's Disease Registry. 
# CPDR_Reporting_v0.01
#
# You can run the application by clicking 'Run App' above.
# This server program logic is maintained by Peter Ching-Tze Ip (Peter.Ip@cdph.ca.gov)
#_____________________________________________________________________________________\ 

#function deidentifies data from method 2 (conversion of low counties to NA)
filterCensusTbl <- function(tmp_df){
  #1. P Identifiers in data? Presumably already removed in appended census data table
  
  #2. Assessment of data for small numerators or denominators:
  #Provider data
  
  #Patient Data Criteria
  #numerators fewer than 11 individuals? denominators less than 20,000 individuals?
  #counties that do meet threshold for reporting:
  tmp_df[!(tmp_df$Freq < 11 | tmp_df$estimate < 20000),]
  
  #counties that do not meet threshold for reporting
  tmp_df[tmp_df$Freq < 11 | tmp_df$estimate < 20000,]$Freq
  
  tmp_df[which(tmp_df$Freq < 11 & tmp_df$estimate >= 20000),]$Freq #useNA = "always" in table creations
  
  tmp_df[which(tmp_df$Freq < 11 & tmp_df$estimate < 20000),]$Freq
  #3. Method used for deidentification of data.
  #Solution 1 is to convert all values not meeting threshold criteria to NA
  
  #Solution 2 is to combine numbers with other counties
  #Counties with small numbers would be bound with a predetermined county. Counties would reflect sum and measures of both counties...
  
  
  
  #Final Steps are legal review and program mgmt and Office of Public Affairs
}

#This function is to remove Personal Identifiers from deduplicated cpdr index data and prepare data for server uploads
deIdentify_data <- function(){
  
}