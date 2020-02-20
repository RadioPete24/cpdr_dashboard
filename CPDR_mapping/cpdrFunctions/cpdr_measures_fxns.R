# This file contains functions for obtaining epidemiological measures from California's Parkinsons Disease Registry
# Input files: CPDR_acs_county_2017.rda (census data)
#              cpdr_df_unique (deidentified CA Parkinson's Disease Registry data)
# Output files: temp_census_info (with prevalence and incidence measures)

# Update priorities: Race Ethnicity demographic updates, date formatting updates, AddressCountyNme -> Cities -> census_tract data, filters to apply for deidentification, relative standard error of 30% or more with NCHS applied

#Example of getCensusGrp function:
#sample line for obtaining census info: test_info <- getCensusGrp(df = census_info, age_grp = c("55+"), sex_list = c("M", "MTF"))
getCensusGrp <- function(df = census_info, age_grp, sex_list){
  #Functions to get needed string for census data population refinement
  #require(tidyr)
  #Function that will get the age group string from the census data
  searchYrs <- function(age_grp){
    if(age_grp == "all_ages"){yr_string = ""} else {
    if(age_grp == "0-54"){yr_string = "!!0 to 54 years"} else {
    if(age_grp == "55+"){yr_string = "!!55 years and over"} else {#in cpdr_df_unique, there is no 55+ age group.
    if(age_grp == "55-59"){yr_string = "!!55 to 59 years"} else {
    if(age_grp == "60-64"){yr_string = "!!60 to 64 years"} else {
    if(age_grp == "65-69"){yr_string = "!!65 to 69 years"} else {
    if(age_grp == "70-74"){yr_string = "!!70 to 74 years"} else {
    if(age_grp == "75-79"){yr_string = "!!75 to 79 years"} else {
    if(age_grp == "80-84"){yr_string = "!!80 to 84 years"} else {
    if(age_grp == "85+"){yr_string = "!!85 years and over"}
    else{yr_string = ""}}}}}}}}}}
    return(yr_string)
  } #age_grp options: {#"55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+", "55+"}
  #Function that will return the appropriate sex group from the census data
  searchSex <- function(sex_list){
    if(length(sex_list) ==0){sex_string = ""}
    else{if(any(sex_list %in% c("M", "MTF", "O", "U"))==TRUE & any(sex_list %in% c("F", "FTM"))==TRUE){sex_string = ""} 
    else{if(any(sex_list %in% c("F", "FTM"))==TRUE){sex_string = "!!Female"}
      else{if(any(sex_list %in% c("M", "MTF", "O", "U"))==TRUE){sex_string = "!!Male"}
        # else{return(warning("No sex demographic selected"))}}}
        else{sex_string = ""}}}}
    return(sex_string)
  }
  
  # This function pulls in the census_info data containing the requested demographic (using sex and age group) Pulls in designated Sex, 
  # Other fields to be later incorporated include Race, Ethnicity, Death Registry information, 
  tmp_census_info <- df[(grep(pattern = paste0("^Estimate!!Total", searchSex(sex_list), searchYrs(age_grp), "$"), df$label)),] %>% 
    group_by(NAME) %>% 
    summarise(GEOID = unique(GEOID)
              , variable = unique(variable)
              , estimate = sum(estimate, na.rm = TRUE)
              , moe = sum(moe, na.rm = TRUE)
              , geometry = unique(geometry)
              , label = unique(label)
    )
  tmp_census_info
}

getPrevalence <- function(tmp_df, vital, age_grp, sex_list, filter_id){
  # if(length(sex_list) == 0){return(warning("There is no sex demographic chosen"))} else
  if(age_grp == "55+")    {cpdr_prev_df <- setDT(as.data.table(as.data.frame(table(filter(tmp_df #from cpdr_index
                                                                                      #, Race
                                                                                      #, Ethnicity
                                                                                      , VitalStatus %in% vital
                                                                                      , age >= 55
                                                                                      , Sex %in% sex_list)[c("AddressCountyNme")]
                                                                               )
                                                                         )
                                                           )
                                             )[, .(Freq = sum(Freq)), by = .(Var1)]} else {
  if(age_grp == "all_ages"){cpdr_prev_df <- setDT(as.data.table(as.data.frame(table(filter(tmp_df #from cpdr_index
                                                                                          #, Race
                                                                                          #, Ethnicity
                                                                                          , VitalStatus %in% vital
                                                                                          , Sex %in% sex_list)[c("AddressCountyNme")]
                                                                               )
                                                                        )
                                                         )
                                              )[, .(Freq = sum(Freq)), by = .(Var1)]}
  else                    {cpdr_prev_df <- setDT(as.data.table(as.data.frame(table(filter(tmp_df #is cpdr_index
                                                                                      #, Race
                                                                                      #, Ethnicity
                                                                                      , VitalStatus %in% vital
                                                                                      , ageGrp == age_grp #"55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"
                                                                                      , Sex %in% sex_list)[c("AddressCountyNme")]
                                                                                )
                                                                         )
                                                           )
  )[, .(Freq = sum(Freq)), by = .(Var1)]}}
  # cpdr_prev_df <- return(cpdr_prev_df)
  #After extracting counts from the registry, calculation of the census data
  tmp_census_info <- getCensusGrp(census_info, age_grp = age_grp, sex_list = sex_list) #Race, ethnicity, etc for later factors
  tmp_census_info <- merge(tmp_census_info, cpdr_prev_df, by.x = "NAME", by.y = "Var1", all = TRUE) #tmp_census_info is the specified census table
  
  #Functio either adds estimates between counties then does calculation, or converts to NA  
  ifelse(filter_id == TRUE
         # , tmp_census_info <- filterCensusTbl(tmp_census_info)
         , tmp_census_info <- tmp_census_info
         , tmp_census_info <- tmp_census_info
  )

  tmp_census_info$prevalence_rt <- (as.numeric(tmp_census_info$Freq)*100000)/(as.numeric(tmp_census_info$estimate))
  tmp_census_info$prev_st_err <- as.numeric(tmp_census_info$prevalence_rt)/(as.numeric(tmp_census_info$estimate)^0.5)
  tmp_census_info$prev_rel_sterr <- (tmp_census_info$prev_st_err*100)/tmp_census_info$prevalence_rt
  
  #data.table option:
  # tmp_census_info <- tmp_census_info[, c("prevalence_rt", "prev_st_err", "prev_rel_ster") := list((as.numeric(tmp_census_info$Freq)*100000)/(as.numeric(tmp_census_info$estimate))
  #                                                                                ,as.numeric(tmp_census_info$prevalence_rt)/(as.numeric(tmp_census_info$estimate)^0.5)
  #                                                                                , (tmp_census_info$prev_st_err*100)/tmp_census_info$prevalence_rt
  #                                                                                )
  #                                    ]
  
  tmp_census_info <- as.data.table(tmp_census_info) %>% sf::st_as_sf() #creating as a shapefile
  #tmp_census_info <- as(tmp_census_info, 'Spatial')
  return(tmp_census_info)
}

#get measured rates through age
getPrevalence_age <- function(tmp_df, vital, sex_list, county_list){
  temp_census_info <- df[df$NAME %in% input$countyList,] %>% group_by(NAME) %>% 
    summarise(GEOID = unique(GEOID)
              , variable = unique(variable)
              , estimate = sum(estimate, na.rm = TRUE)
              , moe = sum(moe, na.rm = TRUE)
              , geometry = unique(geometry)
              , label = unique(label)
    )
}


#For functions to operate in other functions/titles
searchYrs_title <- function(age_grp){
  if(age_grp == "all_ages"){yr_string = "All Ages"} else {
  if(age_grp == "0-54"){yr_string = "0 to 54 years"} else {
  if(age_grp == "55+"){yr_string = "55 years and over"} else { #in cpdr_df_unique, there is no 55+ age group.
  if(age_grp == "55-59"){yr_string = "55 to 59 years"} else {
  if(age_grp == "60-64"){yr_string = "60 to 64 years"} else {
  if(age_grp == "65-69"){yr_string = "65 to 69 years"} else {
  if(age_grp == "70-74"){yr_string = "70 to 74 years"} else {
  if(age_grp == "75-79"){yr_string = "75 to 79 years"} else {
  if(age_grp == "80-84"){yr_string = "80 to 84 years"} else {
  if(age_grp == "85+"){yr_string = "85 years and over"}
  else{yr_string = ""}}}}}}}}}}
  return(yr_string)
} #age_grp options: {#"55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+", "55+"}
#Function that will return the appropriate sex group from the census data
searchSex_title <- function(sex_list){
  if(any(sex_list %in% c("M", "MTF", "O", "U"))==TRUE & any(sex_list %in% c("F", "FTM"))==TRUE){sex_string = "population"} 
  else{if(any(sex_list %in% c("F", "FTM"))==TRUE){sex_string = "Female"}
    else{if(any(sex_list %in% c("M", "MTF", "O", "U"))==TRUE){sex_string = "Male"}
      else{sex_string = "No sex demographic selected"}}}
  return(sex_string)
}

#For Incidence, will need to write an algorithm that prioritizes dates chosen for incidence (DateOfDiagnosis > DateCreated > DateReceived > Date Loaded > ?DateConfirmed) while filtering Date of Diagnosis for real dates
#Might need to adjust for dashboard to take into account dynamic adjustment to dateLoaded

getIncidence <- function(tmp_df, vital, age_grp, sex_list, dateDxMin, dateDxMax){
  # if(length(sex_list) == 0){return(warning("There is no sex demographic chosen"))} else
  if(age_grp == "55+")    {cpdr_inc_df <- setDT(as.data.table(as.data.frame(table(filter(tmp_df #from cpdr_index
                                                                                          #, Race
                                                                                          #, Ethnicity
                                                                                         , DateOfDiagnosis >= dateDxMin
                                                                                         , DateOfDiagnosis <= dateDxMax
                                                                                         , VitalStatus %in% vital
                                                                                         , age >= 55
                                                                                         , Sex %in% sex_list)[c("AddressCountyNme")]
                                                                                  )
                                                                            )
                                                              )
                                                )[, .(Freq = sum(Freq)), by = .(Var1)]} else {
    if(age_grp == "all_ages"){cpdr_inc_df <- setDT(as.data.table(as.data.frame(table(filter(tmp_df #from cpdr_index
                                                                                             #, Race
                                                                                             #, Ethnicity
                                                                                            , DateOfDiagnosis >= dateDxMin
                                                                                            , DateOfDiagnosis <= dateDxMax
                                                                                            , VitalStatus %in% vital
                                                                                            , Sex %in% sex_list)[c("AddressCountyNme")]
                                                                                     )
                                                                               )
    )
                                                   )[, .(Freq = sum(Freq)), by = .(Var1)]} else {cpdr_inc_df <- setDT(as.data.table(as.data.frame(table(filter(tmp_df #is cpdr_index
                                                                                            #, Race
                                                                                            #, Ethnicity
                                                                                            , DateOfDiagnosis >= dateDxMin
                                                                                            , DateOfDiagnosis <= dateDxMax
                                                                                            , VitalStatus %in% vital
                                                                                            , ageGrp == age_grp #"55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"
                                                                                            , Sex %in% sex_list)[c("AddressCountyNme")]
                                                                                            )
                                                                                      )
                                                   )
                                                   )[, .(Freq = sum(Freq)), by = .(Var1)]}}
  # cpdr_prev_df <- return(cpdr_prev_df)
  #After extracting counts from the registry, calculation of the census data
  tmp_census_info <- getCensusGrp(census_info, age_grp = age_grp, sex_list = sex_list) #Race, ethnicity, etc for later factors
  tmp_census_info <- merge(tmp_census_info, cpdr_inc_df, by.x = "NAME", by.y = "Var1", all = TRUE) #tmp_census_info is the specified census table
  
  tmp_census_info$incidence_rt <- (as.numeric(tmp_census_info$Freq)*100000)/(as.numeric(tmp_census_info$estimate))
  tmp_census_info$inc_st_err <- as.numeric(tmp_census_info$incidence_rt)/(as.numeric(tmp_census_info$estimate)^0.5)
  tmp_census_info$inc_rel_sterr <- (tmp_census_info$inc_st_err*100)/tmp_census_info$incidence_rt
  
  #data.table option:
  # tmp_census_info <- tmp_census_info[, c("prevalence_rt", "prev_st_err", "prev_rel_ster") := list((as.numeric(tmp_census_info$Freq)*100000)/(as.numeric(tmp_census_info$estimate))
  #                                                                                ,as.numeric(tmp_census_info$prevalence_rt)/(as.numeric(tmp_census_info$estimate)^0.5)
  #                                                                                , (tmp_census_info$prev_st_err*100)/tmp_census_info$prevalence_rt
  #                                                                                )
  #                                    ]
  
  tmp_census_info <- as.data.table(tmp_census_info) %>% sf::st_as_sf() #creating as a shapefile
  #tmp_census_info <- as(tmp_census_info, 'Spatial')
  return(tmp_census_info)
}

# getIncidence <- function(tmp_df, vital, DtDx_lower, DtDx_upper, age_grp, sex_list){
#   new_df <- tmp_df[tmp_df$DateOfDiagnosis <= as.Date(DateDx_upper)
#                    & tmp_df$DateOfDiagnosis <= as.Date(DateDx_lower),]
#   if(age_grp == "55+"){cpdr_inc_df <- setDT(as.data.table(as.data.frame(table(filter(new_df
#                                                                                      #, Race
#                                                                                      #, Ethnicity
#                                                                                      , VitalStatus %in% vital
#                                                                                      , age >= 55
#                                                                                      , Sex %in% sex_list)[c("AddressCountyNme")]
#   )
#   )
#   )
#   )[, .(Freq = sum(Freq)), by = .(Var1)]} 
#   else {cpdr_inc_df <- setDT(as.data.table(as.data.frame(table(filter(new_df
#                                                                       #, Race
#                                                                       #, Ethnicity
#                                                                       , VitalStatus %in% vital
#                                                                       , ageGrp == age_grp #"55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+" (need to filter for any 55+)
#                                                                       , Sex %in% sex_list)[c("AddressCountyNme")])
#   )
#   )
#   )[, .(Freq=sum(Freq)), by = .(Var1)]}
#   tmp_census_info <- getCensusGrp(census_info, age_grp = age_grp, sex_list = sex_list) #Race, ethnicity, etc for later factors
#   tmp_census_info <- merge(tmp_census_info, cpdr_inc_df, by.x = "NAME", by.y = "Var1", all = TRUE) #tmp_census_info is the specified census table
#   tmp_census_info$incidence_rt <- (as.numeric(tmp_census_info$Freq)*100000)/(as.numeric(tmp_census_info$estimate))
#   tmp_census_info$inc_st_err <- as.numeric(tmp_census_info$incidence_rt)/(as.numeric(tmp_census_info$estimate)^0.5)
#   tmp_census_info$inc_rel_sterr <- (tmp_census_info$inc_st_err*100)/tmp_census_info$incidence_rt
#   tmp_census_info <- tmp_df
# }