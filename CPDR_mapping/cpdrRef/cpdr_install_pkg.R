cdph_pkg_list <- c("data.table"
                   #, "plotly"
                   , "DT"
                   , "dplyr"
                   , "lubridate"
                   , "ggplot2"
                   , "xlsx"
                   , "XLConnect"
                   , "foreign"
                   , "openxlsx"
                   , "readxl"
                   , "Rcpp"
                   , "writexl"
                   , "WriteXLS"
                   , "scales"
                   , "Rcapture"
                   , "RColorBrewer"
                   , "Rcapture"
                   , "stringr"
                   , "multiplex"
                   , "utils"
                   , "tibble"
                   , "tidyr"
                   , "tidyverse"
                   , "tidyxl"
                   , "sas7bdat"
                   , "haven"
                   , "tibble"
                   , "devtools"
                   , "utils"
)
cdph_pkg_list_new <- cdph_pkg_list[!(cdph_pkg_list %in% installed.packages()[,"Package"])]
if(length(cdph_pkg_list_new)>0) {
  install.packages(cdph_pkg_list_new, dependencies = TRUE)
}

lapply(cdph_pkg_list, require, character.only = TRUE)