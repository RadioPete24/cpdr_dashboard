#_____________________________________________________________________________________
##### "global.R" file                                                            ######
#            designate folder locations                                               |
#            load packages                                                            |
#            read in shape and data files                                             |
#            creates sub-site for "San Joaquin Public Health Consortium"              |
#            load functions                                                           |
#            read key "info" files                                                    |
#            creates vectors and contants used for Shiny app                          |
#                                                                                     |   
#_____________________________________________________________________________________

#-- Set Locations Etc----------------------------------------------------------------------
myCPDRLoc <- getwd()   # for Shiny.io... /home/cdphintra.ca.gov/pip/CPDR_reporter
myRef       <- paste0(myCPDRLoc, "/cpdrRef") #/CPDR_mapping
myFunctions <- paste0(myCPDRLoc, "/cpdrFunctions")
myData      <- paste0(myCPDRLoc, "/cpdrData")

#-- Load Packages --------------------------------------------------------------------------

source(file.path(myRef, "/cpdr_install_pkg.R"))
source(file.path(myRef, "/gis_install_pkg.R"))
source(file.path(myRef, "/shiny_install_pkg.R"))

#-- Run Application ------------------------------------------------------------------------

# myFunction_list <- c("tsvFile.R", "CRmap_00.R", "dateRange.R")
# sapply(file.path(paste0(myFunctions, myFunction_list)), FUN = function(x){ifelse (file.exists(x), source(x), warning("file does not exists"))}, .GlobalEnv)

#source(file.path(myFunctions, "tsvFile.R")) #for uploading data
#source(file.path(myFunctions, "CPDRmap_00.R")) # Zoom Map
source(file.path(myFunctions, "/cpdr_measures_fxns.R"))
source(file.path(myFunctions, "/cpdr_plots_fxns.R"))
source(file.path(myFunctions, "/cpdr_mapping_fxns.R")) # Static Map
source(file.path(myFunctions, "/cpdr_table_fxns.R")) #Census Tables
# source(file.path(myFunctions, "cpdr_deidentify_fxns.R"))

#source(file.path(myFunctions, "dateRange.R"))
# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# --- Load CPDR Data --------------------------------------------------------------------------------------

load(file.path(myData, "/CPDR_acs_county_2017.rda")) #opens census_info
#load(file.path(myData, "CPDR_acs_comm_2017.rda")) #for census_comm_info
#load(file.path(myData, "CPDR_acs_tract_2017.rda")) #for census_tract_info
load(file.path(myData, "/cpdr_shiny_20200127.Rdata")) #for cpdr_registry level info
# --- CPDR Key Inputs --------------------------------------------------------------------------------------

#Shapefile data for county, community, census...now pulled from acs census data
#acs_County       <- load(paste0(myPlace,"/myData/CPDR_acs_county_2017.rda")) 
# shape_Comm         <-readOGR(paste0(myPlace,"/myData/shape_Comm.shp"))   # Read Shape Files
# shape_Tract        <- readOGR(paste0(myPlace,"/myData/shape_Tract.shp"))  

# --- Shiny constants --------------------------------------------------------------------------------------

#Other measures to include - unique counts, 
cpdrMeasureNames <- c(
  "Prevalence"
  , "Incidence"
  , "Death Rate"
  , "Hospital Rate"
)

# Colors from CDPH logo
BLUE <- "#0079C2"
GREEN <- "#5C8727"
ORANGE <- "#EB6E1F"
BEIGE <- "#F9F5F1"

# --- END --------------------------------------------------------------------------------------------------