#_____________________________________________________________________________________
########## "ui.R" file                                                         ####
#            user interface for Shiny server output files and functions               \
#            layout of output files and functions                                     \
#            backend output calculations                                              \
#                                                                                     \
#                                                                                     \  
#_____________________________________________________________________________________\
#
# This is the server logic of a Shiny web application for the California Parkinson's Disease Registry. 
# CPDR_Reporting_v0.01
#
# You can run the application by clicking 'Run App' above.
# This server program logic is maintained by Peter Ching-Tze Ip (Peter.Ip@cdph.ca.gov)
#_____________________________________________________________________________________\ 

n1 <- "California Parkinson's Disease Registry Dashboard v0.1.1"
n2 <- "Description: The California Parkinson's Disease Registry (CPDR) dashboard is designed to automate reporting of Parkinson's Disease data selected by the user that has been collected by the California Department of Public Health"
#

STATE <- "CALIFORNIA"

#With credit to CBD coding
fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ", vec, c(rep("|",tRep),""), collapse="")
}

fC_text <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == '", vec, c(rep("'|",tRep),"'"), collapse="")
}

LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # theme = shinytheme("slate"),
  theme = "bootstrap.css",
  useShinyjs(),
  #Formatting skin color to hexcode color
  tags$head(tags$style(HTML(
    '/* logo */
        .skin-blue .main-header .logo {
            background-color: #0079C2;
            }
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
            background-color: #EB6E1F;
            }
        /* header (rest of the header) */
        .skin-blue .main-header .navbar {
            background-color: #0079C2;
            }
        /* navigation bar color */
        .navbar-default {
            background-color: #5D8AA8;
            color: white;
            }
         '
    ))),
  
  # .navbar-default .navbar-brand { 
  #   color: #262626;
  # }  # This css line does nothing so far
  
  # Application title
  dashboardPage(
    skin = "blue",
    # skin = "black",
    dashboardHeader(title = span(img(src = "CPDR_logo_clear.png", height = 60), "California Parkinson's Disease Registry (CPDR)")
                    , titleWidth = 550 
                    , tags$li(a(strong("About CPDR"),
                                href = "https://www.cdph.ca.gov/Programs/CCDPHP/DCDIC/CDSRB/Pages/California-Parkinson's-Disease-Registry.aspx"
                                , title = ""
                                , target = "_blank")
                              , class = "dropdown")
    ),
    # tabPanel(title = "Home", icon = icon("home"),
    
    # SIDEBAR ------------------------------------------------------------------------
    dashboardSidebar(
      width = 275,
      # div(id = "Patient Registry", #testing to go with sidbar that adjusts with report
      sidebarMenu(id = "tabs", sidebarMenuOutput("menu")),
      # sidebarPanel(
      HTML('<center><img src="CDPH_logo.png" height="65" width="235"></center>'),
      br(),
      
      sidebarMenu(id = 'tab1', menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))),
      
      #Selection of measure being mapped
      selectInput("cpdrMeasure"
                  , label = "Choose the measure of Interest:"
                  , choices = c("Prevalence" = "prev_rt"
                                , "Incidence" = "inc_rt"
                                , "Case Count" = "case_cnt"
                                # , "Crude Death Rate" = "cr_death_rt"
                                # , "Age_adjusted Death Rate" = "aa_death_rt"
                                # , "Hospitalization Rate" = "hosp_rt"
                  )
      ),
      
      #Adding Title for map Plot
      textInput("mapTitle", "Enter Map Plot Title:", "***"),
      
      conditionalPanel(condition = fC(c(3))
                       # , fC_text(c("County Ranking"))
                       , textInput("plotTitle", "Enter Plot Title:", value = "***")),
      
      selectInput("ageGrp" #myAge
                  , label = "Choose the age group of interest:"
                  #, choices = levels(df$ageGrp) #data from cpdr_df_unique
                  , choices = c("All Ages" = "all_ages"
                                , "0-54 years" = "0-54"
                                , "55-59 years" = "55-59"
                                , "60-64 years" = "60-64"
                                , "65-69 years" = "65-69"
                                , "70-74 years" = "70-74"
                                , "75-79 years" = "75-79"
                                , "80-84 years" = "80-84"
                                , "85+ years" = "85+"
                                , "55+ years" = "55+"
                  )
      ),
      
      checkboxGroupInput("sexList" #mySex
                         , label = "Choose the sex groups of interest:", inline = TRUE
                         #, choices = levels(df$Sex) #sex selection from cpdr_df_unique
                         , choiceNames = c("Male", "Male-to-Female", "Female", "Female-to-Male", "Other", "Unknown")
                         , choiceValues = c("M", "MTF", "F", "FTM", "O", "U")
                         , selected = c("M", "MTF", "F", "FTM", "O", "U")
      ),
      
      checkboxGroupInput("vitalStatus" #myVital
                         , label = "Choose Death Record Vital Status:"
                         #, choices = c("")
                         , choiceNames = c("Alive", "Diseased")
                         , choiceValues = c("1", "0")
                         , selected = c("1", "0")
      ),
      
      checkboxGroupInput("documentType"
                         , label = "Choose the Message Format:"
                         , choiceNames = c("HL7", "eICR", "Portal")
                         , choiceValues = c("3", "7", "10")
                         , selected = c("3", "7", "10")
      ),
      
      selectInput("myGEO"
                  , label = h5("Select geographic level of mapping")
                  , choices = c("County", "Community", "Census Tract")
                  , selected = "County"
                  , multiple = FALSE
      ),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      conditionalPanel(condition = fC(c(3, 5))
                       , uiOutput("countyList") #Dynamic innput for counties in data
                       , uiOutput("facilityList") #Dynamic input for facilities in data
      ),
      #May be able to move all inputs to the same conditionalPanel
      # conditionalPanel(condition = fC(c(3, 5))
      #                  , uiOutput("facilityList")
      # ), 
      
      # conditionalPanel(condition = "input.cpdrMeasure == inc_rt"
      #                  , uiOutput("dateDxRange")
      # ), #Dynamic input for dates of Diagnosis range
      
      uiOutput("dateDxRange"),
      
      #For DeIdentification of maps, plots, and tables
      materialSwitch(inputId = "filterID"
                     , label = "Filter Identification"
                     , value = FALSE
                     , status = "primary" #other options include ("info", "success", "warning", "danger")
                     ),
      
      sidebarMenu(id = 'tab2', menuItem(" About the Dashboard", tabName = "tabAbout", icon = icon("info"))),
      
      sidebarMenu(id = 'tab3', menuItem( "Frequently Asked Questions", tabName = "FAQ", icon = icon("question-circle")))
      # ) #Dynamic input for DateLoaded Range to add
    ), 
    
    # MAIN BODY ---------------------------------------------------------------------
    dashboardBody(
      # hr()
      
      # NAVBAR -------------------------------------------------------------------------
      
      navbarPage(title = ""
      # , collapsible = TRUE, collapsed = FALSE,
                 #, inverse = TRUE #need to find CSS way to change format and colore
                 # title = (span("Report"
                 # , style = ".navbar-default{background-color: #0079C2;}")
                 
                 # MAIN BODY ---------------------------------------------------------------------
                 , tabPanel(title = "Patient Registry"
                            , value = "pat_regID"
                            , tabItems(
                              tabItem("dashboard"
                                      , tabsetPanel(type = "tabs"
                                                    # tabPanel("Quick View", fluidRow(plotOutput("distPlot", width = "100%"))
                                                    #          , value = 1)
                                                    , tabPanel("Map (Static)", plotOutput("mapPlot_st", click = "plot_click1", width = 700, height = 700), verbatimTextOutput("mapPlot_stInfo")
                                                               , value = 1)
                                                    , tabPanel("Map (Interactive)", leafletOutput("mapPlot_int", width = 700, height = 700)
                                                               , value = 2)
                                                    , tabPanel("County Ranking", plotOutput("rankPlot_st", height = 800)
                                                               , value = 3)
                                                    # , tabPanel("Test Panel", plotOutput("distPlot", width = "100%", click = "plot1_click")
                                                    #            , value = 4)
                                                    , tabPanel("Summary Table", downloadButton('downloadSummary', 'Download Table', class = "btn-xs btn-info"), div(dataTableOutput("demogrTbl_st"), style = "font-size:80%")
                                                               , value = 4)
                                                    , tabPanel("Census Table", downloadButton('downloadCensus', 'Download Data', class = "btn-xs btn-info"), div(dataTableOutput("censusTbl_int"), style = "font-size:80%")
                                                               , value = 5)
                                                    # , tabPanel("County Report", plotOutput("rankPlot_st", height = 800)
                                                    #            , value = 7)
                                                    #, tabPanel("Facility Report", plotOutput(), value = 6)
                                                    
                                                    , id = "ID")
                              ), #With tabItem
                              
                              tabItem("FAQ"
                                      , fluidRow(
                                        ##### Frequently Asked Questions #####
                                        box(title = h3("Frequently Asked Question")
                                            , width = "100%"
                                            , tags$p(
                                              "Place where people can find answers to all their questions...")
                                            , tags$p(tags$b(
                                              "Q: What is the California Parkinson's Disease Registry (CPDR)?"),
                                              tags$p("A: ")
                                              )
                                            , tags$p(tags$b(
                                              "Q: How secure is the data on the California Parkinson's Disease Dashboard?"),
                                              tags$p("A: The data has strict modeling for web-based applications that CPDR must abide by. Servers, ports, protocols, encryption. deidentification guidlines...")
                                              )
                                            , tags$p(tags$b(
                                              "Q: Where can I learn more about?"),
                                              tags$p("A: ")
                                              )
                                            , tags$p(tags$b(
                                              "Q: How do check to see if my health provider is reporting? How do I report a Parkinson's Disease record?"),
                                              tags$p("A:")
                                              )
                                            , tags$p(tags$b(
                                              "Q: Who do I contact to make suggestions for changes to this dashboard?"),
                                              tags$p("A: https://www.cdph.ca.gov/Programs/CCDPHP/DCDIC/CDSRB/Pages/CPDR-FAQs.aspx")
                                            )
                                            )
                                        )
                                      
                              ),
                              
                              tabItem("tabAbout"
                                      ######About Page #####
                                      # ------------ #
                                      , fluidRow(
                                        # About Application
                                        box(title = h3("About the CPDR Dashboard")
                                            , status = "danger"
                                            # , width = "24 col-lg-6"
                                            , width = "100%"
                                            , tags$p(tags$b("What you will find here")
                                            , tags$p(
                                              "This dashboard is an interactive software tool for rapid analysis of Parkinson's Disease",
                                              "Registry patterns and trends in California. It allows for intuitive reproducible data",
                                              "structuring, visualization, prevalence, and incidence reporting of Parkinson's patients",
                                              "without prior experience."
                                            ))
                                            , tags$p(tags$b("What reports you have access to")
                                            , tags$p(""
                                            ))
                                            , tags$p(tags$b("Where the data comes from")
                                            , tags$p("Out team has collected data from over 517 reporting Health Providers since July 2018 and transformed the data into information that can be viewed in this dashboard. The dashboard reflects data that has been updated on a quarterly basis (but may switch to real-time in staging)."
                                            ))
                                            , tags$p(tags$b("What the technical requirements are to run this dashboard"))
                                            , tags$b(
                                              "Links to relevant cross-linked data:"
                                            )
                                            , tags$p(
                                              class = "text-center"
                                              , tags$a(href = "https://www.census.gov/programs-surveys/acs"
                                                       , h6("American Community Survey (ACS)")
                                                       , target = "_blank"
                                              )
                                              , tags$a(href = "https://censusreporter.org/"
                                                       , h6("ACS Census Reporter")
                                                       , target = "_blank")
                                              , tags$a(href = "https://oshpd.ca.gov/"
                                                       , h6("California's Office of Statewide Health Planning and Development")
                                                       , target = "_blank")
                                              , tags$a(href = "https://healthdata.gov/dataset/community-health-status-indicators-chsi-combat-obesity-heart-disease-and-cancer"
                                                       , h6("Other Links")
                                                       , target = "_blank")
                                              , tags$a(href = "https://healthdata.gov/dataset/community-health-status-indicators-chsi-combat-obesity-heart-disease-and-cancer"
                                                       , h6("Other Links")
                                                       , target = "_blank")
                                            )
                                        )
                                      )
                              )
                            ) #Where tabItem ends
      ) #Where tabPanel ends
      , tabPanel(title = "County Report"
                 , value = "countyRepID"
      )
      , tabPanel(title = "Facility Reports"
                 , value = "facRepID"
                 , tabItem(plotOutput("facTimePlot"
                                       , height = 700
                                       , width = 700))
      )
      , tabPanel(title = "Summary Report"
                 , value = "summaryRepID"
                 , tabPanel("Summary",
                            verbatimTextOutput("summary")
                 )
      )
      ) #Where Navbar ends?
    ) #Where dashboard body ends
  )
)
)

