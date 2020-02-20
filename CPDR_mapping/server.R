#_____________________________________________________________________________________
########## "server.R" file                                                         ####
#            designate folder locations                                               \
#            read in shape and data files                                             \
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
# options(shiny.maxRequestSize=10*1024^2)

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
#options(shiny.maxRequestSize=10*1024^2)
##### General used functions in server output #####
    #For collapsible navigation bar, leaving all but icons... need to push conditional inputs into 'dashboard' sidebar Menu.
    runjs({'
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        var clicker = document.querySelector(".sidebar-toggle");
        clicker.id = "switchState";
    '})
    
    onclick('switchState', runjs({'
        var title = document.querySelector(".logo")
        if (title.style.visibility == "hidden") {
          title.style.visibility = "visible";
        } else {
          title.style.visibility = "hidden";
        }
  '}))
    
    cpdr_index_selected <- reactive({
        data.frame(dplyr::filter(cpdr_index
                                 , Sex             %in% input$sexList
                                 , ageGrp          == input$ageGrp
                                 , VitalStatus     %in% input$vitalStatus
                                 , DocumentTypeID %in% input$documentType
                                 #, Race            %in% input$raceList
                                 #, Ethnicity       %in% input$ethnList
        )
        )
    })
    
    censusPrev_df <- reactive({
        SHP <- as(getPrevalence(tmp_df = cpdr_index
                                       , vital = input$vitalStatus
                                       , age_grp = input$ageGrp
                                       , sex_list = input$sexList
                                       , filter_id = input$filterID
                                       #, DocumentTypeNme %in% input$documentType
                                       #, Race            %in% input$raceList
                                       #, Ethnicity       %in% input$ethnList
                                       ), 'Spatial')
        proj4string(SHP) <- CRS(proj2)
        return(SHP)
    })
    
    #Not complete yet
    censusInc_df <- reactive({
        SHP <- as(getIncidence(tmp_df = cpdr_index
                               , vital = input$vitalStatus
                               , age_grp = input$ageGrp
                               , sex_list = input$sexList
                               , dateDxMin = output$dateDxRange
                               , dateDxMax = output$dateDxRange
                               ), 'Spatial')
        proj4string(SHP) <- CRS(proj2)
        return(SHP)
    })
    
#####I. Update tab values after each modification
    # current <- reactiveValues()
    # observe({current$nav = input$navsID
    # current$tab <- switch(current$nav
    #                       , "Maps_st" = input$maps_st
    #                       , "Maps_int" = input$maps_int
    #                       , "County_plots" = input$County_Plots
    #                       , "SummaryTbl" = input$summaryTbl
    #                       , "CensusTbl" = input$censusTbl
    #                       , current$nav)
    # updateTabItems(session, "plotsMenuItems", "tabInputs")
    #     
    # })
    
#####I. Mapping Outputs #####
    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # })
    
    output$summary <- renderPrint({
        summary(cpdr_index_selected())
        
        })
    # output$cpdr_tbl <- DT::renderDataTable(DT::datatable({
    #     dataNow <- getPrevalence(tmp_df = cpdr_df_unique, vital = input$Vital, age_grp = input$ageGrp, sex_list = input$sex_list)
    # }))

    # output$ageGrp <- renderUI({
    #     tmp_df <- as.data.frame(data_now())
    #     getAgeGrp <- unique(unlist(tmp_df$ageGrp))[sort.list(unique(unlist(tmp_df$ageGrp)))]
    #     # getAgeGrp <- str_sort(unique(unlist(tmp_df$ageGrp)), numeric = TRUE) #requires stringr package
    #     checkboxGroupInput(inputId = "ageGrp", label = h5("Age Group (years)")
    #                        , choiceNames = getAgeGrp
    #                        , choiceValues = getAgeGrp
    #                        , inline = TRUE
    #                        , selected = getAgeGrp)
    # })
    
    # real time updating for upper and lower date limits of cpdr_df_unique
    # output$dateRange <- renderUI({
    #     tmp_df <- as.data.frame(data_now())
    #     # Function is meant to read only the initial parameters and stay the same after selections so built without continuous rendering
    #     dateMin <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[1]
    #     dateMax <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[2]
    #     sliderInput("dateRange", label = h5("Select Date Range:")
    #                 , min = dateMin
    #                 , max = dateMax
    #                 , value = c(dateMin, dateMax)
    #                 , step = 12
    #                 , animate = TRUE
    #     )
    # })
    output$mapPlot_int <- renderLeaflet({
        cpdrMap_tmap(myVital  = c(input$vitalStatus)
                   , mySex    = c(input$sexList)
                   , myAge    = c(input$ageGrp)
                   , myGeo      = "County"
                   , myMapTitle = c(input$mapTitle)
                   , myMeasure  = c(input$cpdrMeasure)
                   )
    })
    
    output$mapPlot_st <- renderPlot({
        cpdrMap_st(myVital = c(input$vitalStatus)
                         , mySex = c(input$sexList)
                         , myAge = c(input$ageGrp)
                         , myGeo = "County"
                         # , myMapTitle = input$mapTitle
                         , myMeasure = c(input$cpdrMeasure)
        )
    })
    
    output$mapPlot_stInfo <- renderText({
        paste0("x=", input$plot_click1$x, "\ny=", input$plot_click1$y)
    })

#####II. Plot Outputs #####   
    output$rankPlot_st <- renderPlot({
        shape_df <- getPrevalence(cpdr_index
                                  , vital     = input$vitalStatus
                                  , age_grp   = input$ageGrp
                                  , sex_list  = input$sexList
                                  # , doc_type = input$documentType
                                  , filter_id = input$filterID
                                  )
        shape_df <- as(shape_df, 'Spatial')
        countyRank_plot(tmp_df       = shape_df
                        , plot_title = c(input$plotTitle)
                        , myMeasure  = c(input$cpdrMeasure)
                        )
    })
    
    # output$facTimePlot <- renderPlot({
    #     facilityTime_plot(tmp_df = cpdr_index
    #                       , facility_list
    #                       , plot_title = input$plotTitle)
    # })
    
    # new_tbl <- reactive({as.data.frame(getPrevalence(tmp_df = cpdr_index
    #                                   , vital = input$vitalStatus
    #                                   , age_grp = input$ageGrp
    #                                   , sex_list = input$sexList)
    #                                   })
#####III. Table Outputs #####                   
    output$censusTbl_int <- renderDataTable({
        shape_df <- getPrevalence(tmp_df           = cpdr_index
                                  , vital          = input$vitalStatus
                                  , age_grp        = input$ageGrp
                                  , sex_list       = input$sexList
                                  , filter_id      = input$filterID
                                  #, DocumentTypeID %in% input$documentType
                                  #, Race            %in% input$raceList
                                  #, Ethnicity       %in% input$ethnList
                                  )
        as.data.frame(shape_df)
    }
    )
    
    output$demogrTbl_st <- renderDataTable({
        #getSummaryTbl(cpdr_index)
        if(input$ageGrp == "55+"){tmpTbl <- getSummaryTbl(filter(cpdr_index
                             , Sex             %in% input$sexList
                             , age             >= 55
                             , VitalStatus     %in% input$vitalStatus
                             , DocumentTypeID  %in% input$documentType
                             #, Race            %in% input$raceList
                             #, Ethnicity       %in% input$ethnList
                             ))} 
        else {if (input$ageGrp == "all_ages"){tmpTbl <- getSummaryTbl(filter(cpdr_index
                             , Sex             %in% input$sexList
                             , VitalStatus     %in% input$vitalStatus
                             , DocumentTypeID  %in% input$documentType
                             #, Race            %in% input$raceList
                             #, Ethnicity       %in% input$ethnList
                             ))} 
        else {tmpTbl <- getSummaryTbl(filter(cpdr_index
                             , Sex             %in% input$sexList
                             , ageGrp          == input$ageGrp
                             , VitalStatus     %in% input$vitalStatus
                             , DocumentTypeID  %in% input$documentType
                             #, Race            %in% input$raceList
                             #, Ethnicity       %in% input$ethnList
        ))}}
        return(datatable(tmpTbl, options = list(pageLength = 50)))
    }
    )
    
    # if(input$ageGrp == "55+"){
    #     output$demogrTbl_int <- renderTable({
    #         getSummaryTbl(tmp_df = filter(cpdr_index
    #                                       , Sex             %in% input$sexList
    #                                       , age          >= 55
    #                                       , VitalStatus     %in% input$vitalStatus
    #                                       #, DocumentTypeNme %in% input$documentType
    #                                       #, Race            %in% input$raceList
    #                                       #, Ethnicity       %in% input$ethnList
    #         )
    #         )
    # })} else {}
    
    output$demogrTbl_int <- renderDataTable({
        cpdr_index_selected()
        
    })
    
    # output$my_tmap = renderLeaflet({
    #     tm <- tm_shape(World) + tm_polygons("HPI", legend.title = "Happy Planet Index")
    #     tmap_leaflet(tm)
    # })

#####IV. Dynamic Input bars reading from data to be inserted into the ui.R    #####
    output$downloadSummary <- downloadHandler(
        filename <- function(){
            normalizePath(file.path('.', paste('summaryTbl_info_', format(Sys.Date(), "%Y%m%d"), '.csv', sep = "")))
        },
        content <- function(file){
            
        }
    )
   
    output$downloadCensus <- downloadHandler(
        filename <- function(){
            paste('censusPrev_info_', format(Sys.Date(), "%Y%m%d"), '.zip', sep = "")
            },
        content <- function (file){
            if(length(Sys.glob(paste("censusPrev_info_", format(Sys.Date(), "%Y%m%d"), '.zip', sep = "")))>0){
                file.remove(Sys.glob(paste("censusPrev_info_", format(Sys.Date(), "%Y%m%d"), '.zip', sep = "")))
            }
            writeOGR(censusPrev_df()
                     , dsn = file
                     , layer = paste('censusPrev_', format(Sys.Date(), "%Y%m%d"))
                     , driver = "ESRI Shapefile")
            zip(zipfile = paste('censusPrev_', format(Sys.Date(), "%Y%m%d"), ".zip")
                , files = Sys.glob(paste('censusPrev_', format(Sys.Date(), "%Y%m%d"), ".*", sep = ""))
                , zip = Sys.getenv("R_ZIPCMD", "zip"))
            file.copy(paste('censusPrev_', format(Sys.Date(), "%Y%m%d"), ".zip"), file)
            if(length(Sys.glob(paste('censusPrev_', format(Sys.Date(), "%Y%m%d"), ".*", sep = "")))>0){
                file.remove(Sys.glob(paste('censusPrev_', format(Sys.Date(), "%Y%m%d"), ".*", sep = "")))
            }
            }
        )
    
    output$countyList <- renderUI({
        selectInput("countyList"
            , label = "Choose the county of interest:"
            #, choices = levels(df$ageGrp) #data from cpdr_df_unique
            , choices = c(levels(as.factor(cpdr_index$AddressCountyNme)))
        )
    })
    
    output$facilityList <- renderUI({
        selectInput("facilityList"
                    , label = "Choose the facility of interest:"
                    #, choices = levels(df$ageGrp) #data from cpdr_df_unique
                    , choices = c(levels(cpdr_index$ReportingFacilityName))
        )
    })
    
    output$dateDxRange <- renderUI({
        dateDxMin <- range(as.Date(cpdr_index$DateOfDiagnosis, format = "%m/%d/%Y"), na.rm = TRUE)[1]
        dateDxMax <- range(as.Date(cpdr_index$DateOfDiagnosis, format = "%m/%d/%Y"), na.rm = TRUE)[2]
        sliderInput("dateRange"
                    , label = "Select range of dates of Diagnosis"
                    , min = dateDxMin
                    , max = dateDxMax
                    , value = c(dateDxMin, dateDxMax)
                    , step = 12
                    #,animate = TRUE 
        )
    })
    
    output$dateLdRange <- renderUI({
        dateLdMin <- range(as.Date(cpdr_index$DateLoaded, format = "%m/%d/%Y"), na.rm = TRUE)[1]
        dateLdMax <- range(as.Date(cpdr_index$DateLoaded, format = "%m/%d/%Y"), na.rm = TRUE)[2]
        sliderInput("dateRange"
                    , label = "Select range of dates Loaded"
                    , min = dateLdMin
                    , max = dateLdMax
                    , value = c(dateLdMin, dateLdMax)
                    , step = 12
                    #,animate = TRUE 
        )
    })

    #####V. URL links#####    
    url <- a("CPDR Webpage", href="https://www.cdph.ca.gov/Programs/CCDPHP/DCDIC/CDSRB/Pages/California-Parkinson's-Disease-Registry.aspx")
    output$LINKS <- renderUI({
        tagList("URL link:", url)
    })
        
    
    })
