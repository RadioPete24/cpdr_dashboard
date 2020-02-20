########Description:#######
# Mapping functions for static and interactive mapping
# Input will discern between mapping layers (community, census tract, counties)
# To-do list includes: 1. DocumentType incorporation
# coloration options

########I. Static ggplot mapping:######

cpdrMap_st <- function(myVital = c("0", "1")
                      , mySex = c("M", "MTF", "F", "FTM", "U", "O")
                      , myAge = c("55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
                      , myGeo = "County" #later for census tract with address identification
                      # , myMapTitle = ""
                      , myFilterID = FALSE
                      , myMeasure = "prev_rt"
                      ) {
#if(myMeasure = "prev_rt"){
 temp_censusGrp_info <- getPrevalence(tmp_df = cpdr_index #cpdr_df_unique
                                      , vital = myVital
                                      , age_grp = myAge
                                      , sex_list = mySex
                                      , filter_id = myFilterID
                                      ) 
#} else {if(myMeasure = "inc_rt"){temp_censusGrp_info <- getIncidence(tmp_df = cpdr_index, vital = myVital, age_grp = myAge, sex_list = mySex, dateDxMin >= dateDxRange[1], dateDxMax <= dateDxRange[2] ) }}
#if(myGeo = "County"){
  shape_df <- temp_censusGrp_info 
  #%>% sf::st_as_sf()
  if(myMeasure == "prev_rt"){
  p <- ggplot(data = shape_df) +
    geom_sf(aes(fill = prevalence_rt
                # , prev_st_err
                )) +
    # geom_sf(aes(fill = log1p(prevalence_rt))) + #For log function of prevalence
    # geom_sf_label(aes(label = "prev_st_err")) +
    #ggtitle(myMapTitle) +
    labs(title = paste0("California Prevalence per 100,000 individuals \nAmong ", searchSex_title(mySex), "s of ", searchYrs_title(myAge), sep = "") 
         , x = "longitude"
         , y = "latitude") +
    theme(plot.title = element_text(hjust = 0.5)
          , panel.background = element_blank()) + #for a non-grey grid appearance (change to plot option)
    scale_fill_gradientn(colours=rev(brewer.pal(5, "RdYlBu"))) +
    guides(fill = guide_legend(title = "Prevalence \nRate"))
    # + geom_point() #needs x-y for mouseover with plotly and scales package
    #scale_fill_viridis() + 
    #scale_color_viridis()
  #ggplotly(p, tooltip = "prev_st_err")
  } else {
if(myMeasure == "case_cnt"){  
  p <- ggplot(data = shape_df) +
    geom_sf(aes(fill = log(Freq, 10))) + #alternative is log1p()
    labs(title = paste0("California Unique Patient Reports Recieved by County \nAmong ", searchSex_title(mySex), "s of ", searchYrs_title(myAge), sep = "")
         , x = "longitude"
         , y = "latitude") +
    theme(plot.title = element_text(hjust = 0.5)
          , panel.background = element_blank()) +
    scale_fill_gradientn(colours = rev(brewer.pal(5, "RdYlBu"))) +
    guides(fill = guide_legend(title = "log(Unique Report Counts)"))
  }}
  return(p)
  #}  
}
# if(myGeo = "Census Tract"){
# 
# }

##### II. Interactive mapping #####

cpdrMap_tmap <- function(myVital = c("0", "1")
                         , mySex = c("M", "MTF", "F", "FTM", "U", "O")
                         , myAge = c("55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
                         , myGeo = "County"
                         , myMapTitle = "" #Not activated yet
                         , myMeasure = "prev_rt"
){
  temp_censusGrp_info <- getPrevalence(tmp_df = cpdr_index #cpdr_df_unique
                                       , vital = myVital
                                       , age_grp = myAge
                                       , sex_list = mySex)
  shape_df <- temp_censusGrp_info
  shape_df <- as(shape_df, 'Spatial')
  proj4string(shape_df) <- CRS(proj2)
  # tmap_mode("view") #status is "plot"
  if (myMeasure == "prev_rt"){
  p <- tmap_leaflet(tm_shape(shape_df) + 
                      tm_polygons(col = "prevalence_rt"
                                  , palette = rev(brewer.pal(5, "RdYlBu"))
                                  # , breaks = as.numeric(sort(c(seq(0, max(shape_df@data$prevalence_rt, na.rm = TRUE)
                                  #                                 , by = 200))))
                                  , popup.vars = c("Prvlnce_Rt:" = "prevalence_rt", "St_Error:" = "prev_st_err", "Population" = "estimate")
                                  , alpha = 0.8 #80% transparency that can be adjusted later
                                  , title = myMapTitle
                                  ) 
                    # + tm_fill()
                    
                    # + tm_scale_bar()
                    # + tm_facets(sync = TRUE, ncol = 2)
                    , mode = "view" #"view"
                    , show = TRUE) %>% addScaleBar() 
  # %>% addMarkers(popup = shape_df$prev_st_err, shape_df$prev_rel_sterr)
  } else {if(myMeasure == "case_cnt"){
    p <- tmap_leaflet(tm_shape(shape_df) + 
                        tm_polygons("Freq"
                                    , palette = rev(brewer.pal(5, "RdYlBu"))
                                    # , breaks = as.numeric(sort(c(seq(0, max(shape_df@data$prevalence_rt, na.rm = TRUE)
                                    #                                 , by = 200))))
                                    , style = "log10"
                                    , alpha = 0.8 #80% transparency that can be adjusted later
                                    , title = myMapTitle
                                    ) 
                      # + tm_fill()
                      # + tm_scale_bar()
                      # + tm_facets(sync = TRUE, ncol = 2)
                      , mode = "view" #"view"
                      , show = TRUE) %>% addScaleBar() 
  }}
  return(p)
  #last_map()
}
# 
cpdrMap_int <- function(myVital = c("0", "1")
                        , mySex = c("M", "MTF", "F", "FTM", "U", "O")
                        , myAge = c("55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
                        , myGeo = "County"
                        , myFilterID = FALSE
                        , myMapTitle = input$mapTitle
                        #, myMeasure = "Prevalence
                        ){
  #temp_censusGrp_info <-
  temp_censusGrp_info <- getPrevalence(tmp_df = cpdr_index #cpdr_df_unique
                                       , vital = myVital
                                       , age_grp = myAge
                                       , sex_list = mySex
                                       , filter_df = myFilterID) 
  #}
  #if(myGeo = "County"){
  pal_fun <- rev(brewer.pal(5, "RdYlBu")) #maybe the color pallette for plotting
  shape_df <- as(temp_censusGrp_info, 'Spatial')
  proj4string(shape_df) <- CRS(proj2)
  #}
  p <- leaflet(data = shape_df) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(stroke = FALSE # remove polygon borders
                # , fillColor = ~pal_fun("prevalence_rt") # set fill color with function from above and value
                , fillColor = rev(brewer.pal(5, "RdYlBu"))
                , fillOpacity = 0.8
                , smoothFactor = 0.5
                # make it nicer #, fillColor = ~colorQuantile("Yl0rRd")
                  # popup = p_popup
                ) %>%
    addLegend(position = "bottomright"
              , pal = pal_fun #palette function
              , values = levels(shape_df@data$prevalence_rt) #values to be passed to the palette function
              , title = myMapTitle) #legend title
  return(p)
}




# shape_df <- shape_df %>% 
#   sf::st_as_sf()

# p <- ggplot(california.df) + 
#   geom_polygon(aes(x = long, lat, group = group, fill = prvlnc_)) + 
#   labs(title = "California Prevalence per 1x10^5"
#        , x = "longitude"
#        , y = "latitude") +
#   scale_fill_gradientn(colours=rev(brewer.pal(3, "RdYlBu")))
# # scale_fill_gradient(low = "black", high = "blue")

#ouput Frequency Plot

