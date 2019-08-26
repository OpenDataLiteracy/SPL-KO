library(shiny)
#To get census data via API
library(tidycensus)
#For Mapping data
library(tidyverse)
library(leaflet)
library(sf)
library(leafem)
library(raster)
library(htmlwidgets)

ui <- fluidPage(titlePanel("City of Seattle Census Data"), htmlOutput("intro"), htmlOutput("instructions"),
                tabsetPanel(
                  tabPanel(title= "Age Map", htmlOutput("title1"), leafletOutput("agemap", width = "100%", height = 600)),
                  tabPanel(title= "Income Map", htmlOutput("title2"), leafletOutput("incomemap", width = "100%", height = 600)),
                  tabPanel(title= "Language Map", htmlOutput("title3"), leafletOutput("langmap", width = "100%", height = 600)),
                  tabPanel(title= "School Map", htmlOutput("title4"), leafletOutput("schoolmap", width = "100%", height = 600)),
                  tabPanel(title= "Help", htmlOutput("helpintro"),img(src='help.png', width="100%", height = "100%"))
                ), htmlOutput("branchlegend"),
                img(src='dwn.png'), img(src='mce.png'), img(src='mcw.png'),img(src='ner.png'),img(src='nwr.png'),img(src='ser.png'),img(src='swr.png'), htmlOutput("funding")
                )

wa_age_est <- readRDS("wa_age_est_app.rds")
age_median_1 <- readRDS("age_median_app.rds")
wa_income <- readRDS("wa_income_app.rds")
wa_lang_est <- readRDS("wa_lang_est_app.rds")
wa_school_est <- readRDS("wa_school_app.rds")
branches <- read.csv("branches_app.csv")
getColor <- function(region) {
  sapply(branches$region, function(region) {
    if(region == "DWN") {
      "red"
    } else if(region == "MCE") {
      "orange"
    } else if(region == "MCW") {
      "lightgray"
    } else if(region == "NER") {
      "darkblue"
    } else if(region == "NWR") {
      "lightgreen"
    } else if(region == "SER") {
      "pink"
    } else if(region == "SWR") {
      "darkpurple"
    } else {
      "lightred"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(branches)
)
pal <- colorNumeric(palette = "viridis", domain = age_median_1$estimate)
pal6 <- colorNumeric(palette = "viridis", domain = c(0,100), n = 10)
pal_income <- colorNumeric(palette = "viridis", domain = wa_income$estimate, n = 10)
pal_lang <- colorNumeric(palette = "viridis", domain = c(0,100), n = 10)
pal_school <- colorBin(palette = "viridis", domain = c(-100,100), bins=3, pretty = FALSE)

server <- function(input, output, session) {

  output$branchlegend <- renderUI({
    HTML('<b> SPL Branch Markers:</b> <br>')
  })
  
  output$intro <- renderUI({
    HTML('Welcome! <br> Below you will find four interactive maps of the city of Seattle with data from the U.S. Census Bureau. 
         These maps are intended for use by Seattle Public Library staff to better understand the communities around each library branch. 
          Data is based on 2017 <a href="https://www.census.gov/programs-surveys/acs">American Community Survey</a> estimates.')
  })
  output$instructions <- renderUI({
    HTML("<br> Click on a census tract or branch location below to see statistics for each area."
    )})
  
  output$funding <- renderUI({
    HTML('<br> This data dashboard was created for SPL as a part of the <a href="https://odl.ischool.uw.edu/">Open Data Literacy project</a>. 
         ODL is funded by a grant from the Institute of Museum and Library Services, Laura Bush 21st Century Librarians Program. Grant number 67-5285.<br>'
    )})
  
  output$title1 <- renderUI({
    HTML('<h3>Median Age and General Age Range Breakdown</h3>'
    )})
  
  output$title2 <- renderUI({
    HTML('<h3>Median Household Income</h3>'
    )})
  
  output$title3 <- renderUI({
    HTML('<h3>Language Spoken at Home</h3>'
    )})
  
  output$title4 <- renderUI({
    HTML('<h3>School Enrollment by Level of School (Private versus Public)</h3>'
    )})
  
  output$helpintro <- renderUI({
    HTML('Each map covers a different topic (age, income, languages spoken at home, and school enrollment type).
         The colored polygons on the map can be toggled to represent different groups of the population. <br>'
      
    )})
  
  
  output$agemap <- renderLeaflet({
    agemap <- leaflet() %>% 
      #Base Map
      addProviderTiles("CartoDB.Positron") %>% 
      #Center view on the City of Seattle
      setView(lat = 47.6062, lng = -122.3321, zoom = 11) %>%
      #Add the median age colored polygon layer (name layer with group attribute)
      addPolygons(data=age_median_1, popup = paste("<i> Tract ", str_extract(age_median_1$NAME, "[0-9]+"),"</i> <br>",
                                                   "<b>Median Age:</b>","<br>",
                                                   as.character(age_median_1$estimate), "yrs", "\u00B1", as.character(age_median_1$moe), "yrs", "<br>",
                                                   "<b>Age Breakdown:</b> <br>",
                                                   "Under 18: ", round(wa_age_est$under18_per,digits = 2) , "% <br>",
                                                   "18-39: ", round(wa_age_est$y1839_per,digits = 2), "% <br>",
                                                   "40-64: ", round(wa_age_est$y4063_per,digits = 2), "% <br>",
                                                   "Over 65: ", round(wa_age_est$over65_per,digits = 2), "%"),
                  #Define borders around polygons
                  stroke = TRUE,
                  weight = 1,
                  #Keep accurate polygon shapes when zooming
                  smoothFactor = 0,
                  #define colors
                  fillOpacity = 0.7,
                  color = ~ pal(estimate), group = "Median Age") %>%
      #add layer for under 18
      addPolygons(data=wa_age_est, popup = paste("<i> Tract ", str_extract(age_median_1$NAME, "[0-9]+"),"</i> <br>",
                                                 "<b>Median Age:</b>","<br>",
                                                 as.character(age_median_1$estimate), "yrs", "\u00B1", as.character(age_median_1$moe), "yrs", "<br>",
                                                 "<b>Age Breakdown:</b> <br>",
                                                 "<mark> Under 18: </mark>", round(wa_age_est$under18_per,digits = 2) , "% <br>",
                                                 "18-39: ", round(wa_age_est$y1839_per,digits = 2), "% <br>",
                                                 "40-64: ", round(wa_age_est$y4063_per,digits = 2), "% <br>",
                                                 "Over 65: ", round(wa_age_est$over65_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.6,
                  color = ~ pal6(under18_per), group = "under 18") %>%
      #add layer for 18-39 year
      addPolygons(data=wa_age_est, popup = paste("<i> Tract ", str_extract(age_median_1$NAME, "[0-9]+"),"</i> <br>",
                                                 "<b>Median Age:</b>","<br>",
                                                 as.character(age_median_1$estimate), "yrs", "\u00B1", as.character(age_median_1$moe), "yrs", "<br>",
                                                 "<b>Age Breakdown:</b> <br>",
                                                 "Under 18: ", round(wa_age_est$under18_per,digits = 2) , "% <br>",
                                                 "<mark> 18-39: </mark>", round(wa_age_est$y1839_per,digits = 2), "% <br>",
                                                 "40-64: ", round(wa_age_est$y4063_per,digits = 2), "% <br>",
                                                 "Over 65: ", round(wa_age_est$over65_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.6,
                  color = ~ pal6(y1839_per), group = "18-39") %>%
      #add layer for 40-64 years
      addPolygons(data=wa_age_est, popup = paste("<i> Tract ", str_extract(age_median_1$NAME, "[0-9]+"),"</i> <br>",
                                                 "<b>Median Age:</b>","<br>",
                                                 as.character(age_median_1$estimate), "yrs", "\u00B1", as.character(age_median_1$moe), "yrs", "<br>",
                                                 "<b>Age Breakdown:</b> <br>",
                                                 "Under 18: ", round(wa_age_est$under18_per,digits = 2) , "% <br>",
                                                 "18-39: ", round(wa_age_est$y1839_per,digits = 2), "% <br>",
                                                 "<mark> 40-64: </mark>", round(wa_age_est$y4063_per,digits = 2), "% <br>",
                                                 "Over 65: ", round(wa_age_est$over65_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.6,
                  color = ~ pal6(y4063_per), group = "40-64") %>%
      #add layer for over 65 years
      addPolygons(data=wa_age_est, popup = paste("<i> Tract ", str_extract(age_median_1$NAME, "[0-9]+"),"</i> <br>",
                                                 "<b>Median Age:</b>","<br>",
                                                 as.character(age_median_1$estimate), "yrs", "\u00B1", as.character(age_median_1$moe), "yrs", "<br>",
                                                 "<b>Age Breakdown:</b> <br>",
                                                 "Under 18: ", round(wa_age_est$under18_per,digits = 2) , "% <br>",
                                                 "18-39: ", round(wa_age_est$y1839_per,digits = 2), "% <br>",
                                                 "40-64: ", round(wa_age_est$y4063_per,digits = 2), "% <br>",
                                                 "<mark> Over 65: </mark>", round(wa_age_est$over65_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.6,
                  color = ~ pal6(over65_per), group = "65 and over") %>%
      #Add layer of SPL branch location icons
      addAwesomeMarkers(branches$longitude, branches$latitude, 
                        icon=icons, popup= paste("<i>", branches$name, "</i> <br>",
                                                 "FOR TRACTS WITHIN <br> 1 MILE OF BRANCH: <br>",
                                                 "<b>Average Median Age:</b>","<br>",
                                                 round(branches$age_median_avg, digits = 2), "yrs <br>",
                                                 "<b>Age Breakdown:</b> <br>",
                                                 "Under 18: ", round((branches$age_under18/branches$age_total)*100,digits = 2) , "% <br>",
                                                 "18-39: ", round((branches$age_1839/branches$age_total)*100,digits = 2), "% <br>",
                                                 "40-64: ", round((branches$age_4064/branches$age_total)*100,digits = 2), "% <br>",
                                                 "Over 65: ", round((branches$age_over65/branches$age_total)*100,digits = 2), "%"), group = "SPL Branches") %>% 
      # Layers controls (define layer groups)
      addLayersControl(
        #Base layers have radio dials and legend will be static
        baseGroups = c("Median Age", "under 18", "18-39", "40-64", "65 and over"),
        #Over layers have check boxes and legends will toggle with layer
        overlayGroups = c("SPL Branches"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      #Add home buttons to zoom to each region (coordinates define view bounds)
      #default is lower right corner
      addHomeButton(ext = extent(c(-122.40,-122.30, 47.50, 47.60)), layer.name = "SWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.25, 47.51, 47.61)), layer.name = "SER")  %>%
      addHomeButton(ext = extent(c(-122.37,-122.32, 47.63, 47.73)), layer.name = "NWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.29, 47.65, 47.74)), layer.name = "NER") %>%
      addHomeButton(ext = extent(c(-122.42,-122.33, 47.63, 47.68)), layer.name = "MCW") %>%
      addHomeButton(ext = extent(c(-122.33,-122.27, 47.60, 47.66)), layer.name = "MCE") %>%
      addHomeButton(ext = extent(c(-122.36,-122.31, 47.59, 47.62)), layer.name = "DWN")
    agemap
  })

  #Match the right legend to the correct layer to avoid having two legends on all maps
  #Adapted from: https://github.com/rstudio/leaflet/issues/215
  observeEvent(input$agemap_groups, {
    agemap <- leafletProxy("agemap") %>% clearControls()
    if (input$agemap_groups ==  "Median Age") {
      agemap <- agemap %>% addLegend("bottomleft", 
                  pal = pal, 
                  values = age_median_1$estimate,
                  title = "Median Age of <br> Tract Population",
                  opacity = 1)}
    else if (input$agemap_groups %in%  c("under 18", "18-39", "40-64","65 and over")) {
      agemap <- agemap %>% 
        addLegend("bottomleft", 
                  pal = pal6, 
                  values = c(0,100),
                  title = "Percentage of <br> Tract Population",
                  opacity = 1)}
  })
  
  output$incomemap <- renderLeaflet({
    leaflet() %>% 
      #Base Map
      addProviderTiles("CartoDB.Positron") %>%
      #Center view on the City of Seattle
      setView(lat = 47.6062, lng = -122.3321, zoom = 11) %>%
      #Add income colored polygon layer (name layer with group attribute)
      addPolygons(data=wa_income, popup = paste("<i> Tract ", str_extract(wa_income$NAME, "[0-9]+"),"</i> <br>",
                                                "<b>Median Household Income: </b>","<br>",
                                                "$",format(wa_income$estimate,big.mark=",",scientific=FALSE), "+/- ", "$",format(wa_income$moe,big.mark=",",scientific=FALSE)),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_income(estimate), group = "Median Household Income") %>%
      addLegend("bottomleft", 
                pal = pal_income, 
                values = wa_income$estimate,
                title = "Median Household Income",
                opacity = 1, group='Median Household Income') %>% 
      #Add layer of SPL branch location icons
      addAwesomeMarkers(branches$longitude, branches$latitude, 
                        icon=icons, popup=paste("<i>", branches$name, "</i> <br>",
                                                "FOR TRACTS WITHIN <br> 1 MILE OF BRANCH: <br>",
                                                "<b>Average Median <br> Household Income: </b>","<br>",
                                                "$",format(branches$income_avg,big.mark=",",scientific=FALSE)), group = "SPL Branches") %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("Median Household Income"),
        overlayGroups = c("SPL Branches"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addHomeButton(ext = extent(c(-122.40,-122.30, 47.50, 47.60)), layer.name = "SWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.25, 47.51, 47.61)), layer.name = "SER")  %>%
      addHomeButton(ext = extent(c(-122.37,-122.32, 47.63, 47.73)), layer.name = "NWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.29, 47.65, 47.74)), layer.name = "NER") %>%
      addHomeButton(ext = extent(c(-122.42,-122.33, 47.63, 47.68)), layer.name = "MCW") %>%
      addHomeButton(ext = extent(c(-122.33,-122.27, 47.60, 47.66)), layer.name = "MCE") %>%
      addHomeButton(ext = extent(c(-122.36,-122.31, 47.59, 47.62)), layer.name = "DWN")
  })
  
  output$langmap <- renderLeaflet({
    leaflet() %>% 
      #Base Map
      addProviderTiles("CartoDB.Positron") %>%
      #Center view on the City of Seattle
      setView(lat = 47.6062, lng = -122.3321, zoom = 11) %>%
      #Add the english language colored polygon layer (name layer with group attribute)
      addPolygons(data=wa_lang_est, popup = paste("<i> Tract ", str_extract(wa_lang_est$NAME, "[0-9]+"),"</i> <br>",
                                                  "<b>Language Breakdown:</b> <br>",
                                                  "<mark>Only English: </mark>", round(wa_lang_est$eng_per,digits = 2),"% <br>",
                                                  "Spanish: ", round(wa_lang_est$span_per,digits = 2), "% <br>",
                                                  "Other Indo-European: ", round(wa_lang_est$indo_per,digits = 2), "% <br>",
                                                  "Asian and Pacific Island: ", round(wa_lang_est$asian_per,digits = 2), "% <br>",
                                                  "Other Languages: ", round(wa_lang_est$other_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_lang(eng_per), group = "Only English") %>%
      #Add layer for spanish
      addPolygons(data=wa_lang_est, popup = paste("<i> Tract ", str_extract(wa_lang_est$NAME, "[0-9]+"),"</i> <br>",
                                                  "<b>Language Breakdown:</b> <br>",
                                                  "Only English: ", round(wa_lang_est$eng_per,digits = 2),"% <br>",
                                                  "<mark>Spanish: </mark>", round(wa_lang_est$span_per,digits = 2), "% <br>",
                                                  "Other Indo-European: ", round(wa_lang_est$indo_per,digits = 2), "% <br>",
                                                  "Asian and Pacific Island: ", round(wa_lang_est$asian_per,digits = 2), "% <br>",
                                                  "Other Languages: ", round(wa_lang_est$other_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_lang(span_per), group = "Spanish") %>%
      #add layer for other indo-european languages
      addPolygons(data=wa_lang_est, popup = paste("<i> Tract ", str_extract(wa_lang_est$NAME, "[0-9]+"),"</i> <br>",
                                                  "<b>Language Breakdown:</b> <br>",
                                                  "Only English: ", round(wa_lang_est$eng_per,digits = 2),"% <br>",
                                                  "Spanish: ", round(wa_lang_est$span_per,digits = 2), "% <br>",
                                                  "<mark>Other Indo-European: </mark>", round(wa_lang_est$indo_per,digits = 2), "% <br>",
                                                  "Asian and Pacific Island: ", round(wa_lang_est$asian_per,digits = 2), "% <br>",
                                                  "Other Languages: ", round(wa_lang_est$other_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_lang(indo_per), group = "Other Indo-European") %>%
      #add layer for asian and pacific island languages
      addPolygons(data=wa_lang_est, popup = paste("<i> Tract ", str_extract(wa_lang_est$NAME, "[0-9]+"),"</i> <br>",
                                                  "<b>Language Breakdown:</b> <br>",
                                                  "Only English: ", round(wa_lang_est$eng_per,digits = 2),"% <br>",
                                                  "Spanish: ", round(wa_lang_est$span_per,digits = 2), "% <br>",
                                                  "Other Indo-European: ", round(wa_lang_est$indo_per,digits = 2), "% <br>",
                                                  "<mark>Asian and Pacific Island: </mark>", round(wa_lang_est$asian_per,digits = 2), "% <br>",
                                                  "Other Languages: ", round(wa_lang_est$other_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_lang(asian_per), group = "Asian and Pacific Island") %>%
      #add a layer for other languages
      addPolygons(data=wa_lang_est, popup = paste("<i> Tract ", str_extract(wa_lang_est$NAME, "[0-9]+"),"</i> <br>",
                                                  "<b>Language Breakdown:</b> <br>",
                                                  "Only English: ", round(wa_lang_est$eng_per,digits = 2),"% <br>",
                                                  "Spanish: ", round(wa_lang_est$span_per,digits = 2), "% <br>",
                                                  "Other Indo-European: ", round(wa_lang_est$indo_per,digits = 2), "% <br>",
                                                  "Asian and Pacific Island: ", round(wa_lang_est$asian_per,digits = 2), "% <br>",
                                                  "<mark>Other Languages: </mark>", round(wa_lang_est$other_per,digits = 2), "%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_lang(other_per), group = "Other Languages") %>%
      #add legend for all layers that is calibrated for percentages from 0-100
      addLegend("bottomleft", 
                pal = pal_lang, 
                values = c(0,100),
                title = "Percentage of <br> Tract Population",
                opacity = 1) %>% 
      #Add layer of SPL branch location icons
      addAwesomeMarkers(branches$longitude, branches$latitude, 
                        icon=icons, popup= paste("<i>", branches$name, "</i> <br>",
                                                 "FOR TRACTS WITHIN <br> 1 MILE OF BRANCH: <br>",
                                                 "<b>Language Breakdown:</b> <br>",
                                                 "Only English: ", round((branches$lang_eng/branches$lang_total)*100,digits = 2) , "% <br>",
                                                 "Spanish: ", round((branches$lang_span/branches$lang_total)*100,digits = 2), "% <br>",
                                                 "Other Indo-European: ", round((branches$lang_indo/branches$lang_total)*100,digits = 2), "% <br>",
                                                 "Asian and Pacific Island: ", round((branches$lang_asian/branches$lang_total)*100,digits = 2), "% <br>",
                                                 "Other Languages: ", round((branches$lang_other/branches$lang_total)*100,digits = 2), "%"), group = "SPL Branches") %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("Only English", "Spanish", "Other Indo-European", "Asian and Pacific Island", "Other Languages"),
        overlayGroups = c("SPL Branches"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      addHomeButton(ext = extent(c(-122.40,-122.30, 47.50, 47.60)), layer.name = "SWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.25, 47.51, 47.61)), layer.name = "SER")  %>%
      addHomeButton(ext = extent(c(-122.37,-122.32, 47.63, 47.73)), layer.name = "NWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.29, 47.65, 47.74)), layer.name = "NER") %>%
      addHomeButton(ext = extent(c(-122.42,-122.33, 47.63, 47.68)), layer.name = "MCW") %>%
      addHomeButton(ext = extent(c(-122.33,-122.27, 47.60, 47.66)), layer.name = "MCE") %>%
      addHomeButton(ext = extent(c(-122.36,-122.31, 47.59, 47.62)), layer.name = "DWN")
  })
  output$schoolmap <- renderLeaflet({
    leaflet() %>% 
      #Base Map
      addProviderTiles("CartoDB.Positron") %>% 
      #Center view on the City of Seattle
      setView(lat = 47.6062, lng = -122.3321, zoom = 11) %>%
      #Add the prek school colored polygon layer (name layer with group attribute)
      addPolygons(data=wa_school_est, popup = paste("<i> Tract ", str_extract(wa_school_est$NAME, "[0-9]+"),"</i> <br>","<b>Estimated students <br> enrolled (preschool): </b><br>", (wa_school_est$prek_tot),"<br><b>School Type (preschool): </b><br>Private:",
                                                    round(wa_school_est$pri_prek_per,digits = 2), "% <br> Public: ", round(wa_school_est$pub_prek_per,digits = 2),"%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_school(prek_dif), group = "Preschool") %>%
      #add a lyer for grades k-4
      addPolygons(data=wa_school_est, popup = paste("<i> Tract ", str_extract(wa_school_est$NAME, "[0-9]+"),"</i> <br>","<b>Estimated students <br> enrolled (K-4): </b><br>", (wa_school_est$k4_tot),"<br><b>School Type (K-4): </b><br>Private:",
                                                    round(wa_school_est$pri_k4_per,digits = 2), "% <br> Public: ", round(wa_school_est$pub_k4_per,digits = 2),"%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_school(k4_dif), group = "K-4") %>%
      #add a layer for grades 5-8
      addPolygons(data=wa_school_est, popup = paste("<i> Tract ", str_extract(wa_school_est$NAME, "[0-9]+"),"</i> <br>","<b>Estimated students <br> enrolled (5-8): </b><br>", (wa_school_est$g58_tot),"<br><b>School Type (5-8): </b><br>Private:",
                                                    round(wa_school_est$pri_58_per,digits = 2), "% <br> Public: ", round(wa_school_est$pub_58_per,digits = 2),"%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_school(g58_dif), group = "5-8") %>%
      #add a layer for grades 9-12
      addPolygons(data=wa_school_est, popup = paste("<i> Tract ", str_extract(wa_school_est$NAME, "[0-9]+"),"</i> <br>","<b>Estimated students <br> enrolled (9-12): </b><br>", (wa_school_est$g912_tot),"<br><b>School Type (9-12): </b><br>Private:",
                                                    round(wa_school_est$pri_912_per,digits = 2), "% <br> Public: ", round(wa_school_est$pub_912_per,digits = 2),"%"),
                  stroke = TRUE,
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal_school(g912_dif), group = "9-12") %>%
      #Create a legend for color of difference in school types
      addLegend("bottomleft", 
                #Create similar colored legend with custom labels
                labels=c("Mostly Private","Mixed", "Mostly Public"),
                colors=c("yellow", "cadetblue", "#571B7E"),
                title = "Majority Enrollment",
                opacity = 1) %>% 
      #Add layer of SPL branch location icons
      addAwesomeMarkers(branches$longitude, branches$latitude, 
                        icon=icons, popup= paste("<i>", branches$name, "</i> <br>",
                                                 "FOR TRACTS WITHIN <br> 1 MILE OF BRANCH: <br>",
                                                 "<b>Estimated students <br> enrolled (all levels):</b>","<br>",
                                                 round(branches$school_total, digits = 2), "<br>",
                                                 "<b>School Type (all levels): </b><br>Private:",
                                                 round(((branches$school_pri_prek+branches$school_pri_k4+branches$school_pri_58+branches$school_pri_912)/branches$school_total)*100,digits = 2) , "% <br>",
                                                 "Public: ", round(((branches$school_pub_prek+branches$school_pub_k4+branches$school_pub_58+branches$school_pub_912)/branches$school_total)*100,digits = 2), "%"), group = "SPL Branches") %>% 
      # Layers control
      addLayersControl(
        baseGroups = c("Preschool", "K-4", "5-8", "9-12"),
        overlayGroups = c("SPL Branches"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      #add zoom buttons for regions
      addHomeButton(ext = extent(c(-122.40,-122.30, 47.50, 47.60)), layer.name = "SWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.25, 47.51, 47.61)), layer.name = "SER")  %>%
      addHomeButton(ext = extent(c(-122.37,-122.32, 47.63, 47.73)), layer.name = "NWR") %>% 
      addHomeButton(ext = extent(c(-122.34,-122.29, 47.65, 47.74)), layer.name = "NER") %>%
      addHomeButton(ext = extent(c(-122.42,-122.33, 47.63, 47.68)), layer.name = "MCW") %>%
      addHomeButton(ext = extent(c(-122.33,-122.27, 47.60, 47.66)), layer.name = "MCE") %>%
      addHomeButton(ext = extent(c(-122.36,-122.31, 47.59, 47.62)), layer.name = "DWN")
  })
}

shinyApp(ui = ui, server = server)