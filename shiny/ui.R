## load necessary libraries
require(googleCharts)
require(dplyr)

shinyUI(fluidPage(
 ## this starts the googleCharts engine
 googleChartsInit(),
 
 ## create sidebar
 sidebarLayout(
  sidebarPanel(
   
   ## author line
   h5(paste0("Created by Stephen A Lauer, Krzysztof Sakrejda, and Nicholas G Reich")),
   
   tags$hr(),
   
   ## in map, allow for timespan selection
   conditionalPanel(
    condition="input.tabs == 'Map'",
    selectInput("date", "Select Prediction Date", choices = names(table(forecasts$date)))),
   
   conditionalPanel(
    condition="input.tabs == 'Map'",
    selectInput("var", "Select Prediction Variable", 
                choices = list("Outbreak Probability" = "outbreak_prob",
                               "Incidence" = "cpp"))),
   
   conditionalPanel(
    condition="input.tabs == 'Plot'",
    selectInput("moph", "Select MOPH Region", multiple = TRUE,
                choices = seq(0,12)))
  ),
  
  ## create main panel
  mainPanel(
   ## create tabs
   tabsetPanel(
    ## plot map
    tabPanel("Map", ## make chart title here (otherwise not centered)
             h4("Thailand Forecasted Counts per 100,000 Population", align="center"),
             ## make line chart
             googleGeoChart("map", width="100%", height="475px", options = list(
              
              data = list("map"),
              
              region = "TH",
              displayMode = "regions",
              resolution = "provinces",
              
              ## set fonts
              fontName = "Source Sans Pro",
              fontSize = 14,
              
              ## set legend fonts
              legend = list(
               textStyle = list(
                fontSize=14)),
              
              ## set chart area padding
              chartArea = list(
               top = 50, left = 75,
               height = "75%", width = "70%"
              ),
              
              # set colors
              colorAxis = list(
               #maxValue = map_max,
               minValue = map_min,
               colors = cbbPalette[c(4, 5, 7)]),
              
              # set tooltip font size
              tooltip = list(
               textStyle = list(
                fontSize = 14)
              )
             )), id="Map"),
    ## plot tab with google chart options
    tabPanel("Time Series",
             ## make chart title here (otherwise not centered)
             h4("Observed and forecasted cases of dengue fever", align="center"),
             ## make line chart
             googleComboChart("plot", width="100%", height="475px", options = list(
              
              ## set fonts
              fontName = "Source Sans Pro",
              fontSize = 14,
              
              ## set axis titles, ticks, fonts, and ranges
              hAxis = list(
               #title = "",
               #format = "####-##-##",
               #                ticks = seq(1999, 2011, 2),
               #                viewWindow = xlim,
               textStyle = list(
                fontSize = 14),
               titleTextStyle = list(
                fontSize = 16,
                bold = TRUE,
                italic = FALSE)
              ),
              vAxis = list(
               title = "Number of cases per biweek",
               textStyle = list(
                fontSize = 14),
               titleTextStyle = list(
                fontSize = 16,
                bold = TRUE,
                italic = FALSE)
              ),
              
              seriesType = "bars",
              series = list(
               "1" = list(
                type = "line"),
               "2" = list(
                type = "area"),
               "3" = list(
                type = "line", visibleInLegend=FALSE),
               "4" = list(
                type = "area", lineWidth=0, pointSize=0, visibleInLegend=FALSE, areaOpacity=1)
              ),
              
              isStacked = FALSE,
              
              ## set legend fonts
              legend = list(
               textStyle = list(
                fontSize=14)),
              
              ## set chart area padding
              chartArea = list(
               top = 50, left = 75,
               height = "75%", width = "65%"
              ),
              
              #               # Allow pan/zoom
              #               explorer = list(),
              #               # Set bubble visual props
              #               bubble = list(
              #                opacity = 0.4, stroke = "none",
              #                # Hide bubble label
              #                textStyle = list(
              #                 color = "none"
              #                )
              #               ),
              
              ## set colors
              colors = c(cbbPalette[c(1:3,3)], "white"),
              
              ## set point size
              pointSize = 3,
              
              # set tooltip font size
              tooltip = list(
               textStyle = list(
                fontSize = 14)
              )
             )),
             value="Plot"),
    id="tabs")
  )
 )
))