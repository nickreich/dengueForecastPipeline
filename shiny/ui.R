## load necessary libraries
require(googleCharts)

## load in the data
# Load most recent counts
# counts.list <- list.files(path="../counts", pattern = "*.csv")
# counts.info <- file.info(paste0("../counts/", counts.list))
# recent.count <- which.max(counts.info$mtime)
# counts <- read.csv(paste0("../counts/", counts.list[recent.count]))

## create maxs and mins for googleCharts
# xlim <- list(
#  min = min(suidata$Year)-1,
#  max = max(suidata$Year)+1
# )
# ylim <- list(
#  min = 0,
#  max = max(suidata$Crude.Rate, na.rm=T)+5
# )

## set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

shinyUI(fluidPage(
 ## this starts the googleCharts engine
 googleChartsInit(),
 
 ## create sidebar
 sidebarLayout(
  sidebarPanel(
   helpText("If using Internet Explorer, application only visible in version 10."),
   tags$hr(),
   
   ## in map, allow for timespan selection
   conditionalPanel(
    condition="input.tabs == 'map'",
    sliderInput("biweek", "Select Prediction Biweek",
                min=1, max=6, value=1)),
  
  tags$hr(),
  
  ## author line
  helpText("Created by Stephen A Lauer")
  ),
    
  ## create main panel
  mainPanel(
   ## create tabs
   tabsetPanel(
    ## plot map
    tabPanel("map", ## make chart title here (otherwise not centered)
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
               colors = cbbPalette[c(4, 5, 7)]),
              
              # set tooltip font size
              tooltip = list(
               textStyle = list(
                fontSize = 14)
              )
             )), id="map"),
    ## plot tab with google chart options
    tabPanel("plot",
             ## make chart title here (otherwise not centered)
             h4("Thailand Counts and Forecast", align="center"),
             ## make line chart
             googleComboChart("plot", width="100%", height="475px", options = list(
              
              ## set fonts
              fontName = "Source Sans Pro",
              fontSize = 14,
              
              ## set axis titles, ticks, fonts, and ranges
              hAxis = list(
               title = "Date",
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
               title = "Counts",
               #                viewWindow = ylim,
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
                type = "line")#,
               #                "2" = list(
               #                 type = "area"),
               #                "3" = list(
               #                 type = "line")
              ),
              
              ## set legend fonts
              legend = list(
               textStyle = list(
                fontSize=14)),
              
              ## set chart area padding
              chartArea = list(
               top = 50, left = 75,
               height = "75%", width = "70%"
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
              colors = cbbPalette,
              
              ## set point size
              pointSize = 3,
              
              # set tooltip font size
              tooltip = list(
               textStyle = list(
                fontSize = 14)
              )
             )),
             value="plot"),
    id="tabs")
  )
 )
))