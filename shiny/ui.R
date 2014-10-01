## load necessary libraries
require(googleCharts)
require(dplyr)

## load most recent counts
counts.list <- list.files(path="../counts", pattern = "*.csv")
counts.info <- file.info(paste0("../counts/", counts.list))
recent.count <- which.max(counts.info$mtime)
counts <- read.csv(paste0("../counts/", counts.list[recent.count]))

## summarise counts for whole country in each biweek
plot_counts <- counts %>%
 filter(disease == 26, date_sick_year >=2013) %>%
 group_by(date_sick_biweek, date_sick_year) %>%
 summarise(count = sum(count)) 

plot_max <- ceiling(max(plot_counts$count)/1000)*1000

## load most recent forecasts
forecasts.list <- list.files(path="../forecasts", pattern = "*.csv")
forecasts.list2 <- forecasts.list[grep(pattern = "_forecast_", forecasts.list)]
forecasts.info <- file.info(paste0("../forecasts/", forecasts.list2))
recent.forecast <- which.max(forecasts.info$mtime)
forecasts <- read.csv(paste0("../forecasts/", forecasts.list2[recent.forecast]))

## load thai population data
thai.pop <- read.csv("2010Census.csv")

## merge sheets
map_forecasts <- merge(forecasts, thai.pop, by.x = "pid", by.y = "FIPS", all.x=T)

## calculate predicted cases per 100,000 population
map_forecasts$cpp <- round(100000*map_forecasts$predicted_count/map_forecasts$Population,2)

map_max <- max(map_forecasts$cpp, na.rm=T)
map_min <- min(map_forecasts$cpp, na.rm=T)

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
    condition="input.tabs == 'Map'",
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
               maxValue = map_max,
               minValue = map_min,
               colors = cbbPalette[c(4, 5, 7)]),
              
              # set tooltip font size
              tooltip = list(
               textStyle = list(
                fontSize = 14)
              )
             )), id="Map"),
    ## plot tab with google chart options
    tabPanel("Plot",
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
               title = "Number of Cases",
               viewWindow = list(max = plot_max),
               #maxValue = max(plot_counts$count),
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