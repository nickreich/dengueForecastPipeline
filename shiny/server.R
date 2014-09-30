## load necessary libraries
require(dplyr)
require(googleCharts)
require(cruftery)

## load most recent counts
counts.list <- list.files(path="../counts", pattern = "*.csv")
counts.info <- file.info(paste0("../counts/", counts.list))
recent.count <- which.max(counts.info$mtime)
counts <- read.csv(paste0("../counts/", counts.list[recent.count]))

 
## load most recent forecasts
forecasts.list <- list.files(path="../forecasts", pattern = "*.csv")
forecasts.list2 <- forecasts.list[grep(pattern = "_forecast_", forecasts.list)]
forecasts.info <- file.info(paste0("../forecasts/", forecasts.list2))
recent.forecast <- which.max(forecasts.info$mtime)
forecasts <- read.csv(paste0("../forecasts/", forecasts.list2[recent.forecast]))

## load thai population data
thai.pop <- read.csv("2010Census.csv")

## set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7")

shinyServer(function(input, output) {
 
 ## create the plot of the data
 output$plot <- reactive({
  ## format counts into plot format 
  plot_counts <- counts %>%
   filter(disease == 26, date_sick_year >=2013) %>%
   group_by(date_sick_biweek, date_sick_year) %>%
   summarise(count = sum(count)) 
  
  ## add date variable
  plot_counts$date <- biweek_to_date(plot_counts$date_sick_biweek, 
                                     plot_counts$date_sick_year)
  
  plot_forecasts <- forecasts %>%
   group_by(biweek, year) %>%
   summarise(predicted_count = sum(predicted_count))#,
  #              ub = sum(ub),
  #              lb = sum(lb))
  
  plot_forecasts$date <- biweek_to_date(plot_forecasts$biweek, 
                                        plot_forecasts$year)
  
  plot_df <- merge(plot_counts, plot_forecasts, by = "date", all=T)[,c("date", "count", "predicted_count")]#, "ub", "lb")]
  
  plot_df$date <- as.Date(plot_df$date)
  
  colnames(plot_df) <- c("Date", "Observed Cases", "Forecasted Cases")
  
  ## this outputs the google data to be used in the UI to create the dataframe
  list(
   data=googleDataTable(plot_df))
 })
 
 # create the map of the data
 output$map <- reactive({
  map_biweeks <- names(table(forecasts$biweek))
  
  map_forecasts <- merge(forecasts, thai.pop, by.x = "pid", by.y = "FIPS", all.x=T) %>%
   filter(biweek == as.numeric(map_biweeks[input$biweek]))
  
  map_forecasts$cpp <- round(100000*map_forecasts$predicted_count/map_forecasts$Population,2)
  
  map_forecasts$pname <- ifelse(map_forecasts$pname == "Bangkok Metropolis", "Bangkok",
                                as.character(map_forecasts$pname))
  
  map_df <- data.frame(map_forecasts$pname, map_forecasts$cpp)
  colnames(map_df) <- c("Province", "Counts per 100,000 Population")
  list(
   data=googleDataTable(map_df))
  
#   gvisGeoChart(data=map_df, locationvar="Provinces", colorvar='Count', 
#                options=list(region="TH", displayMode="regions", resolution="provinces",
#                             height=600, width=1000, keepAspectRatio=FALSE, 
#                             colorAxis = "{colors:[\'green', \'yellow\', \'red']}"))
 })
 
})