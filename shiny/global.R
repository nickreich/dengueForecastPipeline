library(dplyr)
library(googleCharts)
library(dengueThailand)
library(spatialpred)

## load most recent counts
counts.file <- list.files(pattern = "counts")
counts <- read.csv(counts.file)

counts$date <- biweek_to_date(counts$date_sick_biweek, counts$date_sick_year)

## load most recent forecasts
forecasts.file <- list.files(pattern = "forecast")
forecasts <- read.csv(forecasts.file)
forecasts$date <- biweek_to_date(forecasts$biweek, forecasts$year)
forecasts$outbreak_prob <- round(100*forecasts$outbreak_prob)

## load thai population data
data(thai_prov_data)

## merge sheets
mapMaxdf <- merge(forecasts, thai_prov_data, by.x = "pid", by.y = "FIPS", all.x=T)

## calculate predicted cases per 100,000 population
mapMaxdf$cpp <- round(100000*mapMaxdf$predicted_count/mapMaxdf$Population)

map_max <- max(mapMaxdf$cpp, na.rm=T)
map_min <- min(mapMaxdf$cpp, na.rm=T)

## set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#cccccc")
