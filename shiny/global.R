library(dplyr)
library(googleCharts)
library(dengueThailand)
library(spatialpred)
library(ggplot2)

## load most recent counts
counts_file <- list.files(pattern = "counts")
counts_info <- file.info(counts_file)
recent_counts <- which.max(counts_info$ctime)
counts <- read.csv(counts_file[recent_counts])

counts$date <- biweek_to_date(counts$biweek, counts$year)

## load most recent forecasts
forecasts_file <- list.files(pattern = "forecast")
forecasts_info <- file.info(forecasts_file)
recent_forecasts <- which.max(forecasts_info$ctime)
forecasts <- read.csv(forecasts_file[recent_forecasts])
forecasts$date <- biweek_to_date(forecasts$biweek, forecasts$year)
forecasts$outbreak_prob <- round(forecasts$outbreak_prob)

## load thai population data
data(thai_prov_data)

## merge sheets
mapMaxdf <- merge(forecasts, thai_prov_data, by.x = "pid", by.y = "FIPS", all.x=T)

## calculate predicted cases per 100,000 population
mapMaxdf$cpp <- round(100000*mapMaxdf$predicted_count/mapMaxdf$Population,2)

map_max <- max(ceiling(mapMaxdf$cpp), na.rm=T)
map_min <- min(mapMaxdf$cpp, na.rm=T)

## set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#cccccc")
