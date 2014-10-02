require(dplyr)
require(cruftery)

## load most recent counts
counts.list <- list.files(path="../counts", pattern = "*.csv")
counts.info <- file.info(paste0("../counts/", counts.list))
recent.count <- which.max(counts.info$mtime)
counts <- read.csv(paste0("../counts/", counts.list[recent.count]))

counts$date <- biweek_to_date(counts$date_sick_biweek, counts$date_sick_year)

## load most recent forecasts
forecasts.list <- list.files(path="../forecasts", pattern = "*.csv")
forecasts.list2 <- forecasts.list[grep(pattern = "_forecast_", forecasts.list)]
forecasts.info <- file.info(paste0("../forecasts/", forecasts.list2))
recent.forecast <- which.max(forecasts.info$mtime)
forecasts <- read.csv(paste0("../forecasts/", forecasts.list2[recent.forecast]))
forecasts$date <- biweek_to_date(forecasts$biweek, forecasts$year)

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
                "#0072B2", "#D55E00", "#CC79A7")
