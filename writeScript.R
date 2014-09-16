## get most recent dates
dates.list <- list.files(path="data", pattern = "*.csv")
dates.info <- file.info(paste0("data/", dates.list))
recent.date <- which.max(dates.info$mtime)
dateList <- read.csv(paste0("data/", dates.list[recent.date]))

sink("doForecast.sh")
for(i in 1:length(dateList[,2])){
 shell.code <- paste0("Rscript makeForecast.R ", dateList[i,2], " > forecasts/",
                      as.character(Sys.Date(), "%Y%m%d"), 
                      "_forecast_", as.character(as.Date(dateList[i,2]), "%Y%m%d"), ".Rout\n",
                      "Rscript graphForecasts.R ", dateList[i,2], " > forecasts/",
                      as.character(Sys.Date(), "%Y%m%d"), 
                      "_graph_", as.character(as.Date(dateList[i,2]), "%Y%m%d"), ".Rout\n")
 cat(shell.code)
}

sink()