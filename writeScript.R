dateList <- read.csv(paste0("data/", as.character(Sys.Date(), "%Y%m%d"), "dateList.csv"))
sink("doForecast.sh")
for(i in 1:length(dateList[,2])){
 shell.code <- paste0("Rscript makeForecast.R ", dateList[i,2], " > forecasts/",
                      as.character(Sys.Date(), "%Y%m%d"), 
                      "_forecast_", as.character(as.Date(dateList[i,2]), "%Y%m%d"), ".Rout\n",
                      "Rscript graphForecasts.R ", dateList[i,2], " > graphs/",
                      as.character(Sys.Date(), "%Y%m%d"), 
                      "_graph_", as.character(as.Date(dateList[i,2]), "%Y%m%d"), ".Rout\n")
 cat(shell.code)
}

sink()