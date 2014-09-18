require(devtools)
install_github(rep='sakrejda/cruftery/package_dir')
require(cruftery)
require(lubridate)
require(dplyr)

options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

# Load most recent counts
counts.list <- list.files(path="counts", pattern = "*.csv")
counts.info <- file.info(paste0("counts/", counts.list))
recent.count <- which.max(counts.info$mtime)
counts <- read.csv(paste0("counts/", counts.list[recent.count]))

deliv.dates <- as.Date(names(table(counts$delivery_date)))

if(nchar(args[1])==10){
 deliv.dates <- deliv.dates[which(deliv.dates >= as.Date(args))]
} else{
 biweeks.back <- ifelse(length(args)==0, 0, as.numeric(args[1]))
 back.date <- biweek_to_date(biweek=date_to_biweek(Sys.Date())-biweeks.back, year=year(Sys.Date()))
 deliv.dates <- deliv.dates[which(deliv.dates >= back.date)]
}

date.list <- unique(biweek_to_date(biweek=date_to_biweek(deliv.dates)+1, year=year(deliv.dates)))

write.csv(date.list, file=paste0("data/", as.character(Sys.Date(), "%Y%m%d"), "dateList.csv"))