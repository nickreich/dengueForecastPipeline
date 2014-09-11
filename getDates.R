require(devtools)
install_github(rep='sakrejda/cruftery/package_dir')
require(cruftery)
require(lubridate)

options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

deliv.biweek <- date_to_biweek(Sys.Date())
deliv.year <- year(Sys.Date())

if(nchar(args[1])==10){
 first.biweek <- date_to_biweek(as.Date(args[1]))
 first.year <- year(as.Date(args[1]))
 weeks.back <- deliv.year*26+deliv.biweek - first.year*26 - first.biweek
} else
 weeks.back <- ifelse(length(args)==0, 0, as.numeric(args[1]))

date.list <- biweek_to_date(biweek=seq(deliv.biweek-weeks.back, deliv.biweek, 1), year=deliv.year)

write.csv(date.list, file=paste0("data/", as.character(Sys.Date(), "%Y%m%d"), "dateList.csv"))