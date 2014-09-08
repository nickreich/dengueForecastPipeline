require(cruftery)
require(lubridate)

biweek_to_date <- function(biweek, year){
 if(length(biweek)!=length(year))
  message("Note: the input biweeks and years are not of the same length.")
 biweek.df <- data.frame(Biweek=biweek, Year=year)
 biweek.df$Leap <- leap_year(biweek.df$Year)
 biweek.df$Yr.Adj <- 0
 biweek.df$Yr.Adj <- biweek.df$Year + floor((biweek.df$Biweek - 1)/26)
 biweek.df$Bw.Adj <- biweek.df$Biweek - 26*floor((biweek.df$Biweek - 1)/26)
 biweek_map <- with(environment(date_to_biweek), leap_year_map)
 biweek.df$Bw.Day <- biweek_map$julian_day[match(biweek.df$Bw.Adj, biweek_map$biweek)]
 biweek.df$Date <- as.Date(biweek.df$Bw.Day-1, origin=paste0("2008-01-01"))
 year(biweek.df$Date) <- biweek.df$Yr.Adj
 return(biweek.df$Date)
}