require(cruftery)
require(lubridate)

biweek_to_date <- function(biweek, year=NULL){
  biweek_map <- with(environment(date_to_biweek), leap_year_map)
  while(biweek>26 & !is.null(year)){
    biweek <- biweek-26
    year <- year+1
  }
  if(biweek>26 & is.null(year)){
    stop("Biweek must be less than 27 or year must be specified.")
  }
  while(biweek<1 & !is.null(year)){
    biweek <- biweek+26
    year <- year-1
  }
  if(biweek<0 & is.null(year)){
    stop("Biweek must be greater than 0 or year must be specified.")
  }
  biweek_day <- biweek_map$julian_day[match(biweek, biweek_map$biweek)]
  if(!is.null(year)){
    biweek_date <- as.Date(biweek_day-1, origin=paste0("2008-01-01"))
    year(biweek_date) <- year
    return(biweek_date)
  }
  biweek_date <- format(as.Date(biweek_day-1, origin="2008-01-01"), format="%m-%d")
  return(biweek_date)
}

