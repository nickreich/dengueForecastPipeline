require(ggplot2)
require(dplyr)
require(cruftery)
require(lubridate)

# Load most recent counts
counts.list <- list.files(path="counts", pattern = "*.csv")
counts.info <- file.info(paste0("counts/", counts.list))
recent.count <- which.max(counts.info$mtime)
counts <- read.csv(paste0("counts/", counts.list[recent.count]))

# read arguments from terminal
options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

# if no terminal arguments, default to today's date
if(length(args)==0)
 args <- Sys.Date()

check_forecasts_graph <- function(df=counts, top.provs=1, TO_DATE=4, pred.ahead=1, lags=1, show.ahead=2,
                                  show.back=26, DELIVERY_DATE=args[1], save.graph=TRUE) {
 # load Thai Province data from cruftery
 data(thai_prov_data)
 
 # attach correct province numbers to counts data
 df$pid <- thai_prov_data$FIPS[match(df$province, thai_prov_data$ISO)]
 df$prov.name <- thai_prov_data$Province[match(df$province, thai_prov_data$ISO)]
 
 # assign each date_sick_biweek to the first date of the biweek
 df$biweek_day <- biweek_to_date(df$date_sick_biweek, df$date_sick_year)
 
 ddate.colors <- c("#D55E00", "#F0E442", "#009E73")
 
 for(i in 1:length(DELIVERY_DATE)){
  # find the date of the first day of the biweek of the specified delivery date
  adj_delivery <- biweek_to_date(date_to_biweek(as.Date(DELIVERY_DATE[i])), year(as.Date(DELIVERY_DATE[i])))
  
  # find date of the first day of the biweek pred.ahead weeks in the future (from delivery date)
  pred_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)+pred.ahead, year=year(adj_delivery))
  
  # if TO_DATE is a number, find the date of the biweek TO_DATE weeks in the past
  if(is.numeric(TO_DATE))
   to_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)-TO_DATE, 
                               year=year(adj_delivery))
  # if TO_DATE is a date, find the date of the first day of the TO_DATE biweek
  if(!is.numeric(TO_DATE))
   to_biweek <- biweek_to_date(biweek=date_to_biweek(as.Date(TO_DATE)), 
                               year=year(as.Date(TO_DATE)))
  
  # check if lags is a number
  if(!is.numeric(lags))
   stop("lags must be a number")
  
  # find date of the first day of the biweek of lags weeks before TO_DATE
  lag_biweek <- biweek_to_date(biweek=date_to_biweek(to_biweek)-lags, year=year(to_biweek))
  
  # if show.back is a number, find the date of the biweek show.back weeks in the past
  if(is.numeric(show.back)){
   show_back_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)-show.back, 
                                 year=year(adj_delivery))
   show_back_biweeks <- show.back
  }
  
  # if show.back is a date, find the date of the first day of the show.back biweek
  if(!is.numeric(show.back)){
   show_back_biweek <- biweek_to_date(biweek=date_to_biweek(as.Date(show.back)),
                                 year=year(as.Date(show.back)))
   show_back_biweeks <- 26*(year(adj_delivery)-year(show_back_biweek)) + date_to_biweek(adj_delivery)-date_to_biweek(show_back_biweek)
  }
  
  # if show.ahead is a number, find the date of the biweek show.ahead weeks in the past
  if(is.numeric(show.ahead)){
   show_ahead_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery) + show.ahead, 
                                 year=year(adj_delivery))
   show_ahead_biweeks <- show.ahead
  }
  
  # if show.ahead is a date, find the date of the first day of the show.ahead biweek
  if(!is.numeric(show.ahead)){
   show_ahead_biweek <- biweek_to_date(biweek=date_to_biweek(as.Date(show.ahead)),
                                 year=year(as.Date(show.ahead)))
   show_ahead_biweeks <- 26*(year(show_ahead_biweek) - year(adj_delivery)) - date_to_biweek(adj_delivery) + 
    date_to_biweek(show_ahead_biweek)
  }
  
  # summarise counts by biweek
  biweek_counts <- df %>%
   group_by(disease, province, pid, prov.name, delivery_date, date_sick_biweek, biweek_day) %>%
   summarise(count=sum(count)) %>%
   filter(biweek_day >= show_back_biweek & biweek_day<=pred_biweek)
  
  # find provinces with highest sick counts
  sick.provs <- biweek_counts %>%
   group_by(province, pid) %>%
   summarise(count=sum(count)) %>%
   arrange(desc(count))
  
  provinces <- sick.provs$pid[1:top.provs]
  
  # load proper forecast
  forecasts.list <- list.files(path="forecasts", pattern = "*.csv")
  forecasts.list <- forecasts.list[grep(paste0("forecast_", as.character(as.Date(DELIVERY_DATE[i]), format="%Y%m%d")),
                                        forecasts.list)]
  
  # check if appropriate forecast is available. if not, issue warning. if so, load up proper file
  tryCatch({
  if(length(forecasts.list)==0)
   stop(paste("Please run makeForecast for", DELIVERY_DATE[i]))
  forecasts.info <- file.info(paste0("forecasts/", forecasts.list))
  recent.forecast <- which.max(forecasts.info$mtime)
  forecast <- read.csv(paste0("forecasts/", forecasts.list[recent.forecast]))
  forecast$biweek_day <-biweek_to_date(biweek=forecast$biweek, year=forecast$year)
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")})
  
  for(j in 1:length(provinces)){
   # filter counts by province
   plot_counts <- filter(biweek_counts, pid==provinces[j])
   
   # add biweek numbers and biweek dates to counts
   plot_counts$delivery_biweek <- date_to_biweek(as.Date(plot_counts$delivery_date))
   plot_counts$deliv_biweek_day <- biweek_to_date(biweek=plot_counts$delivery_biweek, 
                                                  year=year(plot_counts$delivery_date))
   # specify which counts were available, used, or both
   plot_counts$avail <- ifelse(plot_counts$deliv_biweek_day<=adj_delivery, 1, 0)
   plot_counts$used <- ifelse(plot_counts$deliv_biweek_day<=adj_delivery &
                               plot_counts$biweek_day<to_biweek, 1, 0)
   plot_counts$type <- plot_counts$avail+plot_counts$used+1
   
   # set the colors based on data available
   fill.cols <- ddate.colors[c(as.numeric(names(table(plot_counts$type))))]
   
   # filter forecast by province
   forecast1 <- filter(forecast, pid==as.character(provinces[j]))
   
   # plot counts and forecast
   p <- ggplot() +
    geom_bar(data=plot_counts, aes(x=biweek_day, y=count, fill=as.factor(type)), 
             stat="identity") +
    geom_line(data=forecast1, aes(x=biweek_day, y=predicted_count, color=pname), size=2) +
    geom_point(data=forecast1, aes(x=biweek_day, y=predicted_count, color=pname), size=4) +
    geom_ribbon(data=forecast1, aes(x=biweek_day, ymin=lb, ymax=ub), alpha=0.4, fill="#0072B2") +
    scale_x_date(name="Date", limits=c(show_back_biweek,show_ahead_biweek), 
                 breaks=biweek_to_date(biweek=seq(date_to_biweek(adj_delivery)-show_back_biweeks,
                                                  date_to_biweek(adj_delivery)+show_ahead_biweeks, 2), 
                                       year=year(adj_delivery)))+ 
    scale_y_continuous(name="Case Counts") +
    ggtitle(paste0("Actual and Predicted Case Counts for\n", plot_counts$prov.name[1], " on ",
                   as.Date(DELIVERY_DATE[i], format="%Y%m%d"))) +
    scale_fill_manual(name="Availability", values=fill.cols,
                      breaks=c(3,2,1),
                      labels=c("Available and Used", "Available and Unused", "Unavailable")) +
    scale_color_manual(name="", labels="Predicted Count", values="#000000")+
    theme_bw() +
    theme(plot.title = element_text(size=18, face="bold"),
          axis.title.x = element_text(size=16, face="bold"),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x = element_text(size=12, angle=90, vjust=0.5),
          axis.text.y = element_text(size=12),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"))
   print(p)
   # save plot to pdf
   if(save.graph==TRUE)
    ggsave(filename=paste0("forecasts/", plot_counts$prov.name[1], as.Date(DELIVERY_DATE[i], format="%Y%m%d"), "graph.pdf"), p)
  }
 }
}

check_forecasts_graph()
