require(ggplot2)
require(dplyr)
require(cruftery)
require(lubridate)

# Load most recent counts
counts.list <- list.files(path="counts", pattern = "*.csv")
counts.info <- file.info(paste0("counts/", counts.list))
recent.count <- which.max(counts.info$mtime)
counts <- read.csv(paste0("counts/", counts.list[recent.count]))

# load Thai Province data from cruftery
data(thai_prov_data)

# read arguments from terminal
options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

# if no terminal arguments, default to today's date
if(length(args)==0)
 args <- Sys.Date()

# attach correct province numbers to counts data
counts$pid <- thai_prov_data$FIPS[match(counts$province, thai_prov_data$ISO)]
counts$prov.name <- thai_prov_data$Province[match(counts$province, thai_prov_data$ISO)]

# assign each date_sick_biweek to the first date of the biweek
counts$biweek_day <- biweek_to_date(counts$date_sick_biweek, counts$date_sick_year)

ddate.colors <- c("#D55E00", "#F0E442", "#009E73")

check_forecasts_graph <- function(df=counts, top.provs=1, to.date=4, pred.ahead=2, lags=1, 
                                  show.back=26, dates=args[1]) {
  for(i in 1:length(dates)){
   # find the date of the first day of the biweek of the specified delivery date
    adj_delivery <- biweek_to_date(date_to_biweek(dates[i]), year(dates[i]))
    
    # check if pred.ahead is a number
    if(!is.numeric(pred.ahead))
      stop("pred.ahead must be a number")
    # find date of the first day of the biweek pred.ahead weeks in the future (from delivery date)
    pred_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)+pred.ahead, 
                                  year=year(adj_delivery))
    
    # if to.date is a number, find the date of the biweek to.date weeks in the past
    if(is.numeric(to.date))
     to_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)-to.date, 
                                 year=year(adj_delivery))
    # if to.date is a date, find the date of the first day of the to.date biweek
    if(!is.numeric(to.date))
     to_biweek <- biweek_to_date(biweek=date_to_biweek(as.Date(to.date)), 
                                 year=year(as.Date(to.date)))
        
    # check if lags is a number
    if(!is.numeric(lags))
      stop("lags must be a number")
    # find date of the first day of the biweek of lags weeks before to.date
    lag_biweek <- biweek_to_date(biweek=date_to_biweek(to_biweek)-lags, year=year(to_biweek))
    
    # if show.back is a number, find the date of the biweek show.back weeks in the past
    if(is.numeric(show.back))
      show_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)-show.back, 
                                    year=year(adj_delivery))
    
    # if show.back is a date, find the date of the first day of the show.back biweek
    if(!is.numeric(show.back))
      show_biweek <- biweek_to_date(biweek=date_to_biweek(as.Date(show.back)),
                                  year=year(as.Date(show.back)))
    
    biweek_counts <- df %>%
      group_by(disease, province, pid, prov.name, delivery_date, date_sick_biweek, biweek_day) %>%
      summarise(count=sum(count)) %>%
      filter(biweek_day >= show_biweek & biweek_day<=pred_biweek)
    
    sick.provs <- biweek_counts %>%
      group_by(province, pid) %>%
      summarise(count=sum(count)) %>%
      arrange(desc(count))
    
    provinces <- sick.provs$pid[1:top.provs]
    forecast <- read.csv(paste0("forecasts/", as.character(Sys.Date(), format="%Y%m%d"),
                                "_forecast_", as.character(dates[i], format="%Y%m%d"), ".csv"))[,-1]
    forecast$biweek_day <-biweek_to_date(biweek=forecast$biweek, year=forecast$year)
    for(j in 1:length(provinces)){
      plot_counts <- filter(biweek_counts, pid==provinces[j])
      plot_counts$delivery_biweek <- date_to_biweek(as.Date(plot_counts$delivery_date))
      plot_counts$deliv_biweek_day <- biweek_to_date(biweek=plot_counts$delivery_biweek, 
                                                     year=year(plot_counts$delivery_date))
      plot_counts$avail <- ifelse(plot_counts$deliv_biweek_day<=adj_delivery, 1, 0)
      plot_counts$used <- ifelse(plot_counts$deliv_biweek_day<=adj_delivery &
                                   plot_counts$biweek_day<to_biweek, 1, 0)
      plot_counts$type <- plot_counts$avail+plot_counts$used+1
      fill.cols <- ddate.colors[c(as.numeric(names(table(plot_counts$type))))]
      forecast1 <- filter(forecast, pid==as.character(provinces[j]))
      
      p <- ggplot() +
        geom_bar(data=plot_counts, aes(x=biweek_day, y=count, fill=as.factor(type)), 
                 stat="identity") +
        geom_line(data=forecast1, aes(x=biweek_day, y=predicted_count, color=pname), size=2) +
        geom_point(data=forecast1, aes(x=biweek_day, y=predicted_count, color=pname), size=4) +
        scale_x_date(name="Date", limits=c(show_biweek,pred_biweek), 
                     breaks=biweek_to_date(biweek=seq(date_to_biweek(adj_delivery)-show.back,
                                                      date_to_biweek(adj_delivery)+pred.ahead, 2), 
                                           year=year(adj_delivery)))+ 
        scale_y_continuous(name="Case Counts") +
        ggtitle(paste0("Actual and Predicted Case Counts for\n", plot_counts$prov.name[1], " on ",
                       as.Date(dates[i], format="%Y%m%d"))) +
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
      ggsave(filename=paste0("forecasts/", plot_counts$prov.name[1], as.Date(dates[i], format="%Y%m%d"), "graph.pdf"), p)
    }
  }
}

check_forecasts_graph()
