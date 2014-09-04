require(ggplot2)
require(dplyr)
require(cruftery)
require(lubridate)

source("biweek_to_date.R")
counts <- read.csv("../dengueForecastAnalyses/forecasts/20140902_counts_20140902.csv")[,-1]
prov_data <- read.csv("../denguemodeling/spamd/trunk/manuscripts/realTimeForecasting/predictions/thaiProvinces.csv")

counts$pid <- prov_data$fips[match(counts$province, prov_data$Province.ID)]
counts$prov.name <- prov_data$Province.Name[match(counts$province, prov_data$Province.ID)]
counts$biweek <- date_to_biweek(as.Date(counts$date_sick))
counts$biweek[which(is.na(counts$biweek))] <- 26
counts$biweek_day <- biweek_to_date(counts$biweek, year(counts$date_sick))

ddate.colors <- c("#D55E00", "#F0E442", "#009E73")


check_forecasts_graph <- function(df=counts, top.provs=1, to.date=3, pred.ahead=2, lags=1, 
                                  show.back=26, 
                                  dates=ymd_hms(names(table(counts$delivery_date))[c(2:4, 7:22)])) {
  for(i in 1:length(dates)){
    adj_delivery <- biweek_to_date(date_to_biweek(dates[i]), year(dates[i]))
    
    if(!is.numeric(pred.ahead))
      stop("pred.ahead must be a number")
    pred_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)+pred.ahead, 
                                  year=year(adj_delivery))
    
    if(!is.numeric(lags))
      stop("lags must be a number")
    lag_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)-to.date-lags, 
                                  year=year(adj_delivery))
    
    if(is.numeric(to.date))
      to_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)-to.date, 
                                  year=year(adj_delivery))
    if(!is.numeric(to.date))
      to_biweek <- biweek_to_date(biweek=date_to_biweek(as.Date(to.date)), 
                                  year=year(as.Date(to.date)))
    
    if(is.numeric(show.back))
      show_biweek <- biweek_to_date(biweek=date_to_biweek(adj_delivery)-show.back, 
                                    year=year(adj_delivery))
    
    if(!is.numeric(show.back))
      to_biweek <- biweek_to_date(biweek=date_to_biweek(as.Date(show.back)),
                                  year=year(as.Date(show.back)))
    
    biweek_counts <- df %>%
      group_by(disease, province, pid, prov.name, delivery_date, biweek, biweek_day) %>%
      summarise(count=sum(count)) %>%
      filter(disease==26 & biweek_day >= show_biweek & biweek_day<=pred_biweek)
    
    sick.provs <- biweek_counts %>%
      group_by(province, pid) %>%
      summarise(count=sum(count)) %>%
      arrange(desc(count))
    
    provinces <- sick.provs$pid[1:top.provs]
    forecast <- read.csv(paste0("~/Documents/dengueForecastAnalyses/forecasts/20140827_forecast_",
                                gsub("-", "", as.Date(dates[i], format="%Y%m%d")), ".csv"))[,-1]
    forecast$biweek <- ifelse(forecast$biweek==0, 26, forecast$biweek)
    forecast$biweek_day <-as.Date(mapply(biweek_to_date, biweek=forecast$biweek, year=forecast$year),
                                  origin="1970-01-01")
    for(j in 1:length(provinces)){
      plot_counts <- filter(biweek_counts, pid==provinces[j])
      plot_counts$delivery_biweek <- sapply(as.Date(plot_counts$delivery_date), date_to_biweek)
      plot_counts$deliv_biweek_day <- as.Date(mapply(biweek_to_date, 
                                                     biweek=plot_counts$delivery_biweek, 
                                                     year=year(plot_counts$delivery_date)), 
                                              origin="1970-01-01")
      plot_counts$avail <- ifelse(plot_counts$deliv_biweek_day<=adj_delivery, 1, 0)
      plot_counts$used <- ifelse(plot_counts$deliv_biweek_day<=adj_delivery &
                                  # plot_counts$biweek_day>=lag_biweek & 
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
                     breaks=as.Date(mapply(biweek_to_date, 
                                           biweek=seq(date_to_biweek(adj_delivery)-show.back,
                                                      date_to_biweek(adj_delivery)+pred.ahead, 2), 
                                           year=year(adj_delivery)),
                                    origin="1970-01-01"))+ 
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
      ggsave(filename=paste0(plot_counts$prov.name[1], as.Date(dates[i], format="%Y%m%d"), "graph.pdf"), p)
    }
  }
}

check_forecasts_graph()
