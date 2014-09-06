## make country prediction graph
## Nicholas Reich
## September 2014

#'@param forecast_file file containing forecasts
#'@param counts_file file containing counts
#'

make_country_prediction_line_graph <- function(forecast_file, counts_file) {
                require(dplyr)
                forecasts <- tbl_df(read.csv(forecast_file))
                counts <- tbl_df(read.csv(counts_file))
        
                ## move counts to biweeks
                counts$biweek <- date_to_biweek(as.Date(counts$date_sick))
                counts$biweek[which(is.na(counts$biweek))] <- 26
                counts$biweek_day <- biweek_to_date(counts$biweek, year(counts$date_sick))
                counts$year <- year(counts$date_sick)
                
                ## aggregate to country-level
                forecasts_cntry <- forecasts %>% group_by(biweek, year) %>% 
                        summarize(predicted_cntry_count = sum(predicted_count),
                                  predicted_ub = sum(ub),
                                  predicted_lb = sum(lb)) %>%
                        mutate(time = year + (biweek-1)/26)
                
                counts_cntry <- counts %>% group_by(biweek, year) %>%
                        summarize(cntry_count = sum(count)) %>%
                        mutate(time = year + (biweek-1)/26)
                
                ## add column in counts_cntry indicating which biweeks were left out of the fit
                
                ## make better time labels!
                
                ## make plot
                ggplot() + theme_bw() +
                        ## plot counts
                        geom_bar(data=counts_cntry, 
                                 aes(x=time, y=cntry_count), 
                                 stat="identity") +
                        ## add forecasts
                        geom_line(data=forecasts_cntry, aes(x=time, y=predicted_cntry_count)) +
                        geom_point(data=forecasts_cntry, aes(x=time, y=predicted_cntry_count)) +
                        geom_ribbon(data=forecasts_cntry, aes(x=time, 
                                                              ymin=predicted_lb, ymax=predicted_ub), 
                                                              alpha=I(.1)) +
                        # air-brushing
                        xlim(2013, 2015) + xlab(NULL) + ylab(NULL) +
                        ggtitle("Observed and predicted DHF case counts for all of Thailand")
        }