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
                        summarize(predicted_cntry_count = sum(predicted_count)) %>%
                        mutate(time = year + (biweek-1)/26)
                
                counts_cntry <- counts %>% group_by(biweek, year) %>%
                        summarize(cntry_count = sum(count)) %>%
                        mutate(time = year + (biweek-1)/26)
                
                ## add column in counts_cntry indicating which biweeks were left out of the fit
                
                ## add confidence intervals
                
                ## make plot
                ggplot() + theme_bw() +
                        geom_bar(data=counts_cntry, 
                                 aes(x=time, y=cntry_count), 
                                 stat="identity") +
                        ## add forecasts
                        geom_line(data=forecasts_cntry, aes(x=time, y=predicted_cntry_count)) +
                        xlim(2012, 2015) + xlab(NULL) + ylab("DHF case count")
        }