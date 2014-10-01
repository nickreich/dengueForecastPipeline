## Calculating MASE for fitted forecasts
## Nicholas Reich
## September 2014

library(dplyr)
library(reshape2)
library(cruftery)

mae <- function(y, yhat) {
        mean(abs(y-yhat))
}

##################
## read in data ##
##################

## get current data, fix as "ground truth" for 2014-01-01 through 2014-07-01
counts_file <- "20140912_newcounts_20140905.csv"
counts <- read.csv(file.path('forecasts', counts_file), 
                   stringsAsFactors=FALSE)

## aggregate away the delivery date
counts <- counts %>%
        group_by(disease, date_sick_year, date_sick_biweek, province) %>%
        summarize(count=round(sum(count)))

## xtab to fill in all zeroes
counts_filled <- counts %>% 
        xtabs(formula=count~province+date_sick_biweek+date_sick_year) %>% 
        as.data.frame(stringsAsFactors=FALSE) %>% tbl_df() 

## subset to calculate only on counts since 2001
counts_since_2001 <- counts_filled %>%
        mutate(province = as.numeric(province),
               date_sick_biweek = as.numeric(date_sick_biweek),
               date_sick_year = as.numeric(date_sick_year)) %>%
        filter(date_sick_year>=2001)

## get baseline truth for 2014
counts_2014 <- counts_since_2001 %>% 
        filter(date_sick_year>=2014 & date_sick_biweek<=13) %>%
        mutate(true_count=Freq) %>%
        select(-Freq)

## read in all sets of forecasts


###############################
## calculate baseline models ##
###############################


## calculate MAE for province-yearly-mean model
province_mean <- counts_since_2001 %>% 
        group_by(province) %>%
        summarize(mean_count = round(mean(Freq)),
                  median_count = round(median(Freq)))

province_mean_eval <- left_join(counts_2014, province_mean) %>% 
        group_by(province) %>%
        summarize(MAE_annual_mean=mae(mean_count, true_count),
                  MAE_annual_median=mae(median_count, true_count)) %>%
        left_join(province_mean) 


## calculate MAE for province-biweekly-mean model
province_biweek_mean <- counts_since_2001 %>% 
        group_by(province, date_sick_biweek) %>%
        summarize(mean_count = round(mean(Freq)),
                  median_count = round(median(Freq)))

province_biweek_mean_eval <- left_join(counts_2014, province_biweek_mean) %>% 
        group_by(province) %>%
        summarize(MAE_biweekly_mean=mae(mean_count, true_count),
                  MAE_biweekly_median = mae(median_count, true_count))


## compare biweekly and annual models
MASE <- inner_join(province_mean_eval, 
                   province_biweek_mean_eval) %>%
        mutate(mase_annual_mean=MAE_annual_mean/MAE_annual_mean,
               mase_annual_median = MAE_annual_median/MAE_annual_mean,
               mase_biweek_mean = MAE_biweekly_mean/MAE_annual_mean,
               mase_biweek_median = MAE_biweekly_median/MAE_annual_mean)

data(thai_prov_data)
MASE <- left_join(MASE, thai_prov_data) %>% 
        left_join(province_mean) %>%
        select(province, Province, 
               mase_annual_mean, mase_annual_median, mase_biweek_mean, mase_biweek_median, 
               mean_count, MAE_annual_mean, MAE_biweekly_mean, Population, MOPH_Admin_Code)

sum(MASE$mase_annual_median>1)
sum(MASE$mase_biweek_mean>1)
sum(MASE$mase_biweek_median>1)
qplot(mean_count, mase, data=MASE) + coord_trans(x="log2") +geom_vline(xintercept=10)
qplot(Population, mase, data=MASE) + coord_trans(x="log10") +geom_smooth()

## plot MASE
m_mase <- melt(MASE, measure.vars=c("mase_annual_mean", "mase_annual_median", 
                                    "mase_biweek_mean", "mase_biweek_median"))
m_mase$Province <- reorder(m_mase$Province, m_mase$Population)
m_mase$metric <- reorder(m_mase$variable, m_mase$value, FUN=function(x) -mean(x))
ggplot(m_mase) + geom_raster(aes(x=metric, y=Province, fill=value)) +
        scale_fill_gradient2(low="green", mid="yellow", high="red", midpoint=1) +
        facet_grid(MOPH_Admin_Code~., scales="free", space="free") +
        theme_bw()

############################
## evaluate our forecasts ##
############################

## calculate set-specific MAE, i.e. within one set of forecasts


## calculate step-specific MAE, i.e. across forecasts, for each step into the future


