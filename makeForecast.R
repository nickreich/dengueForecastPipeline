## master file for running forecasts of dengue in Thailand
## Nicholas Reich, Stephen Lauer
## April 2015

## for shell script
options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

if(length(args)==0) {
 args <- Sys.Date()
} else {
 args <- as.Date(args)
}

#######################
## SET LOCAL OPTIONS ## 
#######################

## set dates
FROM_DATE <- as.Date('1968-01-01')
DELIVERY_DATE <- as.Date(args[1])
to_date_lag <- 4 # in biweeks
run_full_year <- TRUE
steps_ahead <- 30
show_seasonality <- FALSE
bad_prov_removal <- TRUE

## modeling globals
MODEL <- 'spamd_tops3_lag1'

## define machine-specific properties/folders
## Nick
CORES <- 18
root_dir <- '~/Documents/code_versioned/denguePrediction/' ## parent dir for dengueForecastPipeline repo
spamd_dir <- '~/Documents/code_versioned/spamd/'
pgsql <- '~/credentials/sql_zaraza.rds'

## Steve
# CORES <- 2 
# root_dir <- '~/Documents/' ## parent dir for dengueForecastPipeline repo
# spamd_dir <- '~/Documents/denguemodeling/spamd/'
# pgsql <- '~/Documents/credentials/sql_zaraza.rds'

#######################
## USE LOCAL OPTIONS ## 
#######################

## additional date manipulation
DELIVERY_DATE_STRING <- format(DELIVERY_DATE, "%Y%m%d") ## for use in filenames
ANALYSIS_DATE <- Sys.Date()

## set number of computing cores
options(mc.cores=CORES)

## main repo
setwd(file.path(root_dir, 'dengueForecastPipeline'))
dFP_github_hash <- system("git rev-parse HEAD | cut -c1-10", intern=TRUE)

## folder with thai administrative data
path_to_census_file <- '../dengue_data/peripheral_data/2010Census.csv'

## folder where data will be stored
aggregated_data_dir <- file.path(root_dir, 
                                 'dengueForecastPipeline',
                                 'data') 

## load packages
library(lubridate)
library(parallel)
library(RPostgreSQL)
library(reshape2)
library(dplyr)
library(httr)
# library(devtools)

## load and store version for cruftery
response <- GET("https://api.github.com/repos/sakrejda/cruftery/git/refs/heads/master")
cruftery_github_hash <- content(response)[['object']][['sha']]

# install_github(rep='sakrejda/cruftery/package_dir', ref=cruftery_github_hash)
# library(cruftery)
library(spatialpred)
library(dengueThailand)

#######################
## pull data from DB ##
#######################

## set TO_DATE = t_{k-l}
delivery_biweek <- date_to_biweek(DELIVERY_DATE)
TO_DATE <- biweek_to_date(delivery_biweek - to_date_lag, 
                          year(DELIVERY_DATE))
to_biweek <- date_to_biweek(TO_DATE)
to_year <- year(TO_DATE)

## set steps ahead if running full year forecasts
if(run_full_year) {
 steps_ahead <- 26 - to_biweek + 1       
}


## Load most recent counts data
counts.list <- list.files(path="counts", pattern = "*.csv")
counts.info <- file.info(paste0("counts/", counts.list))
recent.count <- which.max(counts.info$mtime)
counts <- read.csv(paste0("counts/", counts.list[recent.count]))

# ggplot(counts) + geom_raster(aes(x=year+biweek/26, y=province, fill=count)) + facet_wrap(~disease, ncol=1, scales="free_x")

## subset to onset dates < TO_DATE
to_time <- to_year + (to_biweek-1)/26
counts_subset <- counts %>%
 filter( (date_sick_year+(date_sick_biweek-1)/26) < to_time) %>%
 group_by(disease, date_sick_year, date_sick_biweek, province) %>%
 summarize(count=sum(count))

## put into wide format, save all objects needed for prediction to aggregated_data
pred_objects <- create_standard_wide_format(counts_subset, keep_codes=26, 
                                            path_to_census=path_to_census_file)

fname <- paste0("counts_through_", DELIVERY_DATE_STRING, ".RData")
save(pred_objects, file=file.path(aggregated_data_dir, fname))

count_matrix <- pred_objects$count_matrix


####################################
## source the spamd modeling code ##
####################################

setwd(spamd_dir)
spamd_version <- system('svn info |grep Revision: |cut -c11-', intern=TRUE)
# source("trunk/source/dengpred/R/Utility.r")
# source.deng.pred("trunk/source/dengpred/R/")
# source("trunk/manuscripts/realTimeForecasting/code/spatialPlotting.R")
## load(file.path(aggregated_data_dir, fname)) ## only needed if starting from this point

#############################
## find, set province info ##
#############################

load("trunk/manuscripts/realTimeForecasting/predictions/THA_adm1.RData") ## loads object called gadm

## define locations for which forecasts will be created
pnames <- as.character(pred_objects$province_names)

## insert NAs until first non-zero occurrence for province and its children: 
## Nong Bua Lam Phu, Amnat Charoen, Sa Kaeo, Mukdahan, Yasothon, 
## Phayao, Udon Thani, Ubon Ratchathani, Prachin Buri, and Chiang Rai
if(bad_prov_removal == TRUE){
 idx_NBL <- which(pnames=="Nong Bua Lam Phu")
 
 NBL_first_count <- which(count_matrix[idx_NBL,] != 0)[1]
 count_matrix[idx_NBL,1:(NBL_first_count-1)] <- NA
 
 idx_AC <- which(pnames=="Amnat Charoen")
 
 AC_first_count <- which(count_matrix[idx_AC,] != 0)[1]
 count_matrix[idx_AC,1:(AC_first_count-1)] <- NA
 
 idx_SK <- which(pnames=="Sa Kaeo")
 
 SK_first_count <- which(count_matrix[idx_SK,] != 0)[1]
 count_matrix[idx_SK,1:(SK_first_count-1)] <- NA
 
 idx_M <- which(pnames=="Mukdahan")
 
 M_first_count <- which(count_matrix[idx_M,] != 0)[1]
 count_matrix[idx_M,1:(M_first_count-1)] <- NA
 
 idx_Y <- which(pnames=="Yasothon")
 
 Y_first_count <- which(count_matrix[idx_Y,] != 0)[1]
 count_matrix[idx_Y,1:(Y_first_count-1)] <- NA
 
 idx_P <- which(pnames=="Phayao")
 
 P_first_count <- which(count_matrix[idx_P,] != 0)[1]
 count_matrix[idx_P,1:(P_first_count-1)] <- NA
 
 idx_UT <- which(pnames=="Udon Thani")
 
 UT_first_count <- max(M_first_count, NBL_first_count)
 count_matrix[idx_UT,1:(UT_first_count-1)] <- NA
 
 idx_UR <- which(pnames=="Ubon Ratchathani")
 
 UR_first_count <- max(Y_first_count, AC_first_count)
 count_matrix[idx_UR,1:(UR_first_count-1)] <- NA
 
 idx_PB <- which(pnames=="Prachin Buri")
 
 PB_first_count <- SK_first_count
 count_matrix[idx_PB,1:(PB_first_count-1)] <- NA
 
 idx_CR <- which(pnames=="Chiang Rai")
 
 CR_first_count <- P_first_count
 count_matrix[idx_CR,1:(CR_first_count-1)] <- NA
}

## merging Nong Khai and Bueng Kan
idx_NK <- which(pnames=="Nong Khai")
idx_BK <- which(pnames=="Bueng Kan")

count_matrix[idx_BK,][is.na(count_matrix[idx_BK,])] <- 0

count_matrix[idx_NK,] <- count_matrix[idx_NK,] + count_matrix[idx_BK,]
count_matrix <- count_matrix[-idx_BK,]
fips <- pred_objects$fips[-idx_BK]
pnames <- pnames[-idx_BK]
pop <- pred_objects$pop[-idx_BK]

##############################
## create a den.data object ##
##############################

## matching FIPS into the spatial data frame
gadm@data$FIPS_ADMIN <- as.character(fips[match(gadm@data$NAME_1, pnames)])
dat <- new.cntry.data(case.counts = count_matrix,
                      time.info = pred_objects$time_matrix,
                      fips = fips,
                      names = pnames,
                      pop.data = pop,
                      loc.info = gadm)


#####################
## smooth the data ##
#####################
den_smooth <- smooth.cdata(dat)


###################
## run forecasts ##
###################

## chosen 3 tops and lag 1 based on plots of MASE across all provinces from 
##    casePredictionStepsFwd.R
##    predictionPerformance_09132013a.rda
den_mdl <- fit.cntry.pred.mdl(den_smooth, num.tops=3, cor.lags=1)

if(show_seasonality == TRUE){
 season_data <- matrix(0, nrow = 76, 
                       ncol = den_mdl@loc.mdls[[1]]@num.tops + 
                        length(den_mdl@loc.mdls[[1]]@mdl[[1]]) + 1)
 season_data[,1] <- seq(1, 76, 1)
 for(i in 1:length(den_mdl@loc.mdls)){
  season_data[i,c(2:dim(season_data)[2])] <- c(den_mdl@loc.mdls[[i]]@tops,
                                               den_mdl@loc.mdls[[i]]@mdl[[1]])
 }
 season_data <- as.data.frame(season_data)
 colnames(season_data) <- c("Province", 
                            paste0("top", seq(1,den_mdl@loc.mdls[[1]]@num.tops, 1)),
                            names(den_mdl@loc.mdls[[1]]@mdl[[1]]))
 write.csv(season_data, file.path(aggregated_data_dir, paste0("season_data", DELIVERY_DATE_STRING, ".csv")))
 
}

den_forecast <- forecast(den_mdl, dat, steps=steps_ahead, stochastic=T, verbose=T, MC.sims=1000, predictions.only=T, num.cores=CORES)

########################
## save forecast data ##
########################

## move forecast data to long format
forecast_data <- den_forecast@.Data
colnames(forecast_data) <- paste(den_forecast@yr, formatC(den_forecast@time.in.yr, width=2, flag="0"))
forecast_data <- data.frame(forecast_data)
forecast_data$pid <- den_forecast@loc.info@data$FIPS_ADMIN
forecast_data$pname <- rownames(forecast_data)
forecast_data$numid <- 1:(den_forecast@n.locs)
forecasts <- tbl_df(melt(forecast_data, id.vars = c("pid", "pname", "numid")))

## add dates, round counts, drop unneeded columns
forecasts <- 
 forecasts %>% 
 mutate(biweek = as.numeric(substr(variable, 7, 8)),
        year = as.numeric(substr(variable, 2, 5)),
        predicted_count = round(value),
        model = MODEL,
        from_date = FROM_DATE,
        to_date = TO_DATE,
        delivery_date = DELIVERY_DATE,
        analysis_date = ANALYSIS_DATE,
        analysis_biweek = date_to_biweek(ANALYSIS_DATE),
        repo1_name = "dengueForecastPipeline-github",
        repo1_hash = dFP_github_hash,
        repo2_name = "cruftery-github",
        repo2_hash = cruftery_github_hash,
        repo3_name = "spamd-springloops",
        repo3_hash = spamd_version) %>%
 select(-variable, -value)

## add prediction intervals
forecasts$lb <- forecasts$ub <- NA

for (i in 1:(den_forecast@n.locs)){
 idx <- which(forecasts$numid == i)
 forecasts[idx,c("lb", "ub")] <- predint(den_forecast, i, 0.95)
}

## get outbreak probabilities
outbreak_prob <- tbl_df(data.frame(get.outbreak.probability(den_forecast, den_smooth, thresh.type="thai")))
colnames(outbreak_prob) <- paste(den_forecast@yr, formatC(den_forecast@time.in.yr, width=2, flag="0"))
outbreak_prob$pid <- den_forecast@loc.info@data$FIPS_ADMIN
melted_outbreak_prob <- tbl_df(melt(outbreak_prob, id.vars = c("pid")))
melted_outbreak_prob <- 
 melted_outbreak_prob %>%
 mutate(biweek = as.numeric(substr(variable, 6, 8)),
        year = as.numeric(substr(variable, 1, 4)),
        outbreak_prob = value) %>%
 select(-variable, -value)

## join forecasts and outbreak probabilities
forecasts <- left_join(forecasts, melted_outbreak_prob)

## save the forecasts
forecast_file <- paste0(format(Sys.Date(), "%Y%m%d"), 
                        '_forecast_', 
                        DELIVERY_DATE_STRING, 
                        '.csv')

write.csv(forecasts, file=file.path(root_dir, 
                                    'dengueForecastPipeline',
                                    'forecasts',
                                    forecast_file))

## save the original cntry.data object
data_file <- paste0(format(Sys.Date(), "%Y%m%d"), 
                    '_cntrydata_', 
                    DELIVERY_DATE_STRING, 
                    '.rda')
save(dat, file=file.path(root_dir, 
                         'dengueForecastPipeline',
                         'forecasts',
                         data_file))

## save the original counts
counts_file <- paste0(format(Sys.Date(), "%Y%m%d"), 
                      '_newcounts_', 
                      DELIVERY_DATE_STRING, 
                      '.csv')
write.csv(counts, file=file.path(root_dir, 
                                 'dengueForecastPipeline',
                                 'forecasts',
                                 counts_file), row.names=FALSE)

