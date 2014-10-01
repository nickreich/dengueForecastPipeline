## Load up most recent counts by delivery date 

options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)

if(length(args)==0)
 args <- Sys.Date()

## We probably don't need all of these packages for this
library(lubridate)
library(parallel)
library(RPostgreSQL)
library(reshape2)
library(dplyr)
library(httr)
library(devtools)

## load and store version for cruftery
response <- GET("https://api.github.com/repos/sakrejda/cruftery/git/refs/heads/master")
cruftery_github_hash <- content(response)[['object']][['sha']]

install_github(rep='sakrejda/cruftery/package_dir', ref=cruftery_github_hash)
library(cruftery)

# Steve pgsql
#pgsql <- '~/Documents/credentials/sql_zaraza.rds'

# Nick pgsql
 pgsql <- '~/credentials/sql_zaraza.rds'

## setup data pulls, ssh connection to zaraza needs to be established
link <- db_connector(pgsql)

## pull data and aggregate, must be connected to zaraza
new_counts <-  import_case_counts(
 source_table = 'unique_case_data',
 group_by = c('disease','date_sick','province', 'delivery_date'),
 from_timepoint = now() - years(100), ## FROM (open)
 to_timepoint = now(), ## TO (closed)
 delivery_timepoint = now(),
 aggregate_formula = NULL,
 link = link
)

new_counts$date_sick_biweek <- date_to_biweek(new_counts$date_sick)
new_counts$date_sick_year <- year(new_counts$date_sick)

old_counts <- import_old_case_counts(link=link)
old_counts$delivery_date <- NA
old_counts$date_sick <- NA

counts <- joint_old_new_cases(new_counts, old_counts)

## give everyone a delivery date
#idx_no_deliv_date <- which(is.na(counts$delivery_date))
#counts[idx_no_deliv_date,"delivery_date"] <- as.Date('2011-04-09', format="%Y-%m-%d")

## keep only disease == 26
counts <- filter(counts, disease==26)

## save the counts
counts_file <- paste0(format(args[1], "%Y%m%d"), '_counts.csv')

write.csv(counts, file=paste0("counts/", counts_file), row.names=FALSE)
