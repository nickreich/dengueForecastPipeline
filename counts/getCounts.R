## Load up most recent counts by delivery date 

## We probably don't need all of these packages for this
library(lubridate)
library(parallel)
library(RPostgreSQL)
library(reshape2)
library(dplyr)
library(httr)
library(devtools)

install_github(rep='sakrejda/cruftery/package_dir', ref=cruftery_github_hash)
library(cruftery)

pgsql <- '../credentials/sql_zaraza.rds'

link <- db_connector(pgsql)

counts <-  import_case_counts(
 source_table = 'unique_case_data',
 group_by = c('disease','date_sick','province', 'delivery_date'),
 from_timepoint = now() - years(100), ## FROM (open)
 to_timepoint = now(), ## TO (closed)
 delivery_timepoint = now(),
 aggregate_formula = NULL,
 link = link
)

## save the counts
counts_file <- paste0(format(Sys.Date(), "%Y%m%d"), '_counts.csv')

write.csv(counts, file=paste0("counts/", counts_file))
