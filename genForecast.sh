#!/bin/bash
cd ~/Documents/dengueForecastPipeline/
Rscript getCounts.R > counts/`/bin/date "+%Y%m%d"`_counts.Rout
Rscript getDates.R "2014-01-01" > counts/`/bin/date "+%Y%m%d"`_dates.Rout
Rscript writeScript.R
sh doForecast.sh