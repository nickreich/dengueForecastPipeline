#!/bin/bash
cd ~/Documents/code_versioned/denguePrediction/dengueForecastPipeline/
Rscript getCounts.R > counts/`/bin/date "+%Y%m%d"`_counts.Rout
Rscript makeForecast.R > forecasts/`/bin/date "+%Y%m%d"`_forecast.Rout