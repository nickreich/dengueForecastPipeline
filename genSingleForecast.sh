#!/bin/bash
cd ~/Documents/code_versioned/denguePrediction/dengueForecastPipeline/
Rscript getCounts.R "2014-11-17" > counts/`/bin/date "+%Y%m%d"`_counts.Rout
Rscript makeForecast.R "2014-11-17" > forecasts/`/bin/date "+%Y%m%d"`_forecast.Rout
