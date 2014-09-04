dengueForecastAnalyses
======================

Code for running forecasts of dengue fever outbreaks in Thailand.

The vision for the structure of this directory is that there are be subdirectories called code, data, reports, and forecasts. Also in the main directory, there would be a file called makeForecasts.R. Within that script, there are a handful of variables that need to be manually specified in order to generate a set of forecasts. These variables include locations of other repositories on the local machine, the number of cores the machine has, and the DATES that are to be used to create the forecast. 

The makeForecast.R script creates forecasts (for now, based on "spamd"-style general additive models with seasonality and lag terms from top correlated provinces). Forecasts are generated and dumped in a standardized format into the forecasts folder. Reports would also be generated (if desired) for collaborators.

## Dates
Four dates define a forecast
* FROM_DATE : the date from which all case data is retrieved from the database (open on the left).
* TO_DATE : the date through which all case data is retrieved from the database (closed on the right).
* DELIVERY_DATE : the date by which all queried cases must have been "delivered" to the research team. Any cases delivered after this date will not be included in the data pull.
* ANALYSIS_DATE : The date on which the current analysis is being conducted.

## Repositories
This code assumes that the dengueForecastAnalyses repository shares a root directory with the [cruftery repository](https://github.com/sakrejda/cruftery).   Additionally, the user must specify the location of the spamd SVN repository.

The makeForecast.R file completes the following sequence of operations
* set local options for file dependencies, number of computing cores, etc...
* read data from the zaraza database
* source the spamd modeling code
* read in spatial info about Thailand
* define locations for which forecasts will be created
* create a den.data object
* run a smooth on the data, then the forecast, using built-in spamd functions
* [generate the report]

Each forecast txt file will have a table with the following columns present:
* pid: provice FIPS id
* pname: province name
* numid : a numeric ID of the province
* biweek: biweek of the predicted data
* year: year of predicted data
* predicted_count: count (the forecast itself)
* model: a title for the model used to create the forecasts
* from_date: early end of case data
* to_date: last date of case data
* delivery_date: date by which all case data had to have been received by
* analysis_date: date the analysis was conducted
* analysis_biweek: biweek of the analysis date
* repo1_name: name of the first repository dependency
* repo1_hash: hash of the current version of repo1
* repo2_name: name of the second repository dependency
* repo2_hash: hash of the current version of repo2
* repo3_name: name of the third repository dependency
* repo3_hash: hash of the current version of repo3
* lb: lower bound of 95% prediction interval for forecast count
* ub: upper bound of 95% prediction interval for forecast count
* outbreak_prob: Predicted probability of an outbreak
