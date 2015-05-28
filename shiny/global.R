library(dplyr)
library(googleCharts)
library(dengueThailand)
library(spatialpred)
library(ggplot2)

## load most recent counts
counts_file <- list.files(pattern = "counts")
counts_info <- file.info(counts_file)
recent_counts <- which.max(counts_info$ctime)
counts <- read.csv(counts_file[recent_counts])

counts$date <- biweek_to_date(counts$date_sick_biweek,
                              counts$date_sick_year)

## load most recent forecasts
forecasts_file <- list.files(pattern = "forecast")
forecasts_info <- file.info(forecasts_file)
recent_forecasts <- which.max(forecasts_info$ctime)
forecasts <- read.csv(forecasts_file[recent_forecasts])
forecasts$date <- biweek_to_date(forecasts$date_sick_biweek,
                                 forecasts$date_sick_year)
forecasts$outbreak_prob <- round(forecasts$outbreak_prob)

## load thai population data
data(thai_prov_data)

## merge sheets
mapMaxdf <- merge(forecasts, thai_prov_data, by.x = "pid", by.y = "FIPS", all.x=T)

## calculate predicted cases per 100,000 population
mapMaxdf$cpp <- round(100000*mapMaxdf$predicted_count/mapMaxdf$Population,2)

map_max <- max(ceiling(mapMaxdf$cpp), na.rm=T)
map_min <- min(mapMaxdf$cpp, na.rm=T)

## set graph colors (special for colorblind people)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#cccccc")

Thai_translations <- read.csv("~/Documents/dengueForecastPipeline/shiny/shiny-Thai-translations.csv", header = TRUE, stringsAsFactors = F)

Thai_translations$Thai <- c("การพยากรณ์โรคไข้เลือดออก ประเทศไทย",
                            "การพยากรณ์โรคไข้เลือดออกฉบับนี้เป็นฉบับร่าง ยังอยู่ระหว่างการตรวจสอบความถูกต้องแบบจำลองทางคณิตศาสตร์",
                            "เลือกวันที่",
                            "สองสัปดาห์ก่อนหน้า",
                            "สองสัปดาห์ต่อไป",
                            "คัดเลือกตัวแปล",
                            "ความน่าจะเป็นในการเกิดการระบาด (ร้อยละ)",
                            "คาดการณ์อัตราการเกิดโรค",
                            "คัดเลือกเขตบริการสุขภาพ",
                            "ประเทศไทย",
                            "กรุงเทพมหานคร",
                            "เลือกปีที่เริ่มต้น",
                            "แผนที่",
                            "กราฟอนุกรมเวลา",
                            "สร้างโดย:",
                            "การพยากรณ์นี้ยังเป็นฉบับร่าง คลิกเพื่อดำเนินการต่อ",
                            "จำนวนผู้ป่วยรายสองสัปดาห์",
                            "วันที่",
                            "จำนวนผู้ป่วยที่พบบ",
                            "จำนวนผู้ป่วยจากการพยากรณ์",
                            "ช่วงของการพยากรณ์",
                            "จำนวนผู้ป่วยต่อประชากรแสนคน",
                            "ค่าการพยากรณ์ผู้ป่วย",
                            "จำนวนผู้ป่วยจากการพยากรณ์ต่อประชากรแสนคน",
                            "ความน่าจะเป็นในการเกิดการระบาดของโรคไข้เลือดออก",
                            "ผู้ป่วยไข้เลือดออกที่พบและผู้ป่วยจากการพยกรณ์ในประเทศไทย",
                            "ผู้ป่วยไข้เลือดออกที่พบและผู้ป่วยจากการพยกรณ์ในกรุงเทพมหานคร",
                            "ผู้ป่วยไข้เลือดออกที่พบและผู้ป่วยจากการพยกรณ์ในเขตบริการสุขภาพที่ 4")