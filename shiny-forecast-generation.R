shiny_forecasts <- biweek_eval_data$formatted_forecasts %>%
 filter(delivery_date == max(delivery_date)) %>%
 group_by(pid, year, biweek) %>%
 summarise(predicted_count = ceiling(median(pred_count)),
           lb = ceiling(quantile(pred_count, probs = .025)),
           ub = ceiling(quantile(pred_count, probs = .975)),
           outbreak_prob = mean(pred_outbreak))

write.csv(shiny_forecasts, "20150424_forecast_20150501.csv")
