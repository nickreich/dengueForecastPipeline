shinyServer(function(input, output, session) {
 ## update input$date when actionButton 'back' is clicked
 observe({
  ## only run when 'back' is clicked
  input$back
  
  ## use isolate so that action is only run one time
  isolate({
   ## if unclicked, do not change weeks
   if(input$back==0)
    return()
   ## find currently set date
   old.date <- which(names(table(forecasts$date)) == input$date)
   ## go to previous biweek, unless at first biweek, in which case stay there
   new.date <- ifelse(old.date-1<1, 1, old.date-1)
   updateSelectInput(session, "date", selected = names(table(forecasts$date))[new.date])
   
  })
 })
 
 ## update input$date when actionButton 'forward' is clicked
 observe({
  input$forward
  ## use isolate so that action is only run one time
  isolate({
   ## if unclicked, do not change weeks
   if(input$forward==0)
    return()
   ## find currently set date
   old.date <- which(names(table(forecasts$date)) == input$date)
   ## go to next biweek, unless at last biweek, in which case stay there
   num_forecasts <- length(unique(forecasts$date))
   new.date <- ifelse(old.date+1>num_forecasts, num_forecasts, old.date+1)
   updateSelectInput(session, "date", selected = names(table(forecasts$date))[new.date])
   
  })
 })
 
 
 ## create the plot of the data
 output$plot <- reactive({
  moph <- input$moph
  
  if(moph == "all")
   moph <- seq(0, 12)
  
  ## format counts into plot format 
  plot_counts <- merge(counts, thai_prov_data, by="province", all.x=T) %>%
   filter(disease == 26, date_sick_year >= input$start, MOPH_Admin_Code %in% moph) %>%
   group_by(date_sick_biweek, date_sick_year) %>%
   summarise(count = round(sum(count)))
  
  ## add date variable
  plot_counts$date <- biweek_to_date(plot_counts$date_sick_biweek, 
                                     plot_counts$date_sick_year)
  
  plot_forecasts <- merge(forecasts, thai_prov_data, by.x="pid", by.y="FIPS", all.x=T) %>%
   filter(MOPH_Admin_Code %in% moph) %>%
   group_by(biweek, year) %>%
   summarise(predicted_count = round(sum(predicted_count)),
             ub = round(sum(ub)),
             lb = round(sum(lb)))
  plot_forecasts$unseen <- plot_forecasts$lb
  
  plot_forecasts$date <- biweek_to_date(plot_forecasts$biweek, plot_forecasts$year)
  
  plot_df <- merge(plot_counts, plot_forecasts, by = "date", all=T)[,c("date", "count", "predicted_count", "ub", "lb", "unseen")]
  
  plot_df$date <- as.Date(plot_df$date)
  
#   plot_df$unused <- ifelse(is.na(plot_df$predicted_count), NA, plot_df$count)
  
  plot_df$count <- ifelse(is.na(plot_df$predicted_count), plot_df$count, NA)
  
  plot_df <- select(plot_df, date, count, #unused, 
                    predicted_count, ub, unseen, lb)
  
  colnames(plot_df) <- c("Date", "Observed Cases",# "Incomplete Recent Cases",
                         "Forecasted Cases", "Prediction interval", "Unseen", 
                         "CI Lower Bound")
  
  ## this outputs the google data to be used in the UI to create the dataframe
  list(
   data=googleDataTable(plot_df),
   options = list(
    vAxis = list(
     viewWindow = list(max = max(plot_counts$count, plot_forecasts$predicted_count)*1.1)
    )
   ))
 })
 
 # create the map of the data
 output$map <- reactive({
  map_forecasts <- merge(forecasts, thai_prov_data, by.x = "pid", by.y = "FIPS", all.x=T) %>%
   filter(date == as.Date(input$date))
  
  map_forecasts$cpp <- round(100000*map_forecasts$predicted_count/map_forecasts$Population)
  
  map_forecasts$pname <- ifelse(map_forecasts$pname == "Bangkok Metropolis", "Bangkok",
                                as.character(map_forecasts$pname))
  
  map_df <- data.frame(map_forecasts$pname, map_forecasts[,input$var])
  colnames(map_df)[1] <- "Province"
  colnames(map_df)[2] <- ifelse(input$var == "cpp", 
                                "Cases per 100,000 Population",
                                "Outbreak Probability (%)")
  
  
  
  list(data=googleDataTable(map_df), 
       options = list(
        colorAxis = list(
         maxValue = max(map_max[input$var=="cpp"],
                        100, na.rm=T),
         colors = cbbPalette[c(4[input$var=="outbreak_prob"], 5[input$var=="cpp"], 
                               9[input$var=="outbreak_prob"], 7)])#,
#         values = c(0, 14[input$var=="cpp"], 50[input$var=="outbreak_prob"], 
#                    100[input$var=="outbreak_prob"])
       ))
 })
 
 output$map_title <- renderUI({
  ifelse(input$var=="cpp", "Forecasted dengue cases per 100,000 inhabitants",
         "Probability of dengue outbreak occurrence")
 })
 
 output$plot_title <- renderUI({
  if(input$moph=="all")
   return("Observed and forecasted cases of dengue fever in Thailand")
  if(input$moph==0)
   return("Observed and forecasted cases of dengue fever in Bangkok")
  return(paste("Observed and forecasted cases of dengue fever in MOPH Region", input$moph))
 })
 
})