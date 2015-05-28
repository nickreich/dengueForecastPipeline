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
      updateSelectInput(session, "date",
                        selected = names(table(forecasts$date))[new.date])
      
    })
  })
  
  
  ## create the plot of the data
  output$plot <- reactive({
    moph <- input$moph
    
    if(moph == "all")
      moph <- seq(0, 12)
    
    ## format counts into plot format 
    plot_counts <- merge(counts, thai_prov_data, by.x="pid", by.y="FIPS",
                         all.x=T) %>%
      filter(date_sick_year >= input$start,
             MOPH_Admin_Code %in% moph) %>%
      group_by(date_sick_biweek, date_sick_year) %>%
      summarise(count = round(sum(count)))
    
    ## add date variable
    plot_counts$date <- biweek_to_date(plot_counts$date_sick_biweek, 
                                       plot_counts$date_sick_year)
    
    plot_forecasts <- merge(forecasts, thai_prov_data, by.x="pid",
                            by.y="FIPS", all.x=T) %>%
      filter(MOPH_Admin_Code %in% moph) %>%
      group_by(date_sick_biweek, date_sick_year) %>%
      summarise(predicted_count = round(sum(predicted_count)),
                ub = round(sum(ub)),
                lb = round(sum(lb)))
    plot_forecasts$unseen <- plot_forecasts$lb
    
    plot_forecasts$date <- biweek_to_date(plot_forecasts$date_sick_biweek,
                                          plot_forecasts$date_sick_year)
    
    plot_df <- merge(plot_counts, plot_forecasts, by = "date", all=T)[,c("date", "count", "predicted_count", "ub", "lb", "unseen")]
    
    plot_df$date <- as.Date(plot_df$date)
    
    #   plot_df$unused <- ifelse(is.na(plot_df$predicted_count), NA, plot_df$count)
    
    plot_df$count <- ifelse(is.na(plot_df$predicted_count), plot_df$count, NA)
    
    plot_df <- select(plot_df, date, count, #unused, 
                      predicted_count, ub, unseen, lb)
    
    colnames(plot_df) <- c("Date", "Observed cases",# "Incomplete Recent Cases",
                           "Forecasted cases", "Prediction interval", "Unseen", 
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
    # browser()
    map_forecasts <- merge(forecasts, thai_prov_data, by.x = "pid",
                           by.y = "FIPS", all.x=T) %>%
      filter(date == as.Date(input$date))
    
    map_forecasts$cpp <- round(100000*map_forecasts$predicted_count/map_forecasts$Population, 2)
    #   if(input$var == "cpp")
    #     map_forecasts$color <- round(log10(map_forecasts$cpp),1)
    #   if(input$var == "outbreak_prob")
    #     map_forecasts$color <- map_forecasts$outbreak_prob
    
    map_forecasts$Province <- ifelse(map_forecasts$Province == "Bangkok Metropolis",
                                     "Bangkok",
                                     as.character(map_forecasts$Province))
    
    map_df <- map_forecasts[,c("Province", input$var, 
                               # "cpp"[input$var == "outbreak_prob"],
                               "outbreak_prob"[input$var=="cpp"],
                               "predicted_count"[input$var=="outbreak_prob"])]
    
    colnames(map_df)[2] <- ifelse(input$var == "cpp", 
                                  "Cases per 100,000 population",
                                  "Outbreak probability (%)")
    
    colnames(map_df)[3] <- ifelse(input$var == "outbreak_prob", 
                                  "Predicted number of cases",
                                  "Outbreak Probability (%)")
    #   
    list(data=googleDataTable(map_df), 
         options = list(
           colorAxis = list(
             # minValue = min(0[input$var=="outbreak_prob"], map_min[input$var=="cpp"], na.rm=TRUE),
             maxValue = max(map_max[input$var=="cpp"],
                            100[input$var=="outbreak_prob"], na.rm=T),
             colors = c("#053061"[input$var=="outbreak_prob"],
                        "#CCCCCC"[input$var=="outbreak_prob"],
                        "#FF2C19"[input$var=="outbreak_prob"],
                        cbbPalette[c(5[input$var=="cpp"],
                                     7[input$var=="cpp"])]))#,
           # values = c(0, 14[input$var=="cpp"], 50[input$var=="outbreak_prob"], 
           # 100[input$var=="outbreak_prob"])
         ))
  })
  
  output$map_title <- renderUI({
    ifelse(input$var=="cpp", "Forecasted dengue cases per 100,000 inhabitants",
           "Probability of dengue outbreak occurrence (%)")
  })
  
  output$plot_title <- renderUI({
    if(input$moph=="all")
      return("Observed and forecasted cases of dengue fever in Thailand")
    if(input$moph==0)
      return("Observed and forecasted cases of dengue fever in Bangkok")
    return(paste("Observed and forecasted cases of dengue fever in MOPH Region", input$moph))
  })
  
  output$legend <- renderPlot({
    if(input$var == 'outbreak_prob'){
      
      paint.brush = colorRampPalette(c("#053061", "#CCCCCC", "#FF2C19"))
      cols <- paint.brush(101)
      leg_dat <- data_frame(y = seq(0, 100), x = 1, col = cols)
      
      p <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = col, x = x), show_guide = FALSE) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
      
    }
    if(input$var == 'cpp'){
      paint.brush = colorRampPalette(cbbPalette[c(5, 7)])
      cols <- paint.brush(length(seq(0, map_max)))
      leg_dat <- data_frame(y = seq(0, map_max), x = 1, col = cols)
      
      p <- ggplot(data = leg_dat) +
        geom_tile(aes(y = y, fill = reorder(col, y), x = x), show_guide = FALSE) +
        scale_y_continuous(limits = c(0, map_max), breaks = seq(0, map_max, length.out = 5)) +
        scale_fill_manual(values = leg_dat$col) + theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.x = element_blank(),
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
    }
    
    return(p)
    
  })
  
  output$text1 <- renderUI({
    if(input$language == "english")
      return(Thai_translations[,1])
    if(input$language == "thai")
      return(Thai_translations[,2])
  })
  
})