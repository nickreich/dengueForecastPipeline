shinyUI(fluidPage(
  HTML('<style type="text/css">
     .action-button {
       -moz-box-shadow:inset 0px 1px 0px 0px #54a3f7;
       -webkit-box-shadow:inset 0px 1px 0px 0px #54a3f7;
       box-shadow:inset 0px 1px 0px 0px #54a3f7;
       background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #007dc1), color-stop(1, #0061a7));
       background:-moz-linear-gradient(top, #007dc1 5%, #0061a7 100%);
       background:-webkit-linear-gradient(top, #007dc1 5%, #0061a7 100%);
       background:-o-linear-gradient(top, #007dc1 5%, #0061a7 100%);
       background:-ms-linear-gradient(top, #007dc1 5%, #0061a7 100%);
       background:linear-gradient(to bottom, #007dc1 5%, #0061a7 100%);
       filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#007dc1", endColorstr="#0061a7",GradientType=0);
       background-color:#007dc1;
       -moz-border-radius:3px;
       -webkit-border-radius:3px;
       border-radius:3px;
       border:1px solid #124d77;
       display:inline-block;
       cursor:pointer;
       color:#ffffff;
       font-family:arial;
       font-size:16px;
       padding:12px 36px;
       text-decoration:none;
       text-shadow:0px 1px 0px #154682;
       }
       .action-button:hover {
       background:-webkit-gradient(linear, left top, left bottom, color-stop(0.05, #0061a7), color-stop(1, #007dc1));
       background:-moz-linear-gradient(top, #0061a7 5%, #007dc1 100%);
       background:-webkit-linear-gradient(top, #0061a7 5%, #007dc1 100%);
       background:-o-linear-gradient(top, #0061a7 5%, #007dc1 100%);
       background:-ms-linear-gradient(top, #0061a7 5%, #007dc1 100%);
       background:linear-gradient(to bottom, #0061a7 5%, #007dc1 100%);
       filter:progid:DXImageTransform.Microsoft.gradient(startColorstr="#0061a7", endColorstr="#007dc1",GradientType=0);
       background-color:#0061a7;
       }
       .action-button:active {
       position:relative;
       top:1px;
       }
       
       </style>'),
  ## this starts the googleCharts engine
  googleChartsInit(),
  ## create title
  h2(uiOutput("text1")),
  ## create sidebar
  sidebarLayout(
    sidebarPanel(
      strong(helpText("These forecasts should be considered preliminary drafts, pending model validation.")),
      
      tags$hr(),
      ## in map, allow for timespan selection
      conditionalPanel(
        condition="input.tabs == 'Map'",
        selectInput("date", "Select date", choices = names(table(forecasts$date))),
        actionButton("back", "Previous"), actionButton("forward", "Next"),
        tags$hr()
      ),
      
      conditionalPanel(
        condition="input.tabs == 'Map'",
        selectInput("var", "Select variable", 
                    choices = list("Outbreak probability (%)" = "outbreak_prob",
                                   "Predicted incidence rate" = "cpp"))),
      
      conditionalPanel(
        condition="input.tabs == 'Plot'",
        selectInput("moph", "Select MOPH region", multiple = FALSE,
                    choices = c(list("Thailand" = "all",
                                     "Bangkok" = 0), seq(1,12))
        )),
      
      conditionalPanel(
        condition="input.tabs == 'Plot'",
        selectInput("start", "Select start year", choices = seq(2014, 2000, -1), selected = 2013)
      ),
      
      selectInput("language", "", choices = c(list("English" = "english", "Thai" = "thai"))),
      
      tags$hr(),
      
      ## author line
      h6("Created by:"),
      h6("Stephen A Lauer"),
      h6("Krzysztof Sakrejda"),
      h6("Nicholas G Reich")
      
    ),
    
    ## create main panel
    mainPanel(
      ## create tabs
      tabsetPanel(
        ## plot map
        tabPanel("Map", ## make chart title here (otherwise not centered)
                 h4(uiOutput("map_title"), align="center"),
                 
                 ## make geochart
                 googleGeoChart("map", width="100%", height="475px", options = list(
                   
                   data = list("map"),
                   
                   region = "TH",
                   displayMode = "regions",
                   resolution = "provinces",
                   
                   ## set fonts
                   fontName = "Source Sans Pro",
                   fontSize = 14,
                   
                   ## set legend fonts
                   legend = "none",
                   #                 list(
                   #                textStyle = list(
                   #                 fontSize=14)),
                   
                   ## set chart area padding
                   chartArea = list(
                     top = 50, left = 75,
                     height = "75%", width = "70%"
                   ),
                   
                   # set colors
                   colorAxis = list(minValue = 0
                   ),
                   
                   
                   # set tooltip font size
                   tooltip = list(
                     textStyle = list(
                       fontSize = 14)
                   )
                 )),
                 
                 ## Add warning button to map area
                 conditionalPanel(
                   condition="input.action == 0",
                   absolutePanel(right = 400, top = 300, width = 200, class = "floater",
                                 actionButton("action", "These are draft predictions. Click to continue."))),
                 
                 absolutePanel(right = 0, top = 100, width = 100, class = "floater",
                               plotOutput("legend")),
        id="Map"),
        ## plot tab with google chart options
        tabPanel("Time Series",
                 ## make chart title here (otherwise not centered)
                 h4(uiOutput("plot_title"), align="center"),
                 ## make line chart
                 googleComboChart("plot", width="100%", height="475px", options = list(
                   
                   ## set fonts
                   fontName = "Source Sans Pro",
                   fontSize = 14,
                   
                   ## set axis titles, ticks, fonts, and ranges
                   hAxis = list(
                     #title = "",
                     #format = "####-##-##",
                     #                ticks = seq(1999, 2011, 2),
                     #                viewWindow = xlim,
                     textStyle = list(
                       fontSize = 14),
                     titleTextStyle = list(
                       fontSize = 16,
                       bold = TRUE,
                       italic = FALSE)
                   ),
                   vAxis = list(
                     title = "Number of cases per biweek",
                     textStyle = list(
                       fontSize = 14),
                     titleTextStyle = list(
                       fontSize = 16,
                       bold = TRUE,
                       italic = FALSE)
                   ),
                   
                   seriesType = "bars",
                   series = list(
                     "1" = list(
                       type = "line"),
                     #                "2" = list(
                     #                 type = "line"),
                     "2" = list(
                       type = "area"),
                     "3" = list(
                       type = "area", lineWidth=0, pointSize=0, areaOpacity=1, visibleInLegend=FALSE),
                     "4" = list(
                       type = "line", visibleInLegend=FALSE)
                   ),
                   isStacked = FALSE,
                   
                   ## set legend fonts
                   legend = list(
                     textStyle = list(
                       fontSize=14)),
                   
                   ## set chart area padding
                   chartArea = list(
                     top = 50, left = 75,
                     height = "75%", width = "65%"
                   ),
                   
                   #               # Allow pan/zoom
                   #               explorer = list(),
                   #               # Set bubble visual props
                   #               bubble = list(
                   #                opacity = 0.4, stroke = "none",
                   #                # Hide bubble label
                   #                textStyle = list(
                   #                 color = "none"
                   #                )
                   #               ),
                   
                   ## set colors
                   colors = c(cbbPalette[1], #"gray", 
                              cbbPalette[2:3], "white", cbbPalette[3]),
                   
                   ## set point size
                   pointSize = 3,
                   
                   # set tooltip font size
                   tooltip = list(
                     textStyle = list(
                       fontSize = 14)
                   )
                 )),
                 value="Plot"),
        id="tabs")
    )
  )
))