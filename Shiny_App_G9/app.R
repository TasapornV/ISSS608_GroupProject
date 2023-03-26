

# LIBRARY -----------------------------------------------------------------
remotes::install_github("timelyportfolio/dataui")

packages = c('CGPfunctions', 'cluster', 'data.table', 'dataui','dendextend', 
             'devtools','DT', 'dplyr', 'dtwclust', 'earth', 'fable', 'feasts', 
             'forecast','fpc', 'geofacet', 'ggdendro', 'ggiraph', 'ggplot2', 
             'ggstatsplot', 'ggrepel', 'ggridges', 'gt', 'gtExtras', 'heatmaply', 
             'knitr','plotly', 'lubridate', 'psych', 'RColorBrewer', 'reactable', 
             'reactablefmtr', 'readr', 'sf', 'shiny', 'shinydashboard', 
             'shinythemes', 'shinyWidgets', 'stats', 'tibble', 'tidymodels', 
             'tidyquant', 'tidyr', 'tidyverse', 'timetk', 'tmap', 'treemap', 
             'TSclust', 'tseries', 'tsibble', 'tsibbletalk', 'zoo')

for(p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

# READ DATA ---------------------------------------------------------------
## Read compressed data file
T2.3 <- readRDS(file = "RDS/T2-3.rds") # Peak System Demand
T3.4 <- readRDS(file = "RDS/T3-4.rds") # Total Household Electricity Consumption by Dwelling Type
T3.5 <- readRDS(file = "RDS/T3-5.rds") # Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type
dwelling <- readRDS(file = "RDS/dwelling.rds")
town <- readRDS(file = "RDS/town.rds")

# reading the map file
mpsz <- st_read(dsn = 'master-plan-2014-subzone-boundary-web-shp',
                layer = 'MP14_SUBZONE_WEB_PL',
                crs = 3414)

# Import the area grid data.
area_grid <- read_csv("data/areagrid.csv")
singapore <- st_transform(mpsz, 4326)

# consumption <- T3.5
# consumption <- consumption %>%
#   mutate(kwh_per_acc = as.numeric(kwh_per_acc)) %>%
#   mutate(year = as.character(year)) %>%
#   mutate('date' = make_date(year=year, month=month))

# PARAMETER ---------------------------------------------------------------
years <- c("2022","2021", "2020", "2019", "2018", "2017")
regions <- c( "Central Region", "NorthEast Region", "East Region", "North Region", "West Region")
tables <- c("Peak System Demand" = "T2.3",
            "Total Household Electricity Consumption by Dwelling Type" = "T3.4",
            "Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type" = "T3.5")
type <- c("parametric", "nonparametric", "robust", "bayes")

introtext = "Singapore has progressively moved towards an open electricity market since 2001 
to ensure a reliable energy supply and promote effective competition in the energy market.
However, it is challenging to analyze the energy market and make informed decisions due to 
several variable components that could influence the market. First, the price of electricity 
in the wholesale market fluctuates depending on the electricity demand and supply, which are 
dependent on consumption patterns as well as gas industry respectively. About 95% of electricity 
is generated using natural gas. Next, there are also multiple parties (Electricity Market Authority, 
Power generation companies, Electricity Retailers, etc) involved in the electricity market.
To address this challenge, we build this RShinny app to provide relevant stakeholders with means 
to analyse and understand the data with applicable analytics models. Also, we want to help the 
users explore more information about the Singapore energy market easily through visualizations."

# UI ----------------------------------------------------------------------
ui = dashboardPage(
  dashboardHeader(title = 'Singapore Energy Consumption', titleWidth = 400),
  
  dashboardSidebar(width = 210,
                   sidebarMenu(
                     menuItem(" OVERVIEW",                tabName = 'overview',    icon = icon("chalkboard-user")),
                     menuItem(" CLUSTERING",              tabName = 'clustering',  icon = icon("circle-nodes")),
                     menuItem(" INFERENTIAL STATISTICS",  tabName = "inferential", icon = icon("magnifying-glass-chart")),
                     menuItem(" TIME SERIES FORECASTING", tabName = "time_series", icon = icon("chart-line")),
                     menuItem(" DATA",                    tabName = "data",        icon = icon("table")),
                     menuItem(" ABOUT",                   tabName = "about",       icon = icon("info"))
                   )
                   ),
  dashboardBody(
    tabItems(
      ## 1 OVERVIEW --------------------------------------------------------------
      tabItem(
        tabName = "overview",
        navbarPage( "OVERVIEW",
                   
                   ### 1.1 introduction --------------------------------------------
                   tabPanel("Introduction",introtext),
                   
                   ### 1.2 geofacet ------------------------------------------------
                   tabPanel("Consumption by Planning Area & Dwelling Type",
                            fluidPage(
                              radioButtons("axis", label = "select axis control",
                                           choices = c("fixed y-axis" = "fixed", 
                                                       # "free x-axis" = "free_x", 
                                                       "free y-axis" = "free_y"), 
                                           inline = T),
                              plotOutput("geo", height = 800)
                            )),
                   
                   ### 1.3 peak system demand --------------------------------------
                   tabPanel("Consumption by Dwelling type and Town",
                            fluidRow(
                              column(4, sliderInput("slider_year", "Select year",min = 2005,
                                          max = 2022, step = 1, round = FALSE,
                                          value =  c(2005, 2022))),
                              column(4, radioButtons("type", "Select parameter", choices = c("By town", "By dwelling type"), inline = TRUE)),
                              column(4, radioButtons("slope_value", "select value", choices = c("sum", "average", "median" ), inline = TRUE))
                              
                            ),
                            fluidRow(
                              column(6, plotlyOutput("dwelling", height = 400)),
                              column(6, tableOutput("sparktable"))
                            ),
                            fluidRow(
                              column(6, plotlyOutput("cycleplot")),
                              column(6, plotOutput("slope",height=400))
                            )
                   )
        )
      ),
      
      ## 2 CLUSTERING ----------------------------------------------------------------
      tabItem(
        tabName = "clustering",
        navbarPage("CLUSTERING",
                   ### 2.1 hierachical Clustering ----------------------------
                   tabPanel("Hierachical Clustering",
                            fluidPage(
                              fluidRow(
                                
                                column(3,pickerInput("method", "Choose Clustering Method",
                                                     choices = c("ward.D", "ward.D2", "single",
                                                                 "complete", "average", "mcquitty",
                                                                 "median", "centroid"),
                                                     selected = "complete")),
                                
                                column(3,pickerInput("distance", "Choose Distance Method",
                                                     choices = c("euclidian", "maximum", "manhattan",
                                                                 "canberra", "binary", "minkowski"))),
                                
                                column(3,numericInput("k", "Choose number of cluster",
                                                      min = 1, max = 10, value = 2))
                              ),
                              
                              fluidRow(
                                column(4, dataTableOutput("dendextend"),
                                       plotOutput("numberk", height = "350px")),
                                column(8, plotlyOutput("dendro", height = "400px"),
                                       tmapOutput("map"))
                              )
                            )
                   ),
                   
                   ### 2.2 Time Series Clustering ----------------------------------
                   tabPanel("Time Series Clustering",
                            fluidPage(
                              fluidRow(
                                
                                column(3,pickerInput("method2", "Choose Clustering Method",
                                                     choices = c("ward.D", "ward.D2", "single",
                                                                 "complete", "average", "mcquitty",
                                                                 "median", "centroid"),
                                                     selected = "complete")),
                                
                                column(3,numericInput("k2", "Choose number of cluster",
                                                      min = 1, max = 10, value = 2))
                              ),
                              plotlyOutput("dtw", height =500)
                            ))
        )
      ),
      
      ## 3 INFERENTIAL STATISTICS ------------------------------------------------
      tabItem(tabName = "inferential",
              navbarPage("INFERENTIAL STATISTICS", 
                         
                         ### 3.1 anova ---------------------------------------------
                         tabPanel("ANOVA",
                                  fluidPage(
                                    fluidRow(
                                      column(5,
                                             pickerInput(inputId = "anovainput", label = "Select Parameter",
                                                         choices = c("dwelling_type", "Region", "year"), selected = "dwelling_type",
                                                         options = list(`actions-box` = TRUE), multiple = F),
                                             verbatimTextOutput("anovastat")
                                      ),
                                      column(7, plotOutput("dwellingstat"))
                                    ),

                                    fluidRow(
                                      column(5,
                                             pickerInput(inputId = "region", label = "Select Region",
                                                         choices = regions, selected = "Central Region",
                                                         options = list(`actions-box` = TRUE), multiple = F),
                                             verbatimTextOutput("anovastat2")
                                      ),
                                      column(7, plotOutput("dwellingstat2"))
                                    )
                         )),

                         ### 3.2 correlation ---------------------------------------
                          tabPanel("Correlation Analysis",
                                   fluidPage(
                                     fluidRow(
                                       column(width = 9, plotlyOutput("correlation",height=400))
                                     )
                         ))
              )
      ),
      
      ## 4 TIME SERIES FORECASTING------------------------------------------------
      
      tabItem(tabName = "time_series",
              navbarPage("TIME SERIES FORECASTING",

                         ### 4.1 arima ---------------------------------------------
                         tabPanel("ARIMA",
                                  fluidPage(
                                    fluidRow(
                                      column(3,
                                             pickerInput(inputId = "timemodel", label = "Select Model",
                                                         choices = c("ARIMA", "ETS", "TSLM", "AR"),
                                                         selected = "ARIMA", multiple = F),
                                             numericInput("year", "Months to forecast ahead", min = 1, max = 24, step=1, value = 3)
                                             ),
                                      column(3,
                                             numericInput("arima_d", "Order of differencing", value=1),
                                             numericInput("arima_d2", "Order of seasonal differencing", value=2),
                                             checkboxInput("arima_d3", "Allow drift", value = FALSE)
                                             )
                                      ),
                                      # ,
                                      column(12, plotlyOutput("arima_plot")),
                                    #   ,
                                      column(5,
                                             verbatimTextOutput("arimatext")),
                                    # #   ,
                                    fluidRow(
                                      column(6, plotOutput("arima",height="500px"))
                                    #   column(width = 6, plotOutput("arima_plot",height=400))
                                      )
                                  )
                                  )
                         )
              
      ),
      
      ## DATA TABLE ------------------------------------------------------------
      tabItem(tabName = "data",
              navbarPage("DATA",
                         tabPanel("data table",
                                  fluidPage(
                                    fluidRow(
                                      column(8, wellPanel(
                                        pickerInput("SelectTable",
                                                    label = "Select Data",
                                                    choices = tables,
                                                    selected = "Electricity")
                                      ) ),
                                      
                                      column(12, dataTableOutput("table"))
                                    )
                                  ))
              )
      ), 
      ## ABOUT ------------------------------------------------------------
      tabItem(tabName = "about")
    ) #close tabItems
  ) #close dashboard body
) #close UI

# SERVER ------------------------------------------------------------------

server = function(input, output, session) {
  
  # geofacet ----------------------------------------------------------------
  geofacet <- town %>% 
        group_by(year, dwelling_type, Description)  %>%
        summarise(average_consumption = mean(kwh_per_acc, na.rm = TRUE))%>%
        ungroup()
  geofacet_gas_consump <- inner_join(geofacet, area_grid,
                                     by = c("Description" = "name"))
  # merge table with town name
  common_grid <- area_grid[area_grid$name %in% unique(geofacet$Description),]
  
  observeEvent(input$axis,{
    
    output$geo <- renderPlot ({
      ggplot(geofacet_gas_consump, aes(x = year, y = average_consumption)) +
        geom_line(aes(color = as.factor(dwelling_type))) +
        facet_geo(~Description, grid = common_grid, scales = input$axis) +
        labs(title = "Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type") +
        theme(plot.title = element_text(size=22),
              axis.text.x = element_text(size = 10, angle = 45),
              axis.text.y = element_text(size = 10),
              strip.text = element_text(size = 10),
              legend.position = "right")
    })
  })
  
  # table ----------------------------------------------------------------------
  observeEvent((input$SelectTable),{
    
    if(input$SelectTable == "T2.3")  {tabletext <- T2.3}
    if(input$SelectTable == "T2.6")  {tabletext <- T2.6}
    if(input$SelectTable == "T3.4")  {tabletext <- T3.4}
    if(input$SelectTable == "T3.5")  {tabletext <- T3.5}
    if(input$SelectTable == "T3.6")  {tabletext <- T3.6}
    if(input$SelectTable == "T3.7")  {tabletext <- T3.7}
    if(input$SelectTable == "T3.8")  {tabletext <- T3.8}
    if(input$SelectTable == "T3.9")  {tabletext <- T3.9}
    if(input$SelectTable == "T5.1")  {tabletext <- T5.1}
    if(input$SelectTable == "T5.2")  {tabletext <- T5.2}
    if(input$SelectTable == "T5.3")  {tabletext <- T5.3}
    if(input$SelectTable == "T5.4")  {tabletext <- T5.4}
    if(input$SelectTable == "T5.5")  {tabletext <- T5.5}
    output$table <- renderDataTable(tabletext)
  })
  
  # arima ----------------------------------------------------------------------
  # arima <- town
  # arima$Date <- yearmonth(as.yearmon(paste(arima$year, arima$month), "%Y %m"))
  # 
  # arima_tsbl  = as_tsibble(arima)
  # arima_ts <- ts(data=arima$kwh_per_acc, start = c(2005,1), end = c(2022, 6), frequency=12)
  # arima_arima = auto.arima(arima_ts, d =1, D =1, allowdrift = FALSE)
  # output$arima <- renderPlot(plot(forecast(arima_arima)))
  # 
  # full_arima = arima_tsbl %>%
  #   # filter(year==2017) %>%
  #   fill_gaps() %>%
  #   tidyr::fill(kwh_per_acc, .direction = "down")

  # a <- town
  # a$Date <- yearmonth(as.yearmon(paste(a$year, a$month), "%Y %m"))
  # a <- a %>% 
  #   group_by(Date) %>% 
  #   summarise(avgcon = mean(kwh_per_acc, na.rm = TRUE))
  # 
  # output$arima_plot <- renderPlot({
  #   a %>%
  #     gg_tsdisplay(difference(avgcon), plot_type='partial')
  # })
  
    # # Generate forecast
    # arima_forecast <- forecast(arima_model, h = 12)
    # 
    # # Create plot
    # arima_plot <- plot_ly() %>%
    #   add_lines(x = index(arima_ts), y = arima_ts, name = "Actual") %>%
    #   add_lines(x = index(arima_forecast$mean), y = arima_forecast$mean, name = "Forecast") %>%
    #   layout(title = "ARIMA Forecast",
    #          xaxis = list(title = "Year"),
    #          yaxis = list(title = "Average Consumption"))
    
    # Render plot
    # output$arima_plot <- renderPlotly(arima_plot)
    # fit <- ets(window(arima_ts))
    #   output$arima <- renderPlot(
    #     { plot(forecast(arima_arima)) }
    #   )
    
    # output$arimatext <- renderPrint(arima_arima)
    
    # arima_tsbl  = as_tsibble(arima)
    # full_arima = arima_tsbl %>%
    #   # filter(year==input$year) %>%
    #   fill_gaps() %>%
    #   tidyr::fill(avgcon, .direction = "down")
    # 
    # output$arima_plot <- renderPlot({
    #   full_arima %>%
    #     gg_tsdisplay(difference(avgcon), plot_type='partial')
    # })
  # })
  
  # ------------- peak system demand ------------- #
  sysdemand <- T2.3 %>%
    mutate(date = parse_date_time(paste0(year, "-", mth,"-1"),"ymd")) %>%
    mutate(monthyear = format(as.Date(date), "%b'%Y"))
  
  output$peakdemand <- renderPlotly({
    p_line <- sysdemand %>%
      mutate(text = paste(monthyear,
                          "<br>System Demand (NW):", peak_system_demand_mw)) %>%
      ggplot(aes(x = date, y = peak_system_demand_mw)) +
      geom_line() +
      geom_point() +
      labs(title = "Monthly Peak System Demand (MW)",
           x = "", y = "MW") +
      theme_tq() +
      #    scale_x_date(expand=c(0,0), date_breaks = "3 month", date_labels = "%b%y") +
      theme(legend.position="none")
    ggplotly(p_line, tooltip = "text")
  })
  
  # cycle plot -----------------------------------------------------------------

  sysdemand21 <- sysdemand %>%
    filter(year %in% c(2005:2021))
  
  #Computing year average by months
  hline.data <- sysdemand21 %>%
    group_by(mth) %>%
    summarise(avg_demand = mean(peak_system_demand_mw))
  
  #Allow user to choose monthly vs quarterly cycleplot
  p_cycleplot <-
    ggplot() +
    geom_line(data = sysdemand21,
              aes(x=year,y=peak_system_demand_mw, group=mth), colour = "black") +
    geom_hline(data = hline.data,
               aes(yintercept=avg_demand),
               linetype=6, 
               colour="red", 
               size=0.5) +
    facet_grid(~mth) +
    theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
    labs(title = "Cycleplot of Peak System Demand (MW), 2005 to 2021") +
    xlab("") +
    ylab("MW")
  
  output$cycleplot <- renderPlotly({
    p_cycleplot
  })
  
  # ----------------- consumption by dwelling type ------------------ #
  dwelling <- T3.4 %>%
    filter(year %in% c(2005:2022)) %>%
    filter(month %in% c(1:12)) %>%
    filter(DWELLING_TYPE %in% c('1-room / 2-room','3-room','4-room',
                                '5-room and Executive',
                                'Private Apartments and Condominiums',
                                'Landed Properties', 'Others')) %>%
    mutate(date = parse_date_time(paste0(year, "-", month,"-1"),"ymd")) %>%
    mutate(monthyear = format(as.Date(date), "%b'%Y"))
  
  #Add Private vs Public Classification
  dwelling$class <- case_when(
    dwelling$DWELLING_TYPE %in% c('Private Apartments and Condominiums',
                                  'Landed Properties', 'Others') ~ "Private",
    dwelling$DWELLING_TYPE %in% c('1-room / 2-room','3-room','4-room',
                                  '5-room and Executive') ~ "Public")
  p_line <- dwelling %>%
    mutate(text = paste(monthyear, 
                        "<br>Consumption (GWh):", consumption_GWh)) %>%
    ggplot(aes(x = date, y = consumption_GWh, colour = DWELLING_TYPE)) +
    #  facet_wrap(vars(class), ncol = 1) +
    geom_line() +
    geom_point() +
    labs(title = "Monthly Household Electricity Consumption (GWh) by Dwelling Type",
         x = "", y = "GWh") +
    theme_tq() +
    #    scale_x_date(expand=c(0,0), date_breaks = "3 month", date_labels = "%b%y") + 
    theme(legend.position="bottom")
  
  output$dwelling <- renderPlotly({ggplotly(p_line, tooltip = "text")})
  
  # ------------------------- dwelling stat ------------------------- #
  #Comparison of Median Consumption by Dwelling Type per year
  #Give user choice to choose the year
  givenyear <- 2021
  
  #plotting violin plot by dwelling type for private and public housing
  private <- ggbetweenstats(data = dwelling |> filter(class == "Private", year == givenyear), x = DWELLING_TYPE, y = consumption_GWh,
                            xlab = "Dwelling Type", ylab = "GWh",
                            type = "np", pairwise.comparisons = T, pairwise.display = "ns", 
                            mean.ci = T, p.adjust.method = "fdr",  conf.level = 0.95,
                            title = "Private",
                            package = "ggthemes", palette = "Tableau_10") +
    theme(axis.title.x = element_blank())
  
  public <- ggbetweenstats(data = dwelling |> filter(class == "Public", year == givenyear), x = DWELLING_TYPE, y = consumption_GWh,
                           xlab = "Dwelling Type", ylab = "GWh",
                           type = "np", pairwise.comparisons = T, pairwise.display = "ns", 
                           mean.ci = T, p.adjust.method = "fdr",  conf.level = 0.95,
                           title = "Public",
                           package = "ggthemes", palette = "Tableau_10") +
    theme(axis.title.x = element_blank())
  
  
  # table dwelling -------------------------------------------------------------
  #combining plots
  combine_plots(
    list(private, public),
    plotgrid.args = list(nrow = 5),
    annotation.args = list(
      title = "Comparison of Monthly Household Electricity Consumption by Dwelling Type",
      subtitle = paste0("Year",givenyear),
      theme = theme(
        plot.subtitle = element_text(size = 10),
        plot.title = element_text(size = 12))))
  
  # output$dwelling2 <-
  # dwelling %>%
  #   filter(year %in% c(startyear:endyear)) %>%
  #   group_by(DWELLING_TYPE) %>%
  #   summarise("Min" = min(consumption_GWh, na.rm = T),
  #             "Max" = max(consumption_GWh, na.rm = T),
  #             "Average" = mean(consumption_GWh, na.rm = T)
  #   ) %>%
  #   gt() %>%
  #   fmt_number(columns = 4,
  #              decimals = 2)
  
  
  # anova -----------------------------------------------------------------
  consumption <- T3.5
  consumption <- consumption %>% 
    mutate(kwh_per_acc = as.numeric(kwh_per_acc)) %>% 
    mutate(year = as.character(year))
  
  observeEvent(input$anovainput,{
    if(input$anovainput == "Region"){output$dwellingstat <- renderPlot({
      consumption %>%
        mutate(class = fct_reorder(Region, kwh_per_acc, .fun='mean')) %>%
        ggplot( aes(x=reorder(Region, kwh_per_acc), y=kwh_per_acc)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", color="red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        ggtitle("Boxplot of consumption per planning area")
    })
    }
    if(input$anovainput == "year"){output$dwellingstat <- renderPlot({
      consumption %>%
        mutate(class = fct_reorder(year, kwh_per_acc, .fun='mean')) %>%
        ggplot( aes(x=reorder(year, kwh_per_acc), y=kwh_per_acc)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", color="red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        ggtitle("Boxplot of consumption per planning area")
    })}
    if(input$anovainput == "dwelling_type"){output$dwellingstat <- renderPlot({
      consumption %>%
        mutate(class = fct_reorder(dwelling_type, kwh_per_acc, .fun='mean')) %>%
        ggplot( aes(x=reorder(dwelling_type, kwh_per_acc), y=kwh_per_acc)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", color="red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        ggtitle("Boxplot of consumption per planning area")
    })}
    if(input$anovainput == "dwelling_type"){
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ dwelling_type, data = consumption))
      })}
    if(input$anovainput == "Region"){
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ Region, data = consumption))
      })}
    if(input$anovainput == "year"){
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ year, data = consumption))
      })}
  })
  
  # anova2 ---------------------------------------------------------------------
  observeEvent(input$region,{
    output$dwellingstat2 <- renderPlot({
      consumption %>%
        filter(Region==input$region) %>% 
        mutate(class = fct_reorder(Description, kwh_per_acc, .fun='mean')) %>%
        ggplot( aes(x=reorder(Description, kwh_per_acc), y=kwh_per_acc)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", color="red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        ggtitle("Boxplot of consumption per planning area")
    })
    
    output$anovastat2 <- renderPrint({
      consumption %>% 
        filter(Region == input$region)
      summary(aov(kwh_per_acc ~ Description, data = consumption))
    })
  })
  
  # sparklines -----------------------------------------------------------------
  observeEvent(c(input$slider_year, input$slope_value),{
    startyear <- input$slider_year[1]
    endyear <- input$slider_year[2]
    
    d_sparks <- dwelling %>%
      filter(year %in% c(startyear:endyear)) %>%
      mutate(`Dwelling Type` = DWELLING_TYPE) %>%
      group_by(`Dwelling Type`) %>%
      summarize(`Monthly Consumption` = list(consumption_GWh))
    #react_sparkline
    output$sparkline <- renderReactable(reactable(
      d_sparks,
      defaultPageSize = 13,
      columns = list(
        `Dwelling Type` = colDef(maxWidth = 200),
        `Monthly Consumption` = colDef(
          cell = react_sparkline(
            d_sparks,
            highlight_points = highlight_points(
              min = "red", max = "blue"),
            line_width = 1,
            bandline = "innerquartiles",
            bandline_color = "green"
          )
        )
      )
    ))
    # spark table --------------------------------------------------------------
    output$sparktable <- renderTable(dwelling %>%
      filter(year %in% c(startyear:endyear)) %>%
      group_by(DWELLING_TYPE) %>%
      summarise("Min" = min(consumption_GWh, na.rm = T),
                "Max" = max(consumption_GWh, na.rm = T),
                "Average" = mean(consumption_GWh, na.rm = T)
      ) %>%
      gt() %>%
      fmt_number(columns = 4,
                 decimals = 2))
    
    # slope graph---------------------------------------------------------------
    cons_yr <- dwelling
    
    if (input$slope_value == "sum") {
      cons_year <- cons_yr %>%
        group_by(DWELLING_TYPE, year) %>%
        summarise(mean_cons=round(sum(consumption_GWh),2))}
    if (input$slope_value == "average") {
      cons_year <- cons_yr %>%
        group_by(DWELLING_TYPE, year) %>%
        summarise(mean_cons=round(mean(consumption_GWh),2))}
    
    if (input$slope_value == "median") {
      cons_year <- cons_yr %>%
        group_by(DWELLING_TYPE, year) %>%
        summarise(mean_cons=round(median(consumption_GWh),2))}
    
    p_slopegraph <- cons_year %>% 
      mutate(year = factor(year)) %>%
      filter(year %in% c(startyear,endyear)) %>%
      newggslopegraph(year, mean_cons, DWELLING_TYPE)
    
    p_slopegraph1 <- p_slopegraph + labs(title = "Monthly Household Electricity Consumption between 2 points of time",
                                         subtitle = "",
                                         caption = "Source:EMA.gov.sg")
    
    output$slope <- renderPlot({p_slopegraph1})
  })
  
  ##  dtw ------------------------------------------------------------------------
  observeEvent(c(input$k2, input$method2),{
  cluster_dtw <- tsclust(clus_matrix1[,-c(1)],
                         type = "h", 
                         k=input$k2,
                         distance="dtw",
                         control = hierarchical_control(method = input$method2),
                         preproc = NULL,
                         args=tsclust_args(dist = list(window.size = 5L)))
  
  hclus_dtw <- cutree(cluster_dtw, k=input$k2) %>%
    as.data.frame(.) %>%
    rename(.,cluster_group = .) %>%
    rownames_to_column("type_col")
  
  # add the cluster number
  dtw_cluster <- clus_group1 %>%
    left_join(hclus_dtw, by=c("Description" = "type_col")) 
  
  # change date columns into rows
  dtw_cluster_t <- dtw_cluster %>%
    mutate_at(vars(contains("202")),as.numeric) %>%
    gather(Date, value, 2:55)
  
  # Add the word "Cluster"
  dtw_cluster_t$cluster_group <- paste("Cluster", dtw_cluster_t$cluster_group)
  
  # convert Date into date format
  dtw_cluster_t$Date <- parse_date_time(dtw_cluster_t$Date, orders=c("%Y-%m-%d")) 
  
  # plot time series by cluster
  
  ts <- plot_time_series(.data=dtw_cluster_t,
                         .date_var=Date, 
                         .value=value,
                         .color_var=Description,
                         .facet_var=cluster_group,
                         .facet_ncol=2,
                         .facet_scales = "free_y",
                         .smooth=FALSE,
                         .line_size = 0.3,
                         .plotly_slider = TRUE,
                         .title = "Time Series Plot by cluster")
  
  ts <- ts %>%
    layout(hovermode="x",
           hoverlabel=list(font=list(size=7)))
  
  output$dtw <- renderPlotly( ts )
  })
  
  #     # Preparing the choropleth map
  #     mpsz_clus <- left_join(singapore, clus_hc, by = c("PLN_AREA_N" = "Description"))
  #       output$map <- renderTmap(
  #
  #         tm_shape(mpsz_clus)+
  #           tmap_options(check.and.fix = TRUE)+
  #           tm_fill("cluster", id=paste("PLN_AREA_N"),
  #                   style = "pretty",
  #                   palette = viridis(input$k)) +
  #           tm_borders(alpha = 0.7)
  #       )
  
  # clustering dendro ----------------------------------------------------------
  clus_data <- T3.5 %>%
    filter(month != "Annual" &
             year > 2017 &
             dwelling_type != "Overall" &
             !str_detect(Description,"Region|Pioneer|Overall"))

  # transform dataset
  # convert kwh into numbers
  clus_data$kwh_per_acc <- as.numeric(clus_data$kwh_per_acc)
  # join month and year into a date
  clus_data$date <- parse_date_time(paste(clus_data$year, clus_data$month), orders=c("%Y %m"))

  # drop month and year column
  clus_data <- subset(clus_data, select=-c(month, year, Region)) %>%
    arrange(date)

  # pivot wider
  clus <- clus_data %>%
    pivot_wider(names_from=date, values_from=kwh_per_acc)

  # omit na
  clus <- na.omit(clus)
  clus <- clus %>% relocate(Description, .before = dwelling_type)

  clus_group1 <- clus[,-c(2)] %>%
    group_by(Description) %>%
    summarise_each(list(sum))

  # making "Description" the row name (index)
  row.names(clus_group1) <- clus_group1$Description

  # Making it into a matrix
  clus_matrix1 <- data.matrix(clus_group1)

  # plot
  observeEvent(c(input$k, input$method, input$distance),{

    output$dendro <- renderPlotly({
      heatmaply(clus_matrix1[,-c(1)],
                scale = "column",
                dist_method = input$distance,
                hclust_method = input$method,
                Colv=NA,
                seriate = "none",
                k_row = input$k,
                margins = c(NA,200,50,NA),
                colors = viridis(
                  n= 256, alpha=1,
                  begin=0, end=1,
                  option="viridis"),
                fontsize_row = 7,
                fontsize_col = 7,
                main="Hierarchical Clustering",
                ylab = "Towns",
                xlab = "Time")
    })

    # clustering <- dist(normalize(clus_group1,-c(1)), method="euclidean")

#     # clustering dendex --------------------------------------------------------
#     output$dendextend <- renderDataTable(
#       dend_expend(clustering)[[3]]
#     )
#     # clustering number k ------------------------------------------------------
#     clust2 <- hclust(clustering, method = input$method)
#     num_k <- find_k(clust2)
#     output$numberk <- renderPlot(
#       plot(num_k)
#     )
#
#     # clustering map -----------------------------------------------------------
#     num_clus <- cutree(clust2, k=input$k)
#     clus_hc <- cbind(clus_group1, cluster = as.factor(num_clus))
#
#     clus_hc$Description <- toupper(clus_hc$Description)
#
#     # Preparing the choropleth map
#     mpsz_clus <- left_join(singapore, clus_hc, by = c("PLN_AREA_N" = "Description"))
#       output$map <- renderTmap(
#
#         tm_shape(mpsz_clus)+
#           tmap_options(check.and.fix = TRUE)+
#           tm_fill("cluster", id=paste("PLN_AREA_N"),
#                   style = "pretty",
#                   palette = viridis(input$k)) +
#           tm_borders(alpha = 0.7)
#       )
  })
  
}

shinyApp(ui = ui, server = server)
