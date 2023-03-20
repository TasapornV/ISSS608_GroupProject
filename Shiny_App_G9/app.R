library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)

library(dplyr)
library(tidyverse)
library(tidymodels)
library(tidyquant)
library(lubridate)
library(tsibble)
library(tsibbletalk)
library(data.table)
library(knitr)
library(psych)
library(earth)
library(stats)
library(CGPfunctions)

library(ggrepel)
library(ggstatsplot)
library(ggplot2)
library(plotly)
library(ggridges)

library(tseries)
library(feasts)
library(fable)
library(forecast)
library(devtools)

library(RColorBrewer)
library(geofacet)
library(treemap)
library(cluster)
library(dendextend)
library(dendextend)
library(heatmaply)
library(fpc)
library(sf)
library(tmap)

library(zoo)

## Read compressed data file
T2.3 <- readRDS(file = "RDS/T2-3.rds") # Peak System Demand
T3.4 <- readRDS(file = "RDS/T3-4.rds") # Total Household Electricity Consumption by Dwelling Type
T3.5 <- readRDS(file = "RDS/T3-5.rds") # Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type

# reading the map file
mpsz <- st_read(dsn = 'master-plan-2014-subzone-boundary-web-shp',
                layer = 'MP14_SUBZONE_WEB_PL',
                crs = 3414)

# Import the area grid data.
area_grid <- read_csv("data/areagrid.csv")

singapore <- st_transform(mpsz, 4326)

# wrangling data
town <- subset(T3.5, Description != 'Overall' & Description !='Central Region' & 
                 Description !='East Region' & Description !='North East Region' &
                 Description !='North Region' & Description !='West Region' &
                 kwh_per_acc != 's' & dwelling_type != 'Private Housing' &
                 dwelling_type != 'Public Housing' & month != 'Annual') %>%
  mutate(kwh_per_acc = as.numeric(kwh_per_acc)) %>%
  mutate(date = parse_date_time(paste0(year, "-", month,"-1"),"ymd"))

town$type <- case_when(
  town$dwelling_type %in% c('Private Apartments and Condominiums',
                            'Landed Properties', 'Others') ~ "Private",
  town$dwelling_type %in% c('1-room / 2-room','3-room','4-room',
                            '5-room and Executive') ~ "Public")
consumption <- T3.5
consumption <- consumption %>%
  mutate(kwh_per_acc = as.numeric(kwh_per_acc)) %>%
  mutate(year = as.character(year)) %>%
  mutate('date' = make_date(year=year, month=month))

# clus_hc$Description <- toupper(clus_hc$Description)

## Set up parameter
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
      ## OVERVIEW --------------------------------------------------------------
      tabItem(
        tabName = "overview",
        navbarPage("OVERVIEW",
                   
                   ### introduction --------------------------------------------
                   tabPanel("Introduction",introtext),
                   
                   ### geofacet ------------------------------------------------
                   tabPanel("Consumption by Planning Area & Dwelling Type",
                            fluidPage(
                              radioButtons("axis", label = "select axis control",
                                           choices = c("fixed", 
                                                       "free x-axis" = "free_x", 
                                                       "free y-axis" = "free_y", 
                                                       "free"), 
                                           inline = T),
                              plotOutput("geo", height = 800)
                            )),
                   
                   ### peak system demand --------------------------------------
                   tabPanel("Peak System Demand",
                            fluidPage(
                              fluidRow(
                                column(3, 
                                       wellPanel(
                                         sliderInput("slider_year", "Select year",min = 2005, 
                                                     max = 2022, step = 1, round = TRUE,
                                                     value =  c(2005, 2022)),
                                         radioButtons("slope_value", "select value", choices = c("sum", "average", "median" ))
                                       )),
                                
                                column(9, plotOutput("slope",height=400))
                              )),
                            
                            fluidPage(
                              column(12, plotlyOutput("peakdemand"), height = 200),
                              column(12, plotlyOutput("cycleplot"), height = 100)
                            )),
                   
                   ### consumption by dwelling type ----------------------------
                   tabPanel("Consumption by Dwelling Type",
                            fluidPage( plotlyOutput("dwelling"))
                   )
        )
      ),
      
      ## CLUSTERING ----------------------------------------------------------------
      tabItem(
        tabName = "clustering",
        navbarPage("CLUSTERING",
                   ### hierachical Clustering ----------------------------
                   tabPanel("Hierachical Clustering",
                            fluidPage(
                              fluidRow(
                                column(3,pickerInput("method2", "Select method",
                                                     choices = c("agglomerative", "divisive"),
                                                     selected = "agglomerative")),
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
                                           column(4, dataTableOutput("dendextend")),
                                           column(12, plotlyOutput("numberk")),
                                           column(8, plotlyOutput("dendro", height = "500px"))
                                         )
                              # ,
                              # 
                              #            column(12, tmapOutput("map"))
                            )
                   ),
                   
                   ### Time Series Clustering ----------------------------
                   tabPanel("Time Series Clustering")
        )
      ),
      
      ## INFERENTIAL STATISTICS ------------------------------------------------
      tabItem(tabName = "inferential",
              navbarPage("INFERENTIAL STATISTICS", 
                         
                         ### anova ---------------------------------------------
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

                         ### correlation ---------------------------------------
                          tabPanel("Correlation Analysis",
                                   fluidPage(
                                     fluidRow(
                                       column(width = 9, plotlyOutput("correlation",height=400))
                                     )
                         ))
              )
      ),
      
      ## TIME SERIES FORECASTING------------------------------------------------
      
      tabItem(tabName = "time_series", icon = icon("chart-line"),
              navbarPage("TIME SERIES FORECASTING",

                         ### arima ---------------------------------------------
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
                                    #   ,
                                    #   column(5,      
                                    #          verbatimTextOutput("arimatext"))
                                    # #   ,
                                    # fluidRow(
                                    #   column(width = 6, plotOutput("arima",height="500px")),
                                    #   column(width = 6, plotOutput("arima_plot",height=400))
                                    #   )
                                  ))
                         )
              )
      ),
      
      ## DATA TABLE ------------------------------------------------------------
      tabItem(tabName = "data", icon = icon("table"),
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
      tabItem(tabName = "about", icon = icon("info"))
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
              axis.text.x = element_text(size = 10, angle = 90),
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
  # arima$ym <- yearmonth(as.yearmon(paste(arima$year, arima$month), "%Y %m"))
  # a <- arima %>%
  #   group_by(ym) %>%
  #   summarise(avgcon = mean(kwh_per_acc, na.rm = TRUE)) %>%
  #   ungroup()
  # 
  # arima_ts <- ts(data=a$avgcon, start = c(2005,1), end = c(2022,6), frequency=12)
  # 
  # 
  # observeEvent(c(input$timemodel, input$arima_d,input$arima_d2, input$arima_d3, input$year), {
  #   
  #   arima_arima = auto.arima(arima_ts,
  #                            d = input$arima_d,
  #                            D = input$arima_d2,
  #                            allowdrift = input$arima_d3)
    # fit <- ets(window(arima_ts))
    #   output$arima <- renderPlot(
    #     { plot(forecast(arima_arima)) }
    #   )
      
    # if(input$timemodel == "ETS"){
    #   fit <- ets(window(arima_ts))
    #   output$arima <- renderPlot(
    #     { plot(forecast(arima_ts, h =10, model = fit)) }
    #   )
    # }
    # 
    # if(input$timemodel == "ARIMA"){
    #   output$arima <- renderPlot(
    #     { plot(forecast(arima_ts, h =10, model = arima(window(arima_ts))))}
    #   )
    # }
    # 
    # if(input$timemodel == "AR"){
    #   output$arima <- renderPlot(
    #     {plot(forecast(ar(window(arima_ts))))}
    #   )
    # }
    
    output$arimatext <- renderPrint(arima_arima)
    
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
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
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
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle("Boxplot of consumption per planning area")
    })}
    if(input$anovainput == "dwelling_type"){output$dwellingstat <- renderPlot({
      consumption %>%
        mutate(class = fct_reorder(dwelling_type, kwh_per_acc, .fun='mean')) %>%
        ggplot( aes(x=reorder(dwelling_type, kwh_per_acc), y=kwh_per_acc)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", color="red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
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
  
  # anova2 -----------------------------------------------------------------
  observeEvent(input$region,{
    output$dwellingstat2 <- renderPlot({
      consumption %>%
        filter(Region==input$region) %>% 
        mutate(class = fct_reorder(Description, kwh_per_acc, .fun='mean')) %>%
        ggplot( aes(x=reorder(Description, kwh_per_acc), y=kwh_per_acc)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", color="red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle("Boxplot of consumption per planning area")
    })
    
    output$anovastat2 <- renderPrint({
      consumption %>% 
        filter(Region == input$region)
      summary(aov(kwh_per_acc ~ Description, data = consumption))
    })
  })
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
  observeEvent(c(input$k, input$method, input$method2, input$distance),{
    
    output$dendro <- renderPlotly({
      heatmaply(clus_matrix1[,-c(1)],
                scale = "column",
                dist_method = input$distance,
                hclust_method = input$method2,
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
    
    clustering <- dist(normalize(clus_group1,-c(1)), method=input$distance)
    
    # clustering dendex --------------------------------------------------------
    output$dendextend <- renderDataTable(
      dend_expend(clustering)[[3]]
    )
    # clustering number k-------------------------------------------------------
    clust2 <- hclust(clustering, method = input$method)
    num_k <- find_k(clust2)
    output$numberk <- renderPlotly(
      plot(num_k)
    )
    
    

    
  })
  
  

  # Convert to factor
  # clus$Description <- factor(clus$Description)
  # clus$dwelling_type <- factor(clus$dwelling_type)
  # 
  # # calculate distance - can only use "gower" because data has categorical variable
  # clus_dist <- daisy(clus, metric="gower")
  # 
  # # function to create table for clustering stats
  # # Cluster stats comes out as list while it is more convenient to look at it as a table
  # # This code below will produce a dataframe with observations in columns and variables in row
  # # Not quite tidy data, which will require a tweak for plotting, but I prefer this view as an output here as I find it more comprehensive 
  # 
  # cstats.table <- function(dist, tree, k) {
  #   clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
  #                     "wb.ratio","dunn2","avg.silwidth")
  #   clust.size <- c("cluster.size")
  #   stats.names <- c()
  #   row.clust <- c()
  #   output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  #   cluster.sizes <- matrix(ncol = k, nrow = k)
  #   for(i in c(1:k)){
  #     row.clust[i] <- paste("Cluster-", i, " size")
  #   }
  #   for(i in c(2:k)){
  #     stats.names[i] <- paste("Test", i-1)
  #     
  #     for(j in seq_along(clust.assess)){
  #       output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
  #     }
  #     for(d in 1:k) {
  #       cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
  #       dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
  #       cluster.sizes[d, i]
  #       
  #     }
  #   }
  #   output.stats.df <- data.frame(output.stats)
  #   cluster.sizes <- data.frame(cluster.sizes)
  #   cluster.sizes[is.na(cluster.sizes)] <- 0
  #   rows.all <- c(clust.assess, row.clust)
  #   # rownames(output.stats.df) <- clust.assess
  #   output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  #   colnames(output) <- stats.names[2:k]
  #   rownames(output) <- rows.all
  #   is.num <- sapply(output, is.numeric)
  #   output[is.num] <- lapply(output[is.num], round, 2)
  #   output
  # }
  # 
  # 
  # 
  # row.names(clus_group1) <- clus_group1$Description
  # clus_matrix1 <- data.matrix(clus_group1)
  # observeEvent(c(input$k, input$method, input$method2, input$distance),{
  #   
  #   if(input$method2 == "agglomerative") {
  #     title <- "Agglomerative (complete) - Elbow"
  #     output$numberk <- renderPlotly({
  #       ggplot(data = data.frame(t(cstats.table(clus_dist, hc, 15))), 
  #              aes(x=cluster.number, y=within.cluster.ss)) + 
  #         geom_point()+
  #         geom_line() +
  #         ggtitle(title) +
  #         labs(x = "Num.of clusters", y = "Within clusters sum of squares") +
  #         theme_minimal(base_size=12) +
  #         theme(axis.title=element_blank(),
  #               plot.title = element_text(size= rel(1))
  #         )
  #     })} 
  #   
  #   if(input$method2 == "divisive") {
  #     title <- "Divisive (complete) - Elbow"
  #     output$numberk <- renderPlotly({
  #       ggplot(data = data.frame(t(cstats.table(clus_dist, divisive.clust, 15))), 
  #              aes(x=cluster.number, y=within.cluster.ss)) + 
  #         geom_point()+
  #         geom_line() +
  #         ggtitle(title) +
  #         labs(x = "Num.of clusters", y = "Within clusters sum of squares") +
  #         theme_minimal(base_size=12) +
  #         theme(axis.title=element_blank(),
  #               plot.title = element_text(size= rel(1))
  #         )
  #     })}
  #   
  # 
  #   
  #   num_clus <- cutree(a, k=input$k)
  #   clus_hc <- cbind(clus, cluster = as.factor(num_clus))
  #   clus_hc$Description <- toupper(clus_hc$Description)
  # 
  #   # Preparing the choropleth map
  #   mpsz_clus <- left_join(singapore, clus_hc, by = c("PLN_AREA_N" = "Description"))
  #   # output$map <- renderTmap(
  #   #   
  #   #   tm_shape(mpsz_clus)+
  #   #     tmap_options(check.and.fix = TRUE)+
  #   #     tm_fill("cluster", id=paste("PLN_AREA_N"),
  #   #             style = "pretty",
  #   #             palette = rainbow(input$k)) +
  #   #     tm_borders(alpha = 0.7)
  #   # )
  # })
  
  # slope graph-----------------------------------------------------------------
  observeEvent(c(input$slider_year, input$slope_value),{
    startyear <- input$slider_year[1]
    endyear <- input$slider_year[2]
    
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
                                         caption = "Source:Singstat.gov.sg")
    
    output$slope <- renderPlot({p_slopegraph1})
  })
  
}

shinyApp(ui = ui, server = server)
