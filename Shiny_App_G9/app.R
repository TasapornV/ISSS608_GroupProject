# LIBRARY -----------------------------------------------------------------
# remotes::install_github("timelyportfolio/dataui")
# install.packages('PMCMRplus')
# packages = c('CGPfunctions', 'cluster', 'data.table', 'dataui','dendextend', 
#              'devtools','DT', 'dplyr', 'dtwclust', 'earth', 'fable', 'feasts', 
#              'forecast','fpc', 'geofacet', 'ggdendro', 'ggiraph', 'ggplot2', 
#              'ggstatsplot', 'ggrepel', 'ggridges', 'gt', 'gtExtras', 'heatmaply', 
#              'knitr','plotly', 'lubridate', 'psych', 'RColorBrewer', 'reactable', 
#              'reactablefmtr', 'readr', 'sf', 'shiny', 'shinydashboard', 
#              'shinythemes', 'shinyWidgets', 'stats', 'tibble', 'tidymodels', 
#              'tidyquant', 'tidyr', 'tidyverse', 'timetk', 'tmap', 'treemap', 
#              'TSclust', 'tseries', 'tsibble', 'tsibbletalk', 'zoo')
# 
# for(p in packages){
#   library(p,character.only = T)
# }
# 
library(CGPfunctions)
library(cluster)
library(data.table)
# library(dataui)
library(dendextend)
# library(devtools)
# library(DT)
library(dplyr)
library(dtwclust)
# library(earth)
# library(fable)
# library(feasts)
library(forecast)
# library(fpc)
library(geofacet)
library(ggdendro)
# library(ggiraph)
library(ggplot2)
library(ggstatsplot)
# library(ggrepel)
# library(ggridges)
library(gt)
# library(gtExtras)
library(heatmaply)
# library(knitr)
library(plotly)
library(PMCMRplus)
library(lubridate)
# library(psych)
# library(RColorBrewer)
library(reactable)
library(reactablefmtr)
library(readr)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(stats)
library(svglite)
# library(tibble)
# library(tidymodels)
# library(tidyquant)
library(tidyr)
library(tidyverse)
library(timetk)
library(tmap)
# library(treemap)
library(TSclust)
# library(tseries)
library(tsibble)
# library(tsibbletalk)
library(zoo)
# READ DATA ---------------------------------------------------------------
## Read compressed data file
T2.3 <- readRDS(file = "RDS/T2-3.rds") # Peak System Demand
T3.4 <- readRDS(file = "RDS/T3-4.rds") # Total Household Electricity Consumption by Dwelling Type
T3.5 <- readRDS(file = "RDS/T3-5.rds") # Average Monthly Household Electricity Consumption by Town & Dwelling Type
consumption <- readRDS(file = "RDS/anova.rds") # Anova
town <- readRDS(file = "RDS/town.rds") # Geofacet
clus_data <- readRDS(file = "RDS/clus_data.rds") # clustering
clus <- readRDS(file = "RDS/clus.rds") # clustering
chosendata <- readRDS(file = "RDS/chosendata.rds") # intro
dwelling <- readRDS(file = "RDS/dwelling.rds") #intro
# reading the map file
mpsz <- st_read(dsn = 'master-plan-2014-subzone-boundary-web-shp',
                layer = 'MP14_SUBZONE_WEB_PL',
                crs = 3414)
# Import the area grid data.
area_grid <- read_csv("data/areagrid.csv")
singapore <- st_transform(mpsz, 4326)
# PARAMETER ---------------------------------------------------------------
years <- c("2022","2021", "2020", "2019", "2018", "2017")
regions <- c("Central Region", "North East Region", "East Region", "North Region", "West Region")
tables <- c("Peak System Demand" = "T2.3",
            "Total Household Electricity Consumption by Dwelling Type" = "T3.4",
            "Average Monthly Household Electricity Consumption by Town & Dwelling Type" = "T3.5")
type <- c("parametric", "nonparametric", "robust", "bayes")
towns <- unique(chosendata$type)
dwellingtype <- unique(dwelling$DWELLING_TYPE)
introtext <- "Welcome to our Singapore Electricity Consumption Visualization Application. This interactive tool allows you to explore and analyze the electricity consumption patterns in Singapore over the past decade. With data sourced from official government records, our application presents a range of visually appealing and informative charts and graphs, providing insights into trends and patterns in energy consumption across various sectors, including residential, commercial, and industrial. Whether you're a policy maker, a researcher, or a concerned citizen, our tool provides a powerful platform to understand and interpret the dynamics of energy use in Singapore, and to make informed decisions that can shape a sustainable future for all."
# UI ----------------------------------------------------------------------
ui = dashboardPage(skin = "yellow",
                   dashboardHeader(title = 'Singapore Electricity Consumption', titleWidth = 400),
                   
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
                                     tabPanel("Introduction",
                                              fluidPage(
                                                fluidRow(introtext),
                                                fluidRow(
                                                  column(4, imageOutput("page1")),
                                                  column(4, imageOutput("page2")),
                                                  column(4, imageOutput("page4")),
                                                  column(4, imageOutput("page5")),
                                                  column(4, imageOutput("page6")),
                                                  column(4, imageOutput("page7")),
                                                  column(4, imageOutput("page9"))
                                                ))
                                     ),
                                     
                                     ### 1.2 geofacet ------------------------------------------------
                                     tabPanel("Consumption by Town & Dwelling Type",
                                              fluidPage(
                                                radioButtons("axis", label = "Select Y-axis Control",
                                                             choices = c("Fixed y-axis" = "fixed",
                                                                         "Free y-axis" = "free_y"),
                                                             inline = T),
                                                withSpinner(plotOutput("geo", height = 800))
                                              )),
                                     
                                     ### 1.3 Consumption by Town -----------------------------------
                                     tabPanel("Consumption by Town",
                                              fluidRow(
                                                column(3, sliderInput("slider_year", "Select Year",min = 2005,
                                                                      max = 2022, step = 1, round = FALSE,
                                                                      value =  c(2005, 2021),
                                                                      sep = "")),
                                                column(3, pickerInput("towns", "Select Town", choices = towns,
                                                                      selected = c("Ang Mo Kio", "Clementi"),
                                                                      multiple = TRUE)),
                                                column(2, radioButtons("slope_value", "Select Slopegraph Value",
                                                                       choices = c("Sum"="sum", "Average"="average", "Median"="median"),
                                                                       inline = FALSE,
                                                                       selected = "average")),
                                                column(3, htmlOutput("remark"))
                                              ),
                                              fluidRow(
                                                column(6, withSpinner(plotlyOutput("dwelling", height = 400))),
                                                column(6, plotOutput("slope",height=400))
                                              ),
                                              fluidRow(
                                                column(12, plotlyOutput("cycleplot")),
                                                column(6, tableOutput("sparktable"))
                                              )
                                     ),
                                     ### 1.4 Consumption by Dwelling type and dwelling type -------
                                     tabPanel("Consumption by Dwelling Type",
                                              fluidRow(
                                                column(3, sliderInput("slider_year2", "Year",min = 2005,
                                                                      max = 2022, step = 1, round = FALSE,
                                                                      value =  c(2005, 2021),
                                                                      sep = "")),
                                                column(3, pickerInput("towns2", "Select Dwelling Type", choices = dwellingtype,
                                                                      selected = c("3-room", "4-room"),
                                                                      multiple = TRUE)),
                                                column(3, radioButtons("slope_value2", "Select Slopegraph Value",
                                                                       choices = c("Sum"="sum", "Average"="average", "Median"="median"),
                                                                       inline = TRUE,
                                                                       selected = "average"))
                                              ),
                                              fluidRow(
                                                column(6, withSpinner(plotlyOutput("dwelling2", height = 400))),
                                                column(6, plotOutput("slope2",height=400))
                                              ),
                                              fluidRow(
                                                column(6, plotlyOutput("cycleplot2")),
                                                column(6, tableOutput("sparktable2"))
                                              )
                                     )
                         )
                       )
                       ,
                       ## 2 CLUSTERING ----------------------------------------------------------------
                       tabItem(
                         tabName = "clustering",
                         navbarPage("CLUSTERING",
                                    ### 2.1 hierachical Clustering ----------------------------
                                    tabPanel("Hierachical Clustering",
                                             fluidPage(
                                               fluidRow(
                                                 column(2,pickerInput("distance", "Select Distance Calculation",
                                                                      choices = c("euclidean", "maximum", "manhattan",
                                                                                  "canberra", "binary", "minkowski"))),
                                                 
                                                 column(2,pickerInput("method", "Select Clustering Method",
                                                                      choices = c("ward.D", "ward.D2", "single",
                                                                                  "complete", "average", "mcquitty",
                                                                                  "median", "centroid"),
                                                                      selected = "complete")),
                                                 
                                                 column(2, pickerInput("seriate", "Select Seriation",
                                                                       choices = c("Optimal Leaf Ordering" = "OLO", "Gruvaeus and Wainer" = "GW", "Mean"="mean", "None" = "none"))),
                                                 
                                                 
                                                 column(2,numericInput("k", "Select Number of Cluster",
                                                                       min = 1, max = 10, value = 2))
                                               ),
                                               
                                               fluidRow(
                                                 column(4, dataTableOutput("dendextend"),
                                                        plotOutput("numberk", height = "350px")),
                                                 column(8, withSpinner(plotlyOutput("dendro", height = "400px")),
                                                        tmapOutput("map")
                                                 )
                                               )
                                             )
                                    ),
                                    
                                    ### 2.2 Time Series Clustering ----------------------------------
                                    tabPanel("Time Series Clustering",
                                             fluidPage(
                                               fluidRow(
                                                 column(2, pickerInput("distance_func", "Select Distance Function",
                                                                       choices = c("dtw_basic", "dtw", "dtw2", "lbk", 
                                                                                   "lbi", "sbd", "gak", "sdtw"),
                                                                       selected = "dtw")),
                                                 
                                                 column(3,pickerInput("method2", "Select Clustering Method",
                                                                      choices = c("ward.D", "ward.D2", "single",
                                                                                  "complete", "average", "mcquitty",
                                                                                  "median", "centroid"),
                                                                      selected = "complete")),
                                                 
                                                 column(3,numericInput("k2", "Select Number of Cluster",
                                                                       min = 1, max = 10, value = 2))
                                               ),
                                               withSpinner(plotlyOutput("dtw", height =500)),
                                               withSpinner(tmapOutput("dtwmap", height =500))
                                             ))
                         )
                       ),
                       ## 3 INFERENTIAL STATISTICS ------------------------------------------------
                       tabItem(tabName = "inferential",
                               navbarPage("INFERENTIAL ANALYSIS",
                                          
                                          ### 3.1 ANOVA boxplot ---------------------------------------------
                                          tabPanel("ANOVA: Boxplot",
                                                   fluidPage(
                                                     fluidRow(
                                                       column(5,
                                                              htmlOutput("description1"),
                                                              pickerInput(inputId = "anovainput",
                                                                          label = "Select Factor",
                                                                          choices = c("Dwelling Type"="dwelling_type", "Region", "Year"="year"),
                                                                          selected = "Region",
                                                                          options = list(`actions-box` = TRUE),
                                                                          multiple = F),
                                                              
                                                              verbatimTextOutput("anovastat")
                                                       ),
                                                       column(7, withSpinner(plotOutput("dwellingstat")))
                                                     ),
                                                     
                                                     fluidRow(
                                                       column(5,
                                                              htmlOutput("description2"),
                                                              pickerInput(inputId = "region",
                                                                          label = "Select Region",
                                                                          choices = regions,
                                                                          selected = "East Region",
                                                                          options = list(`actions-box` = TRUE),
                                                                          multiple = F),
                                                              verbatimTextOutput("anovastat2")
                                                       ),
                                                       column(7, withSpinner(plotOutput("dwellingstat2")))
                                                     )
                                                   )),
                                          
                                          ### 3.2 ANOVA pairwise stat ---------------------------------------
                                          tabPanel("ANOVA: Pairwise Analysis",
                                                   fluidPage(
                                                     fluidRow(
                                                       column(2, pickerInput(
                                                         inputId = "anovainput2",
                                                         label = "Select Factor",
                                                         choices = c("Dwelling Type"="dwelling_type", "Region","Year"="year"),
                                                         selected = "Region",
                                                         options = list(`actions-box` = TRUE),
                                                         multiple = F)),
                                                       column(2,pickerInput("conf", "Confidence level",
                                                                            choices = c(0.95, 0.99))),
                                                       column(2, pickerInput(
                                                         inputId = "typegg",
                                                         label = "Select type of test",
                                                         choices = c("parametric" = "p", "nonparametric" = "np", "robust" = "r", "Bayes Factor" = "bf")))
                                                
                                                       ),
                                                     fluidRow(withSpinner(plotOutput("dwellingstat3"))),
                                                     fluidRow(
                                                       pickerInput(inputId = "region2",
                                                                   label = "Select Region",
                                                                   choices = regions,
                                                                   selected = "East Region",
                                                                   options = list(`actions-box` = TRUE),
                                                                   multiple = F)
                                                     ),
                                                     fluidRow(withSpinner(plotOutput("dwellingstat4", height = "500px")))
                                                   )
                                          )
                               )
                       ),
                       
                       ## 4 TIME SERIES FORECASTING------------------------------------------------
                       
                       tabItem(tabName = "time_series",
                               navbarPage("TIME SERIES FORECASTING",
                                          
                                          ### 4.1 arima ---------------------------------------------
                                          tabPanel("Trend Prediction",
                                                   fluidPage(
                                                     fluidRow(
                                                       column(width = 5, 
                                                              numericInput("arima_d", "input order of differencing", value=1),
                                                              numericInput("arima_d2", "input order of seasonal differencing", value=2),
                                                              checkboxInput("arima_d3", "allow drift", value = FALSE),
                                                              sliderInput("year", "Select year", min = 2005, max = 2022, step=1, round=TRUE, value = 2022),
                                                              verbatimTextOutput("arimatext")),
                                                       column(width = 7, withSpinner(plotOutput("arima",height=350))),
                                                       column(width = 12, plotOutput("arima_plot",height=400))
                                                     ) )
                                          )
                               )
                       ),
                       
                       ## DATA TABLE ------------------------------------------------------------
                       tabItem(tabName = "data",
                               navbarPage("DATA",
                                          tabPanel("Data Table",
                                                   fluidPage(
                                                     fluidRow(
                                                       column(8, wellPanel(
                                                         pickerInput("SelectTable",
                                                                     label = "Select Data Table To View",
                                                                     choices = tables,
                                                                     selected = "Electricity")
                                                       )),
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
        labs(title = "Average Monthly Household Electricity Consumption by Town & Dwelling Type",
             color = "Dwelling Type") +
        xlab("Year") + 
        ylab("Average \nConsumption") +
        theme(plot.title = element_text(size=21, face="bold"),
              axis.title.y = element_text(face="bold", angle=0, vjust=0.5, hjust=1),
              axis.title.x = element_text(face="bold"),
              axis.text.x = element_text(size = 8, angle = 45),
              axis.text.y = element_text(size = 8),
              strip.text = element_text(size = 10),
              legend.position = "right")
    })
  })
  
  # table ----------------------------------------------------------------------
  observeEvent((input$SelectTable),{
    
    if(input$SelectTable == "T2.3")  {tabletext <- T2.3}
    if(input$SelectTable == "T3.4")  {tabletext <- T3.4}
    if(input$SelectTable == "T3.5")  {tabletext <- T3.5}
    output$table <- renderDataTable(tabletext)
  })
  
  # arima ----------------------------------------------------------------------
  
  arima <- T2.3
  arima$Date <- yearmonth(as.yearmon(paste(arima$year, arima$mth), "%Y %m"))
  arima_ts <- ts(data=arima$peak_system_demand_mw)
  
  observeEvent(c(input$arima_d,input$arima_d2, input$arima_d3, input$year), {
    output$arima <- renderPlot({
      arima_arima = auto.arima(arima_ts, d = input$arima_d, D = input$arima_d2, allowdrift = input$arima_d3)
      plot(forecast(arima_arima))
    })
    output$arimatext <- renderPrint(arima_arima)
    arima_tsbl  = as_tsibble(arima)
    full_arima = arima_tsbl %>%
      filter(year==input$year) %>% 
      fill_gaps() %>% 
      tidyr::fill(peak_system_demand_mw, .direction = "down")
    output$arima_plot <- renderPlot({
      full_arima %>%
        gg_tsdisplay(difference(peak_system_demand_mw), plot_type='partial')
    })
  })
  
  
  # anova boxplot-----------------------------------------------------------------
  observeEvent(input$anovainput,{
    if(input$anovainput == "Region"){
      output$dwellingstat <- renderPlot({
        town %>%
          mutate(class = fct_reorder(Region, kwh_per_acc, .fun='mean')) %>%
          ggplot( aes(x=reorder(Region, kwh_per_acc), y=kwh_per_acc)) +
          geom_boxplot() +
          stat_summary(fun.y=mean, geom="point", color="red") +
          theme(legend.position="none") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          ggtitle("Boxplot of Consumption by Region") +
          ylab("Consumption")+
          xlab('')
        
      })
      
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ Region, data = town))
      })
    }
    
    if(input$anovainput == "year"){
      output$dwellingstat <- renderPlot({
        consumption %>%
          mutate(class = fct_reorder(year, kwh_per_acc, .fun='mean')) %>%
          ggplot( aes(x=reorder(year, kwh_per_acc), y=kwh_per_acc)) +
          geom_boxplot() +
          stat_summary(fun.y=mean, geom="point", color="red") +
          theme(legend.position="none") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          ggtitle("Boxplot of Consumption by Year") +
          ylab("Consumption")+
          xlab("")
      })
      
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ year, data = town))
      })
    }
    
    if(input$anovainput == "dwelling_type"){
      output$dwellingstat <- renderPlot({
        town %>%
          mutate(class = fct_reorder(dwelling_type, kwh_per_acc, .fun='mean')) %>%
          ggplot( aes(x=reorder(dwelling_type, kwh_per_acc), y=kwh_per_acc)) +
          geom_boxplot() +
          stat_summary(fun.y=mean, geom="point", color="red") +
          theme(legend.position="none") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          ggtitle("Boxplot of Consumption by Dwelling type") +
          ylab("Consumption")+
          xlab("")
      })
      
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ dwelling_type, data = town))
      })
      
    }
  })
  
  # anova boxplot2 ---------------------------------------------------------------------
  observeEvent(input$region,{
    output$dwellingstat2 <- renderPlot({
      region_title <- paste("Boxplot of consumption by Town in", input$region)
      town %>%
        filter(Region==input$region) %>% 
        mutate(class = fct_reorder(Description, kwh_per_acc, .fun='mean')) %>%
        ggplot( aes(x=reorder(Description, kwh_per_acc), y=kwh_per_acc)) +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", color="red") +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        ggtitle(region_title) +
        ylab("Consumption")+
        xlab('')
    })
    
    
    output$anovastat2 <- renderPrint({
      town %>% 
        filter(Region == input$region)
      summary(aov(kwh_per_acc ~ Description, data = town))
    })
  })
  
  # anova betweenstat -----------------------------------------------------------
  observeEvent(c(input$anovainput2, input$conf, input$typegg),{
    if(input$anovainput2 == "Region") {
      output$dwellingstat3 <- renderPlot({
        ggbetweenstats(
          data = town,
          x = Region,
          y = kwh_per_acc,
          conf.level = input$conf,
          pairwise.display = "significant",
          type = input$typegg,
          messages = FALSE
        )
      })}
    
    if(input$anovainput2 == "year") {
      output$dwellingstat3 <- renderPlot({
        ggbetweenstats(
          data = town,
          x = year,
          y = kwh_per_acc,
          conf.level = input$conf,
          pairwise.display = "significant",
          type = input$typegg,
          messages = FALSE
        )
      })}
    
    if(input$anovainput2 == "Dwelling_type") {
      output$dwellingstat3 <- renderPlot({
        ggbetweenstats(
          data = town,
          x = Dwelling_type,
          y = kwh_per_acc,
          conf.level = input$conf,
          pairwise.display = "significant",
          type = input$typegg,
          messages = FALSE
        )
      })}
  })
  
  # anova betweenstat2 ----------------------------------------------------------
  observeEvent(c(input$region2, input$conf, input$typegg),{
    output$dwellingstat4 <- renderPlot({
      town %>%
        filter(Region == input$region2) %>%
        ggbetweenstats(
          x = Description,
          y = kwh_per_acc,
          conf.level = input$conf,
          type = input$typegg,
          messages = FALSE
        )
    })
  })
  # consumption by town --------------------------------------------------------
  
  observeEvent(c(input$slider_year, input$slope_value, input$towns),{
    startyear <- input$slider_year[1]
    endyear <- input$slider_year[2]
    
    # line ----------------------------------------------------------------------
    chosendata <- chosendata %>%
      filter(year %in% c(startyear:endyear)) %>% 
      filter(type %in% input$towns)
    
    output$dwelling <- renderPlotly({
      p <- ggplot(data = chosendata, aes(x = date)) +
        geom_line(aes(y = consumption, colour = type)) + 
        labs(title = paste("Electricity Consumption by",chosendata[1,6]),
             x = "Year", y = paste0("Consumption, GWh ")) +
        scale_color_discrete(name="") +
        theme(legend.position="bottom")
      ggplotly(p, tooltip = c("x", "y", "colour")) %>% 
        layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
    })
    
    
    # cycle plot -----------------------------------------------------------------
    
    select_cycle <- chosendata %>% 
      filter(type %in% input$towns) %>%
      mutate(year = factor(year, levels = startyear:endyear),
             month = factor(month, levels = 1:12))
    
    
    # slope graph---------------------------------------------------------------
    cons_yr <- chosendata
    
    if (input$slope_value == "sum") {
      cons_year <- cons_yr %>%
        group_by(type, year) %>%
        summarise(mean_cons=round(sum(consumption),2))
      
      #Computing year average by months
      hline.data <- select_cycle %>%
        group_by(month) %>%
        summarise(avg_cons = sum(consumption))
      output$cycleplot <- renderPlotly({
        #Plotting cycle plot for electricity consumption per dwelling type
        ggplot() + 
          geom_line(data = select_cycle,
                    aes(x=year,y=consumption, group=month), colour = "black") +
          geom_hline(data = hline.data,
                     aes(yintercept=avg_cons),
                     linetype=6, 
                     colour="red", 
                     linewidth=0.5) +
          facet_grid(~month) +
          theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
          labs(title = paste("Cycleplot Shwoing Consumption (GWh) in Selected Towns" , startyear,"-",endyear),
               subtitle = paste0(chosendata[1,6],": ",input$towns)) +
          scale_x_discrete(breaks=c("2005","2010","2015","2020")) +
          xlab("Year") +
          ylab("Consumption")
      })
    }
    
    if (input$slope_value == "average") {
      cons_year <- cons_yr %>%
        group_by(type, year) %>%
        summarise(mean_cons=round(mean(consumption),2))
      
      #Computing year average by months
      hline.data <- select_cycle %>%
        group_by(month) %>%
        summarise(avg_cons = mean(consumption))
      output$cycleplot <- renderPlotly({
        #Plotting cycle plot for electricity consumption per dwelling type
        ggplot() + 
          geom_line(data = select_cycle,
                    aes(x=year,y=consumption, group=month), colour = "black") +
          geom_hline(data = hline.data,
                     aes(yintercept=avg_cons),
                     linetype=6, 
                     colour="red", 
                     linewidth=0.5) +
          facet_grid(~month) +
          theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
          labs(title = paste("Cycleplot Shwoing Consumption (GWh) in Selected Towns" , startyear,"-",endyear),
               subtitle = paste0(chosendata[1,6],": ",input$towns)) +
          scale_x_discrete(breaks=c("2005","2010","2015","2020")) +
          xlab("") +
          ylab("Consumption, GWh")
      })
    }
    if (input$slope_value == "median") {
      cons_year <- cons_yr %>%
        group_by(type, year) %>%
        summarise(mean_cons=round(median(consumption),2))
      #Computing year average by months
      hline.data <- select_cycle %>%
        group_by(month) %>%
        summarise(avg_cons = median(consumption))
      output$cycleplot <- renderPlotly({
        #Plotting cycle plot for electricity consumption per dwelling type
        ggplot() + 
          geom_line(data = select_cycle,
                    aes(x=year,y=consumption, group=month), colour = "black") +
          geom_hline(data = hline.data,
                     aes(yintercept=avg_cons),
                     linetype=6, 
                     colour="red", 
                     linewidth=0.5) +
          facet_grid(~month) +
          theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
          labs(title = paste("Cycleplot Shwoing Consumption (GWh) in Selected Towns" , startyear,"-",endyear),
               subtitle = paste0(chosendata[1,6],": ",input$towns)) +
          scale_x_discrete(breaks=c("2005","2010","2015","2020")) +
          xlab("Year") +
          ylab("Consumption")
      })
    }
    p_slopegraph <- cons_year %>% 
      mutate(year = factor(year)) %>%
      filter(year %in% c(startyear,endyear)) %>%
      newggslopegraph(year, mean_cons, type)
    p_slopegraph1 <- p_slopegraph + labs(title = "Household Electricity Consumption Between 2 Points of Time",
                                         subtitle = "",
                                         caption = "")
    output$slope <- renderPlot({p_slopegraph1})
    # sparkline -------------------------------------------------
    
    d_report <- chosendata %>%   
      filter(year %in% c(startyear:endyear)) %>%   
      mutate(`Category`= type) %>%   
      group_by(`Category`) %>%   
      summarise("Min" = min(consumption, na.rm = T),             
                "Max" = max(consumption, na.rm = T),
                "Average" = mean(consumption, na.rm = T)) 
    
    d_sparks <- chosendata %>%   
      filter(year %in% c(startyear:endyear)) %>%   
      mutate(`Category`= type) %>%   
      group_by(`Category`) %>%   
      summarize(`Monthly Consumption` = list(consumption))
    
    report_data = left_join(d_report, d_sparks) 
    
    output$sparktable <- renderTable({report_data %>% 
      gt() %>% 
      gt_plt_sparkline('Monthly Consumption')
    })
  })
  
  # consumption by dwelling type -------------------------------------------------
  
  observeEvent(c(input$slider_year2, input$slope_value2, input$towns2),{
    startyear <- input$slider_year2[1]
    endyear <- input$slider_year2[2]
    
    ## cycle plot2 -----------------------------------------------------------------
    
    select_cycle <- chosendata %>% 
      filter(type %in% input$towns) %>%
      mutate(year = factor(year, levels = startyear:endyear),
             month = factor(month, levels = 1:12))
    
    #Computing year average by months
    hline.data <- select_cycle %>%
      group_by(month) %>%
      summarise(avg_cons = mean(consumption))
    output$cycleplot <- renderPlotly({
      #Plotting cycle plot for electricity consumption per dwelling type
      ggplot() + 
        geom_line(data = select_cycle,
                  aes(x=year,y=consumption, group=month), colour = "black") +
        geom_hline(data = hline.data,
                   aes(yintercept=avg_cons),
                   linetype=6, 
                   colour="red", 
                   linewidth=0.5) +
        facet_grid(~month) +
        theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1)) +
        labs(title = paste("Cycleplot Shwoing Consumption (GWh) in Selected Towns" , startyear,"-",endyear),
             subtitle = paste0(chosendata[1,6],": ",input$towns)) +
        scale_x_discrete(breaks=c("2005","2010","2015","2020")) +
        xlab("") +
        ylab("Consumption, GWh")
    })
    
    # spark table 2--------------------------------------------------------------
    output$sparktable2 <- renderTable(dwelling %>%
                                        filter(year %in% c(startyear:endyear)) %>%
                                        group_by(DWELLING_TYPE) %>%
                                        summarise("Min" = min(consumption_GWh, na.rm = T),
                                                  "Max" = max(consumption_GWh, na.rm = T),
                                                  "Average" = mean(consumption_GWh, na.rm = T)
                                        ) %>%
                                        gt() %>%
                                        fmt_number(columns = 4,
                                                   decimals = 2))
    
    # line 2----------------------------------------------------------------------
    cons_yr <- dwelling %>% 
      filter(year %in% c(startyear:endyear)) %>% 
      filter(DWELLING_TYPE %in% input$towns2)
    
    output$dwelling2 <- renderPlotly({
      p <- ggplot(data = cons_yr, aes(x = date)) +
        geom_line(aes(y = consumption_GWh, colour = DWELLING_TYPE)) + 
        labs(title = paste0("Electricity Consumption by Dwelling Type"),
             x = "Year", y = "Consumption, GWh ") +
        scale_color_discrete(name="") +
        theme(legend.position="bottom")
      ggplotly(p, tooltip = c("x", "y", "colour")) %>% 
        layout(legend = list(orientation = "h", x = 0.1, y = -0.2))
    })
    
    
    
    # slope graph 2---------------------------------------------------------------
    
    if (input$slope_value2 == "sum") {
      cons_year <- cons_yr %>%
        group_by(DWELLING_TYPE, year) %>%
        summarise(mean_cons=round(sum(consumption_GWh),2))}
    if (input$slope_value2 == "average") {
      cons_year <- cons_yr %>%
        group_by(DWELLING_TYPE, year) %>%
        summarise(mean_cons=round(mean(consumption_GWh),2))}
    
    if (input$slope_value2 == "median") {
      cons_year <- cons_yr %>%
        group_by(DWELLING_TYPE, year) %>%
        summarise(mean_cons=round(median(consumption_GWh),2))}
    
    p_slopegraph <- cons_year %>% 
      mutate(year = factor(year)) %>%
      filter(year %in% c(startyear,endyear)) %>%
      newggslopegraph(year, mean_cons, DWELLING_TYPE)
    
    p_slopegraph1 <- p_slopegraph + labs(title = "Household Electricity Consumption Between 2 Points of Time",
                                         subtitle = "",
                                         caption = "")
    
    output$slope2 <- renderPlot({p_slopegraph1})
    
  })
  
  #  dtw ------------------------------------------------------------------------
  observeEvent(c(input$k2, input$method2, input$distance_func),{
    cluster_dtw <- tsclust(clus_matrix1[,-c(1)],
                           type = "h",
                           k=input$k2,
                           distance=input$distance_func,
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
                           .title = "Time Series Plot by cluster")
    
    ts <- ts %>%
      layout(hovermode="x",
             hoverlabel=list(font=list(size=7)))
    
    output$dtw <- renderPlotly( ts )
    
    dtw_cluster_t$Description <- toupper(dtw_cluster_t$Description)
    
    # Preparing the choropleth map
    mpsz_clus_dtw <- left_join(singapore, dtw_cluster_t, by = c("PLN_AREA_N" = "Description"))
    
    
    map2 <- tm_shape(mpsz_clus_dtw)+
      tmap_options(check.and.fix = TRUE)+
      tm_fill("cluster_group", id=paste("PLN_AREA_N"),
              style = "pretty",
              palette = "Greens") +
      tm_borders(alpha = 0.7)
    
    tmap_mode("view")
    
    output$dtwmap <- renderTmap(
      map2+tm_view(set.view = c(103.851959, 1.350270,11))
    )
    
  })
  
  # clustering dendro ----------------------------------------------------------
  clus_group1 <- clus[,-c(2)] %>%
    group_by(Description) %>%
    summarise_each(list(sum))
  
  # making "Description" the row name (index)
  row.names(clus_group1) <- clus_group1$Description
  
  # Making it into a matrix
  clus_matrix1 <- data.matrix(clus_group1)
  
  # plot
  observeEvent(c(input$k, input$method, input$distance, input$seriate),{
    
    output$dendro <- renderPlotly({
      heatmaply(clus_matrix1[,-c(1)],
                scale = "column",
                dist_method = input$distance,
                hclust_method = input$method,
                Colv=NA,
                seriate = input$seriate,
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
    clustering <- dist(normalize(clus_group1[, -1]), method=input$distance)
    out <- dend_expend(clus, dist_methods = input$distance)
    out <- out$performance
    # clustering dendex --------------------------------------------------------
    output$dendextend <- renderDataTable(
      
      out
    )
    
    # clustering number k ------------------------------------------------------
    clust2 <- hclust(clustering, method = input$method)
    num_k <- find_k(clust2)
    output$numberk <- renderPlot(
      plot(num_k)
    )
    
    # clustering map -----------------------------------------------------------
    num_clus <- cutree(clust2, k=input$k)
    clus_hc <- cbind(clus_group1, cluster = as.factor(num_clus))
    
    clus_hc$Description <- toupper(clus_hc$Description)
    
    # Preparing the choropleth map
    mpsz_clus <- left_join(singapore, clus_hc, by = c("PLN_AREA_N" = "Description"))
    map <- tm_shape(mpsz_clus)+
      tmap_options(check.and.fix = TRUE)+
      tm_fill("cluster", id=paste("PLN_AREA_N"),
              style = "pretty",
              palette = viridis(input$k)) +
      tm_borders(alpha = 0.7)
    
    current.mode <- tmap_mode("plot")
    
    tmap_mode("view")
    output$map <- renderTmap(
      map+ tm_view(set.view = c(103.851959, 1.350270,11))
    )
  })
  
  # images -----------------------------------------------------------
  output$page1 <- renderImage({
    
    list(src = "page 1.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page2 <- renderImage({
    
    list(src = "page 2.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page3 <- renderImage({
    
    list(src = "page 3.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page4 <- renderImage({
    
    list(src = "page 4.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page5 <- renderImage({
    
    list(src = "page 5.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page6 <- renderImage({
    
    list(src = "page 6.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page7 <- renderImage({
    
    list(src = "page 7.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page8 <- renderImage({
    
    list(src = "page 8.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  output$page9 <- renderImage({
    
    list(src = "page 9.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  # text -----------------------------------------------------------------------
  
  output$remark <- renderText({
    
    paste("Please note that the data presented for the year 2022 only covers 
          the period of January to June, and therefore may not reflect the full 
          year's trends or statistics.")
  })
  
  output$description1 <- renderText({
    paste("Select a factor below to study the differences in mean consumption between the variables in the selected factor.")
  })
  
  output$description2 <- renderText({
    paste("Select a region below to study the differences in mean consumption between towns located in the selected region.
")
  })
}
shinyApp(ui = ui, server = server)
