
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
# library(reactablefmtr)
# library(readr)
library(sf)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(stats)
# library(tibble)
# library(tidymodels)
# library(tidyquant)
library(tidyr)
library(tidyverse)
library(timetk)
library(tmap)
# library(treemap)
# library(TSclust)
# library(tseries)
# library(tsibble)
# library(tsibbletalk)
# library(zoo)


# READ DATA ---------------------------------------------------------------
## Read compressed data file
T2.3 <- readRDS(file = "RDS/T2-3.rds") # Peak System Demand
T3.4 <- readRDS(file = "RDS/T3-4.rds") # Total Household Electricity Consumption by Dwelling Type
T3.5 <- readRDS(file = "RDS/T3-5.rds") # Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type
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
            "Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type" = "T3.5")
type <- c("parametric", "nonparametric", "robust", "bayes")
towns <- unique(chosendata$type)
dwellingtype <- unique(dwelling$DWELLING_TYPE)
introtext <- "Welcome to our Singapore Electricity Consumption Visualization Application. This interactive tool allows you to explore and analyze the electricity consumption patterns in Singapore over the past decade. With data sourced from official government records, our application presents a range of visually appealing and informative charts and graphs, providing insights into trends and patterns in energy consumption across various sectors, including residential, commercial, and industrial. Whether you're a policy maker, a researcher, or a concerned citizen, our tool provides a powerful platform to understand and interpret the dynamics of energy use in Singapore, and to make informed decisions that can shape a sustainable future for all."
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
                                                        "free y-axis" = "free_y"),
                                            inline = T),
                               plotOutput("geo", height = 800)
                             )),
                    
                    ### 1.3 Consumption by Town -----------------------------------
                    tabPanel("Consumption by Town",
                             fluidRow(
                               column(3, sliderInput("slider_year", "Select year",min = 2005,
                                                     max = 2022, step = 1, round = FALSE,
                                                     value =  c(2005, 2022))),
                               column(3, pickerInput("towns", "Select town", choices = towns,
                                                     selected = c("Ang Mo Kio", "Clementi"),
                                                     multiple = TRUE)),
                               column(3, radioButtons("type", "Select parameter",
                                                      choices = c("By town", "By dwelling type"),
                                                      inline = TRUE)),
                               column(3, radioButtons("slope_value", "select value",
                                                      choices = c("sum", "average", "median" ),
                                                      inline = TRUE))
                             ),
                             fluidRow(
                               column(6, plotOutput("dwelling", height = 400)),
                               column(6, plotOutput("slope",height=400))
                             ),
                             fluidRow(
                               column(6, plotlyOutput("cycleplot")),
                               column(6, tableOutput("sparktable"))
                             )
                    ),
                    ### 1.4 Consumption by Dwelling type and dwelling type -------
                    tabPanel("Consumption by Dwelling type",
                             fluidRow(
                               column(3, sliderInput("slider_year2", "Select year",min = 2005,
                                                     max = 2022, step = 1, round = FALSE,
                                                     value =  c(2005, 2022))),
                               column(3, pickerInput("towns2", "Select dwelling type", choices = dwellingtype,
                                                     selected = c("3-room", "4-room"),
                                                     multiple = TRUE)),
                               column(3, radioButtons("type2", "Select parameter",
                                                      choices = c("By town", "By dwelling type"),
                                                      inline = TRUE)),
                               column(3, radioButtons("slope_value2", "select value",
                                                      choices = c("sum", "average", "median" ),
                                                      inline = TRUE))
                             ),
                             fluidRow(
                               column(6, plotOutput("dwelling2", height = 400)),
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
                                column(3,pickerInput("method", "Choose Clustering Method",
                                                     choices = c("ward.D", "ward.D2", "single",
                                                                 "complete", "average", "mcquitty",
                                                                 "median", "centroid"),
                                                     selected = "complete")),
                                
                                column(3,pickerInput("distance", "Choose Distance Method",
                                                     choices = c("euclidean", "maximum", "manhattan",
                                                                 "canberra", "binary", "minkowski"))),
                                
                                column(3,numericInput("k", "Choose number of cluster",
                                                      min = 1, max = 10, value = 2))
                              ),
                              
                              fluidRow(
                                column(4, dataTableOutput("dendextend"),
                                       plotOutput("numberk", height = "350px")),
                                column(8, plotlyOutput("dendro", height = "400px"),
                                       tmapOutput("map")
                                )
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
                         
                         ### 3.1 ANOVA boxplot ---------------------------------------------
                         tabPanel("ANOVA boxplot",
                                  fluidPage(
                                    fluidRow(
                                      column(5,
                                             pickerInput(inputId = "anovainput",
                                                         label = "Select Parameter",
                                                         choices = c("dwelling_type", "Region", "year"),
                                                         selected = "dwelling_type",
                                                         options = list(`actions-box` = TRUE),
                                                         multiple = F),
                                             verbatimTextOutput("anovastat")
                                      ),
                                      column(7, plotOutput("dwellingstat"))
                                    ),
                                    
                                    fluidRow(
                                      column(5,
                                             pickerInput(inputId = "region",
                                                         label = "Select Region",
                                                         choices = regions,
                                                         selected = "Central Region",
                                                         options = list(`actions-box` = TRUE),
                                                         multiple = F),
                                             verbatimTextOutput("anovastat2")
                                      ),
                                      column(7, plotOutput("dwellingstat2"))
                                    )
                                  )),
                         
                         ### 3.2 ANOVA betweenstats ---------------------------------------
                         tabPanel("ANOVA betweenstats",
                                  fluidPage(
                                    fluidRow(
                                      pickerInput(
                                        inputId = "anovainput2",
                                        label = "Select Parameter",
                                        choices = c("dwelling_type", "Region", "year"),
                                        selected = "dwelling_type",
                                        options = list(`actions-box` = TRUE),
                                        multiple = F),
                                      "Please wait for a moment as this page may take some time to process."
                                    ),
                                    fluidRow(plotOutput("dwellingstat3")),
                                    fluidRow(
                                      pickerInput(inputId = "region2",
                                                  label = "Select Region",
                                                  choices = regions,
                                                  selected = "Central Region",
                                                  options = list(`actions-box` = TRUE),
                                                  multiple = F)
                                    ),
                                    fluidRow(plotOutput("dwellingstat4", height = "1000px"))
                                  )
                         )
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
                                             pickerInput(inputId = "timemodel",
                                                         label = "Select Model",
                                                         choices = c("ARIMA", "ETS", "TSLM", "AR"),
                                                         selected = "ARIMA",
                                                         multiple = F),
                                             numericInput("year", "Months to forecast ahead",
                                                          min = 1, max = 24, step=1, value = 3)
                                      ),
                                      column(3,
                                             numericInput("arima_d", "Order of differencing", value=1),
                                             numericInput("arima_d2", "Order of seasonal differencing", value=2),
                                             checkboxInput("arima_d3", "Allow drift", value = FALSE)
                                      )
                                    )
                                    # ,
                                    #   column(12, plotlyOutput("arima_plot")),
                                    #   column(5,
                                    #          verbatimTextOutput("arimatext")),
                                    # fluidRow(
                                    #   column(6, plotOutput("arima",height="500px")),
                                    #   column(width = 6, plotOutput("arima_plot",height=400))
                                    #   )
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
    if(input$SelectTable == "T3.4")  {tabletext <- T3.4}
    if(input$SelectTable == "T3.5")  {tabletext <- T3.5}
    output$table <- renderDataTable(tabletext)
  })
  
  # arima ----------------------------------------------------------------------
  
  
  
  
  # cycle plot -----------------------------------------------------------------
  
  
  # ----------------- consumption by dwelling type ------------------ #
  
  
  # table dwelling -------------------------------------------------------------
  
  
  # anova boxplot-----------------------------------------------------------------
  observeEvent(input$anovainput,{
    if(input$anovainput == "Region"){
      output$dwellingstat <- renderPlot({
        consumption %>%
          mutate(class = fct_reorder(Region, kwh_per_acc, .fun='mean')) %>%
          ggplot( aes(x=reorder(Region, kwh_per_acc), y=kwh_per_acc)) +
          geom_boxplot() +
          stat_summary(fun.y=mean, geom="point", color="red") +
          theme(legend.position="none") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          ggtitle("Boxplot of consumption per Region")
        
      })
      
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ Region, data = consumption))
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
          ggtitle("Boxplot of consumption per Year")
      })
      
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ year, data = consumption))
      })
    }
    
    if(input$anovainput == "dwelling_type"){
      output$dwellingstat <- renderPlot({
        consumption %>%
          mutate(class = fct_reorder(dwelling_type, kwh_per_acc, .fun='mean')) %>%
          ggplot( aes(x=reorder(dwelling_type, kwh_per_acc), y=kwh_per_acc)) +
          geom_boxplot() +
          stat_summary(fun.y=mean, geom="point", color="red") +
          theme(legend.position="none") +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
          ggtitle("Boxplot of consumption per Dwelling type")
      })
      
      output$anovastat <- renderPrint({
        summary(aov(kwh_per_acc ~ dwelling_type, data = consumption))
      })
      
    }
  })
  
  # anova boxplot2 ---------------------------------------------------------------------
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
        ggtitle("Boxplot of consumption per Region")
    })
    
    output$anovastat2 <- renderPrint({
      consumption %>% 
        filter(Region == input$region)
      summary(aov(kwh_per_acc ~ Description, data = consumption))
    })
  })
  
  # anova betweenstat -----------------------------------------------------------
  observeEvent(input$anovainput2,{
    if(input$anovainput2 == "Region") {
      output$dwellingstat3 <- renderPlot({
        ggbetweenstats(
          data = consumption,
          x = Region,
          y = kwh_per_acc,
          messages = FALSE
        )
      })}
    
    if(input$anovainput2 == "year") {
      output$dwellingstat3 <- renderPlot({
        ggbetweenstats(
          data = consumption,
          x = year,
          y = kwh_per_acc,
          messages = FALSE
        )
      })}
    
    if(input$anovainput2 == "Dwelling_type") {
      output$dwellingstat3 <- renderPlot({
        ggbetweenstats(
          data = consumption,
          x = Dwelling_type,
          y = kwh_per_acc,
          messages = FALSE
        )
      })}
  })
  
  # anova betweenstat2 ----------------------------------------------------------
  observeEvent(input$region2,{
    output$dwellingstat4 <- renderPlot({
      consumption %>%
        filter(Region == input$region2) %>%
        ggbetweenstats(
          x = Description,
          y = kwh_per_acc,
          messages = FALSE
        )
    })
  })
  
  # sparklines -----------------------------------------------------------------
  
  observeEvent(c(input$slider_year, input$slope_value, input$towns),{
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
    
    # line ----------------------------------------------------------------------
    chosendata <- chosendata %>%
      filter(year %in% c(startyear:endyear)) %>% 
      filter(type %in% input$towns)
    
    output$dwelling <- renderPlot({
      ggplot(data = chosendata, aes(x = date)) +
        geom_line(aes(y = consumption, colour = type)) + 
        labs(title = paste0("Electricity Consumption by ",chosendata[1,6]),
             x = "Year", y = paste0("Consumption, GWh ")) +
        scale_color_discrete(name="") +
        theme(legend.position="bottom")
    })
    
    # slope graph---------------------------------------------------------------
    cons_yr <- chosendata
    
    if (input$slope_value == "sum") {
      cons_year <- cons_yr %>%
        group_by(type, year) %>%
        summarise(mean_cons=round(sum(consumption),2))}
    if (input$slope_value == "average") {
      cons_year <- cons_yr %>%
        group_by(type, year) %>%
        summarise(mean_cons=round(mean(consumption),2))}
    
    if (input$slope_value == "median") {
      cons_year <- cons_yr %>%
        group_by(type, year) %>%
        summarise(mean_cons=round(median(consumption),2))}
    
    p_slopegraph <- cons_year %>% 
      mutate(year = factor(year)) %>%
      filter(year %in% c(startyear,endyear)) %>%
      newggslopegraph(year, mean_cons, type)
    
    p_slopegraph1 <- p_slopegraph + labs(title = "Monthly Household Electricity Consumption between 2 points of time",
                                         subtitle = "",
                                         caption = "Source:EMA.gov.sg")
    
    output$slope <- renderPlot({p_slopegraph1})
  })
  
  # sparklines -----------------------------------------------------------------
  
  observeEvent(c(input$slider_year2, input$slope_value2, input$towns2),{
    startyear <- input$slider_year2[1]
    endyear <- input$slider_year2[2]
    
    # line 2----------------------------------------------------------------------
    cons_yr <- dwelling %>% 
      filter(year %in% c(startyear:endyear)) %>% 
      filter(DWELLING_TYPE %in% input$towns2)
    
    output$dwelling2 <- renderPlot({
      ggplot(data = cons_yr, aes(x = date)) +
        geom_line(aes(y = consumption_GWh, colour = DWELLING_TYPE)) + 
        labs(title = paste0("Electricity Consumption by dwelling type"),
             x = "Year", y = paste0("Consumption, GWh ")) +
        scale_color_discrete(name="") +
        theme(legend.position="bottom")
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
    
    p_slopegraph1 <- p_slopegraph + labs(title = "Monthly Household Electricity Consumption between 2 points of time",
                                         subtitle = "",
                                         caption = "Source:EMA.gov.sg")
    
    output$slope2 <- renderPlot({p_slopegraph1})
    
  })
  #  dtw ------------------------------------------------------------------------
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
  
  # clustering dendro ----------------------------------------------------------
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
    clustering <- dist(normalize(clus_group1[, -1]), method=input$distance)
    
    # clustering dendex --------------------------------------------------------
    output$dendextend <- renderDataTable(
      dend_expend(clustering)[[3]]
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
    output$map <- renderTmap(
      
      tm_shape(mpsz_clus)+
        tmap_options(check.and.fix = TRUE)+
        tm_fill("cluster", id=paste("PLN_AREA_N"),
                style = "pretty",
                palette = viridis(input$k)) +
        tm_borders(alpha = 0.7)
    )
  })
  
}

shinyApp(ui = ui, server = server)
