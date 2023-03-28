
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


# reading the map file
mpsz <- st_read(dsn = 'master-plan-2014-subzone-boundary-web-shp',
                layer = 'MP14_SUBZONE_WEB_PL',
                crs = 3414)

# Import the area grid data.
area_grid <- read_csv("data/areagrid.csv")
singapore <- st_transform(mpsz, 4326)

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
                   
                   ### 1.3 Consumption by Dwelling type and Town -------------------
                   tabPanel("Consumption by Dwelling type and Town",
                            fluidRow(
                              column(4, sliderInput("slider_year", "Select year",min = 2005,
                                          max = 2022, step = 1, round = FALSE,
                                          value =  c(2005, 2022))),
                              column(4, radioButtons("type", "Select parameter", 
                                                     choices = c("By town", "By dwelling type"), 
                                                     inline = TRUE)),
                              column(4, radioButtons("slope_value", "select value", 
                                                     choices = c("sum", "average", "median" ), 
                                                     inline = TRUE))
                              
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

                         ### 3.2 correlation ---------------------------------------
                          tabPanel("ANOVA betweenstats",
                                   fluidPage(
                                     fluidRow(
                                       column(width = 9, plotlyOutput("dwellingstat3",height=400))
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
                                      ),
                                      column(12, plotlyOutput("arima_plot")),
                                      column(5,
                                             verbatimTextOutput("arimatext")),
                                    fluidRow(
                                      column(6, plotOutput("arima",height="500px")),
                                      column(width = 6, plotOutput("arima_plot",height=400))
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

  
  # table ----------------------------------------------------------------------
  observeEvent((input$SelectTable),{
    
    if(input$SelectTable == "T2.3")  {tabletext <- T2.3}
    if(input$SelectTable == "T3.4")  {tabletext <- T3.4}
    if(input$SelectTable == "T3.5")  {tabletext <- T3.5}
    output$table <- renderDataTable(tabletext)
  })
  
  # arima ----------------------------------------------------------------------
  
  
  # ------------- peak system demand ------------- #

  
  # cycle plot -----------------------------------------------------------------

  
  # ----------------- consumption by dwelling type ------------------ #
  
  
  # table dwelling -------------------------------------------------------------
  
  
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
  
    # spark table --------------------------------------------------------------
   
    
    # slope graph---------------------------------------------------------------

  
  
  ##  dtw ------------------------------------------------------------------------
 
  
  # clustering dendro ----------------------------------------------------------
  
  
  
}

shinyApp(ui = ui, server = server)
