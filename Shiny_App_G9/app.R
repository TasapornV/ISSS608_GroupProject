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

library(ggrepel)
library(ggstatsplot)
library(ggplot2)
library(plotly)

library(tseries)
library(feasts)
library(fable)
library(forecast)
library(devtools)


## Read compressed data file
T2.3 <- readRDS(file = "RDS/T2-3.rds") # Peak System Demand
T2.6 <- readRDS(file = "RDS/T2-6.rds") # Market Share of Electricity Generation
T3.4 <- readRDS(file = "RDS/T3-4.rds") # Total Household Electricity Consumption by Dwelling Type
T3.5 <- readRDS(file = "RDS/T3-5.rds") # Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type
T3.6 <- readRDS(file = "RDS/T3-6.rds") # Market Share for Natural Gas Retail
T3.7 <- readRDS(file = "RDS/T3-7.rds") # Natural Gas Consumption by Sub-Sector
T3.8 <- readRDS(file = "RDS/T3-8.rds") # Total Household Town Gas Consumption by Dwelling Type
T3.9 <- readRDS(file = "RDS/T3-9.rds") # Average Monthly Household Town Gas Consumption by Planning Area & Dwelling Type
T5.1 <- readRDS(file = "RDS/T5-1.rds") # Electricity and Gas Tariffs
T5.2 <- readRDS(file = "RDS/T5-2.rds") # Monthly Electricity Tariffs (Low Tension Tariffs)
T5.3 <- readRDS(file = "RDS/T5-3.rds") # Annual Electricity Tariffs by Components (Low Tension Tariffs)
T5.4 <- readRDS(file = "RDS/T5-4.rds") # Average Monthly Uniform Singapore Energy Prices (USEP)
T5.5 <- readRDS(file = "RDS/T5-5.rds") # Monthly Town Gas Tariffs

# wrangling data
consumption <- T3.5
consumption <- consumption %>%
  mutate(kwh_per_acc = as.numeric(kwh_per_acc)) %>%
  mutate(year = as.character(year)) %>%
  mutate('date' = make_date(year=year, month=month))

## Set up parameter
years <- c("2022","2021", "2020", "2019", "2018", "2017")
regions <- c( "Central Region", "NorthEast Region", "East Region", "North Region", "West Region")
tables <- c("Peak System Demand" = "T2.3",
            "Market Share of Electricity Generation" = "T2.6",
            "Total Household Electricity Consumption by Dwelling Type" = "T3.4",
            "Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type" = "T3.5",
            "Market Share for Natural Gas Retail" = "T3.6",
            "Natural Gas Consumption by Sub-Sector" = "T3.7",
            "Total Household Town Gas Consumption by Dwelling Type" = "T3.8",
            "Average Monthly Household Town Gas Consumption by Planning Area & Dwelling Type" = "T3.9",
            "Electricity and Gas Tariffs" = "T5.1",
            "Monthly Electricity Tariffs (Low Tension Tariffs)" = "T5.2",
            "Annual Electricity Tariffs by Components (Low Tension Tariffs)" = "T5.3",
            "Average Monthly Uniform Singapore Energy Prices (USEP)" = "T5.4",
            "Monthly Town Gas Tariffs" = "T5.5")
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

ui = fluidPage(
  theme = shinytheme("slate"),
  headerPanel(title = "Singapore Energy Consumption"),
  
  navlistPanel(
    widths = c(3, 9),
    
    # ========================== OVERVIEW ========================== #      
    tabPanel("OVERVIEW",tabName = "overview",icon = icon("chalkboard-user"),
             navbarPage("OVERVIEW", 
                        # =================== Introduction =================== # 
                        tabPanel("Introduction",introtext),
                        
                        # ====================== Boxplot ====================== #
                        tabPanel("Boxplot",
                                 fluidPage(
                                   fluidRow(
                                     column(3,wellPanel(
                                       pickerInput(inputId = "BoxYear", label = "Select Year", choices = years, selected = "2022", 
                                                   options = list(`actions-box` = TRUE), multiple = F),
                                       
                                       checkboxGroupInput("BoxRegion", label = "Select Region", choices = regions, selected = regions)
                                     )),
                                     
                                     column(width = 9, plotlyOutput("boxplot",height=400))
                                   )
                                 )
                        ),
                        
                        # ====================== Geofacet ====================== #
                        # T4.3 
                        tabPanel("Geofacet",
                                 fluidPage(
                                   fluidRow(
                                     column(12,
                                            fluidPage(
                                              fluidRow(
                                                column(12,plotOutput("geo", height = 800))
                                              )
                                            )
                                     )
                                   )
                                 )
                        ),
                        
                        # ====================== Lineplot ====================== #
                        
                        tabPanel("Lineplot",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 12, plotlyOutput("lineplot",height=400))
                                   )
                                 )
                        ),
                        
                        # ====================== Bar chart ====================== #
                        
                        tabPanel("Barchart",
                                 fluidPage(
                                   fluidRow(
                                     column(3,wellPanel(
                                       radioButtons("BarData",
                                                    label = "Market Share of",
                                                    choices = c("Electricity", "Electricity Generation", "Natural Gas"),
                                                    selected = "Electricity")
                                     )),
                                     
                                     column(9)
                                   )
                                 )
                        )
                        # ******************* END Bar chart ******************* #
             )
    ),
    
    # ************************ END OVERVIEW ************************ #       
    
    # ========================== CLUSTERING ========================== #
    
    tabPanel("CLUSTERING", tabName = "clustering", icon = icon("circle-nodes"),
             navbarPage("CLUSTERING", 
                        tabPanel("Hierachical Clustering"),
                        tabPanel("DTW"),
                        tabPanel("Time Series Clustering")
             )
    ),
    
    # ************************ END CLUSTERING ************************ #
    
    # ========================== INFERENTIAL ========================== #     
    
    tabPanel("INFERENTIAL STATISTICS", tabName = "inferential", icon = icon("magnifying-glass-chart"),
             navbarPage("INFERENTIAL STATISTICS", 
                        
                        # ====================== Anova ====================== #
                        tabPanel("ANOVA",
                                 fluidPage(
                                   fluidRow(
                                     column(3, wellPanel(
                                       pickerInput(inputId = "AnovaVar1", label = "Select Variable", 
                                                   choices = years, selected = "2022", 
                                                   options = list(`actions-box` = TRUE), multiple = F
                                       ),
                                       
                                       pickerInput(inputId = "AnovaVar2", label = "Select Dependent Variable", 
                                                   choices = years, selected = "2022", 
                                                   options = list(`actions-box` = TRUE), multiple = F
                                       ),
                                       
                                       sliderInput("slider_significant", "Significance level (alpha):",min = 0.0001, 
                                                   max = 0.01, 
                                                   value =  0.001),
                                       
                                       radioButtons("anovatype",
                                                    label = "Select test type",
                                                    choices = type,
                                                    selected = "parametric")
                                     )),
                                     
                                     column(width = 9, plotlyOutput("anova",height=400))
                                   )
                                 )),
                        # ====================== End Anova ====================== #
                        
                        # ====================== Correlation ====================== #
                        tabPanel("Correlation Analysis",
                                 fluidPage(
                                   fluidRow(
                                     column(3, wellPanel(
                                       pickerInput(inputId = "CorreSelect", label = "Select Dataset", 
                                                   choices = years, selected = "2022", 
                                                   options = list(`actions-box` = TRUE), multiple = F)
                                     )),
                                     
                                     column(width = 9, plotlyOutput("correlation",height=400))
                                   )
                                 )
                        )
                        
                        # ******************** End Correlation ******************** #
             )
    ),
    
    # ************************* END INFERENTIAL ************************* #        
    
    # ========================== TIME SERIES ========================== #     
    
    tabPanel("TIME SERIES FORECASTING", tabName = "time_series", icon = icon("chart-line"),
             navbarPage("TIME SERIES FORECASTING",
                        
                        # ==================== Trend Prediction ==================== #
                        tabPanel("Trend Prediction",
                                 fluidPage(
                                   fluidRow(
                                     column(width = 5, 
                                            numericInput("arima_d", "input order of differencing", value=1),
                                            numericInput("arima_d2", "input order of seasonal differencing", value=2),
                                            checkboxInput("arima_d3", "allow drift", value = FALSE),
                                            verbatimTextOutput("arimatext")),
                                     column(width = 7, plotOutput("arima",height=350)),
                                     column(width = 12, plotOutput("arima_plot",height=400))
                                   ) )
                        ),
                        
                        # ******************** End Trend Prediction ******************** #
                        
                        # ======================= Slope Graph ======================= #
                        tabPanel("Slope Graph",
                                 fluidPage(
                                   fluidRow(
                                     column(3, wellPanel(
                                       pickerInput(inputId = "BoxYear", label = "Select Year", 
                                                   choices = years, selected = "2022", 
                                                   options = list(`actions-box` = TRUE), multiple = F)
                                     )),
                                     
                                     column(width = 9, plotOutput("oilconsump",height=200))
                                   ) )
                        )
                        
                        # ******************** End Oil consumption ******************** #
             )
    ),
    
    # *************************** END TIME SERIES *************************** #
    
    # =============================== DATA =============================== #     
    
    tabPanel("DATA", tabName = "data", icon = icon("table"),
             navbarPage("DATA",
                        tabPanel("data table",
                                 fluidPage(
                                   fluidRow(
                                       column(8, wellPanel(
                                       radioButtons("SelectTable",
                                                    label = "Select Data",
                                                    choices = tables,
                                                    selected = "Electricity")
                                     ) ),
                                     
                                     column(12, dataTableOutput("table"))
                                   )
                                 ))
             )
    ),
    
    # ******************************* END DATA ******************************* #    
    
    # =============================== ABOUT =============================== #    
    
    tabPanel("ABOUT", tabName = "about", icon = icon("info")),
    
    # ****************************** END ABOUT ****************************** #    
    tabPanel(wellPanel(
      sliderInput("slider_time", "Select date range",min = as.Date("2021-02-24"), 
                  max = as.Date("2021-04-24"), 
                  value =  c(as.Date("2021-02-24"),as.Date("2021-03-03") )),
      
      dateRangeInput("daterange", "Input date range", 
                     start = as.Date("2021-02-24"),end = as.Date("2021-03-03") )
    ))
  )
)

server = function(input, output, session) {
  
  # -------------------- Geofacet ------------------- #
  
  output$geo <- renderPlot ({
    geofacet <- geofacet %>%
      filter(month != "Annual" & 
               year > 2017 & 
               dwelling_type != "Overall" &
               !str_detect(Description,"Region|Pioneer|Overall")) %>% 
      group_by(year, dwelling_type, Description ) %>%
      summarise(avgprice = mean(kwh_per_acc, na.rm = TRUE),
                medprice = median(kwh_per_acc, na.rm = TRUE))%>%
      ungroup()
    
    # merge table with town name
    geofacet_gas_consump <- inner_join(geofacet, area_grid,
                                       by = c("Description" = "name"))
    
    common_grid <- area_grid[area_grid$name %in% unique(geofacet$Description),]
    ggplot(geofacet_gas_consump, aes(x = year, y = avgprice)) +
      geom_line(aes(color = as.factor(dwelling_type))) +
      # scale_x_date(date_labels = "%b") +
      facet_geo(~Description, grid = common_grid) +
      labs(title = "Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type") +
      theme(plot.title = element_text(size=22),
            axis.text.x = element_text(size = 10, angle = 45),
            axis.text.y = element_text(size = 10),
            strip.text = element_text(size = 10),
            legend.position = "right")
  })
  
  # --------------------- Table --------------------- #
  observeEvent((input$SelectTable),{
               if(input$SelectTable == "T2.3") {tabletext <- T2.3}
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
               }
               
  )
  
  # ---------------- ANOVA ------------------- #
  # observeEvent(input$anovatype, input$SelectTable, {
  # output$anova <- renderPlotly(
  #   ggbetweenstats(input$SelectTable,
  #                  x = "DWELLING_TYPE",
  #                  y = "consumption_GWh",
  #                  type = input$anovatype
  #   )
  # )
  # })
 
  # ----------------- Box plot ------------------ #
  
  output$boxplot <- renderPlotly({
    ggplotly(
      consumption %>%
        group_by(year) %>%
        # filter(year == "2022") %>%
        ggplot(mapping = aes(x = year, y = kwh_per_acc)) +
        # Make grouped box plot
        geom_boxplot(aes(fill = as.factor(Region)), color = "grey") +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_fill_viridis_d(option = "C") +
        labs(title = "Average consumption per year by Region", y="kwh per acc", fill = "Region")
    )
  }
  )
  
  # ----------------- Lineplot ------------------ #
  
  output$lineplot <- renderPlotly({
    consumption %>%
      group_by(date) %>%
      summarise(avg = mean(kwh_per_acc, na.rm = TRUE)) %>%
      plot_ly(
        type="scatter",
        x=~date,
        y=~avg,
        mode="lines") %>%
      layout(title=list(text="<b>Average Singapore Energy Consumption</b>"),
             xaxis = list(title="Date"),
             yaxis = list(title="Average Consumption"))
  }
  )
  
  # ------------------ ARIMA -------------------- #
arima <- T2.3
    arima$Date <- yearmonth(as.yearmon(paste(arima$year, arima$mth), "%Y %m"))
    arima_ts <- ts(data=arima$peak_system_demand_mw)

observeEvent(c(input$arima_d,input$arima_d2, input$arima_d3), {
  output$arima <- renderPlot({
    arima_arima = auto.arima(arima_ts, d = input$arima_d, D = input$arima_d2, allowdrift = input$arima_d3)
    plot(forecast(arima_arima))
  })
  output$arimatext <- renderPrint(arima_arima)



arima_tsbl  = as_tsibble(arima)
full_arima = arima_tsbl %>%
  fill_gaps() %>% 
  tidyr::fill(peak_system_demand_mw, .direction = "down")


output$arima_plot <- renderPlot({
  full_arima %>%
    gg_tsdisplay(difference(peak_system_demand_mw), plot_type='partial')
  })
})
}

shinyApp(ui = ui, server = server)
