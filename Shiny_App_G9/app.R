library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(ggstatsplot)
library(psych)
library(lubridate)
library(ggrepel)
library(plotly)
library(tidyverse)

## Read data file
T2.3 <- readRDS(file = "RDS/T2-3.rds")
T2.6 <- readRDS(file = "RDS/T2-6.rds")
T3.4 <- readRDS(file = "RDS/T3-4.rds")
T3.5 <- readRDS(file = "RDS/T3-5.rds")
T3.6 <- readRDS(file = "RDS/T3-6.rds")
T3.7 <- readRDS(file = "RDS/T3-7.rds")
T3.8 <- readRDS(file = "RDS/T3-8.rds")
T3.9 <- readRDS(file = "RDS/T3-9.rds")
T5.1 <- readRDS(file = "RDS/T5-1.rds")
T5.2 <- readRDS(file = "RDS/T5-2.rds")
T5.3 <- readRDS(file = "RDS/T5-3.rds")
T5.4 <- readRDS(file = "RDS/T5-4.rds")
T5.5 <- readRDS(file = "RDS/T5-5.rds")

# wrangling data
consumption <- T3.5
consumption <- consumption %>%
  mutate(kwh_per_acc = as.numeric(kwh_per_acc)) %>%
  mutate(year = as.character(year))


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
                        tabPanel("Geofacet",
                                 fluidPage(
                                   fluidRow(
                                     column(3,wellPanel(
                                       pickerInput(
                                         inputId = "GeofacetYear", label = "Select Year", choices = years,
                                         selected = "2022", options = list(`actions-box` = TRUE), multiple = F),
                                       
                                       checkboxGroupInput("GeofacetRegion", label = "Select Region",
                                                          choices = regions, selected = regions)
                                     )),
                                     
                                     column(9)
                                   )
                                 )
                        ),
                        
                        # ====================== Lineplot ====================== #
                        
                        tabPanel("Lineplot",
                                 fluidPage(
                                   fluidRow(
                                     column(4,wellPanel(
                                       sliderInput("slider_time", "Select date range",min = as.Date("2021-02-24"), 
                                                   max = as.Date("2021-04-24"), 
                                                   value =  c(as.Date("2021-02-24"),as.Date("2021-03-03") )),
                                       
                                       dateRangeInput("daterange", "Input date range", 
                                                      start = as.Date("2021-02-24"),end = as.Date("2021-03-03") ),
                                       
                                       checkboxGroupInput("LineVariable",
                                                          label = "Select Variables",
                                                          choices = c("price", "consumption", "supply"),
                                                          selected = c("price", "consumption", "supply"))
                                     )),
                                     
                                     column(8)
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
                                                   value =  0.001)
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
                        
                        # ==================== Electricity consumption ==================== #
                        tabPanel("Electricity consumption",
                                 fluidPage(
                                   fluidRow(
                                     column(3, wellPanel(
                                       pickerInput(inputId = "BoxYear", label = "Select Year", 
                                                   choices = years, selected = "2022", 
                                                   options = list(`actions-box` = TRUE), multiple = F)
                                     )),
                                     
                                     column(width = 9, plotlyOutput("time",height=400))
                                   ) )
                        ),
                        
                        # ******************** End Electricity consumption ******************** #
                        
                        # ======================= Oil consumption ======================= #
                        tabPanel("Oil consumption",
                                 fluidPage(
                                   fluidRow(
                                     column(3, wellPanel(
                                       pickerInput(inputId = "BoxYear", label = "Select Year", 
                                                   choices = years, selected = "2022", 
                                                   options = list(`actions-box` = TRUE), multiple = F)
                                     )),
                                     
                                     column(width = 9, plotlyOutput("oilconsump",height=400))
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
                                     column(4,wellPanel(
                                       selectizeInput('SelectColumn', "Select Column", choices = c("A","B","C") ,multiple = TRUE)
                                     ) ),
                                     
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
    
    tabPanel("ABOUT", tabName = "about", icon = icon("info"))
    
    # ****************************** END ABOUT ****************************** #    
    
  )
)

server = function(input, output, session) {
  
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
}


shinyApp(ui = ui, server = server)
