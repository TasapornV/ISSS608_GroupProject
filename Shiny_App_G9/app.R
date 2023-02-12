library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)


## Read data file

## Set up parameter
tickers <- c("2022","2021", "2020", "2019", "2018", "2017")
regions <- c( "Central Region", "NorthEast Region", "East Region", "North Region", "West Region")

ui = fluidPage(theme = shinytheme("slate"),
               headerPanel(title = "Singapore Energy Consumption"),
               dashboardSidebar(
                 navlistPanel(widths = c(3, 9),
                              
                              tabPanel("OVERVIEW",tabName = "overview",icon = icon("dashboard"),
                                       navbarPage("OVERVIEW", 
                                                  tabPanel("Introduction",
                                                           "Singapore has progressively moved towards an open electricity market since 2001 to ensure a reliable energy supply and promote effective competition in the energy market.

However, it is challenging to analyze the energy market and make informed decisions due to several variable components that could influence the market. First, the price of electricity in the wholesale market fluctuates depending on the electricity demand and supply, which are dependent on consumption patterns as well as gas industry respectively. About 95% of electricity is generated using natural gas. Next, there are also multiple parties (Electricity Market Authority, Power generation companies, Electricity Retailers, etc) involved in the electricity market.

To address this challenge, we build this RShinny app to provide relevant stakeholders with means to analyse and understand the data with applicable analytics models. Also, we want to help the users explore more information about the Singapore energy market easily through visualizations."),
                                                  
                                                  tabPanel("Boxplot",
                                                           fluidPage(
                                                             fluidRow(
                                                               column(3,wellPanel(
                                                                 pickerInput(
                                                                   inputId = "BoxYear",
                                                                   label = "Select Year",
                                                                   choices = c(
                                                                     "2022" = tickers[1], 
                                                                     "2021" = tickers[2],
                                                                     "2020" = tickers[3],
                                                                     "2019" = tickers[4],
                                                                     "2018" = tickers[5],
                                                                     "2017" = tickers[6]
                                                                   ),
                                                                   selected = "2022",   
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = F
                                                                 ),
                                                                 checkboxGroupInput("BoxRegion",
                                                                                    label = "Select Region",
                                                                                    choices = regions,
                                                                                    selected = regions
                                                                 )
                                                               )
                                                               ),
                                                               column(9)
                                                             )
                                                           )
                                                           
                                                  ),
                                                  tabPanel("Geofacet",
                                                           fluidPage(
                                                             fluidRow(
                                                               column(3,wellPanel(
                                                                 pickerInput(
                                                                   inputId = "GeofacetYear",
                                                                   label = "Select Year",
                                                                   choices = c(
                                                                     "2022" = tickers[1], 
                                                                     "2021" = tickers[2],
                                                                     "2020" = tickers[3],
                                                                     "2019" = tickers[4],
                                                                     "2018" = tickers[5],
                                                                     "2017" = tickers[6]
                                                                   ),
                                                                   selected = "2022",   
                                                                   options = list(`actions-box` = TRUE), 
                                                                   multiple = F
                                                                 ),
                                                                 checkboxGroupInput("GeofacetRegion",
                                                                                    label = "Select Region",
                                                                                    choices = regions,
                                                                                    selected = regions
                                                                 )
                                                               )
                                                               ),
                                                               column(9)
                                                             )
                                                           )
                                                  ),
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
                                                                                    selected = c("price", "consumption", "supply")
                                                                 )
                                                                 
                                                               )
                                                               )
                                                             )
                                                           )
                                                  ),
                                                  tabPanel("Barchart",
                                                           
                                                           fluidPage(
                                                             fluidRow(
                                                               column(3,wellPanel(
                                                                 radioButtons("BarData",
                                                                              label = "Market Share of",
                                                                              choices = c("Electricity", "Electricity Generation", "Natural Gas"),
                                                                              selected = "Electricity")
                                                                 
                                                               )
                                                               )
                                                             )
                                                           )
                                                           
                                                           
                                                           
                                                  )
                                       )
                              ),
                              tabPanel("CLUSTERING", tabName = "clustering", icon = icon("stream"),
                                       navbarPage("CLUSTERING", 
                                                  tabPanel("Hierachical Clustering"),
                                                  tabPanel("Choropleth map"),
                                                  tabPanel("Time Series Clustering")
                                       )
                              ),
                              tabPanel("INFERENTIAL STATISTICS", tabName = "inferential", icon = icon("info"),
                                       navbarPage("INFERENTIAL STATISTICS", 
                                                  tabPanel("ANOVA"),
                                                  tabPanel("Correlation Analysis")
                                       )
                              ),
                              tabPanel("TIME SERIES FORECASTING", tabName = "time_series", icon = icon("info"),
                                       fluidPage(titlePanel("TIME SERIES FORECASTING"),
                                                 sidebarPanel(
                                                   
                                                   textInput("txt", "Text input:", "text here"),
                                                   sliderInput("slider", "Slider input:", 1, 100, 30),
                                                   actionButton("action", "Button"),
                                                   actionButton("action2", "Button2", class = "btn-primary")
                                                 )
                                       )),
                              tabPanel("DATA", tabName = "data", icon = icon("info"),
                                       fluidPage(
                                         
                                         fluidRow(
                                           column(4,wellPanel(
                                             
                                             
                                             
                                             selectizeInput('SelectColumn', "Select Column", choices = c("A","B","C") ,multiple = TRUE),
                                             
                                             radioButtons("SelectTable",
                                                          label = "Select Data",
                                                          choices = c("Peak System Demand",
                                                                      "Market Share of Electricity Generation",
                                                                      "Total Household Electricity Consumption by Dwelling Type",
                                                                      "Average Monthly Household Electricity Consumption by Planning Area & Dwelling Type",
                                                                      "Market Share for Natural Gas Retail",
                                                                      "Natural Gas Consumption by Sub-Sector",
                                                                      "Total Household Town Gas Consumption by Dwelling Type",
                                                                      "Average Monthly Household Town Gas Consumption by Planning Area & Dwelling Type",
                                                                      "Electricity and Gas Tariffs",
                                                                      "Monthly Electricity Tariffs (Low Tension Tariffs)",
                                                                      "Annual Electricity Tariffs by Components (Low Tension Tariffs)",
                                                                      "Average Monthly Uniform Singapore Energy Prices (USEP)",
                                                                      "Monthly Town Gas Tariffs"),
                                                          selected = "Electricity")
                                             
                                           )
                                           )
                                         )
                                       )
                              ),
                 )
               )
               
)

server = function(input, output) {}


shinyApp(ui = ui, server = server)
