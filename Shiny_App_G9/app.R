library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)


ui = fluidPage(theme = shinytheme("slate"),
               headerPanel(title = "Singapore Energy Consumption"),
               dashboardSidebar(
                 navlistPanel(widths = c(3, 9),
                              
                              tabPanel("Overview",tabName = "overview",icon = icon("dashboard"),
                                       navbarPage("Overview", 
                                                  tabPanel("Introduction",
                                                           "An overview tab to include some of the points:
Title, description and objectives of this app Overall trends for basic understanding: Basic EDA Trend of household consumption + tariff (in the same line chart?)
A map showing the consumption with color gradient Something else… An overview tab to include some of the points:
Title, description and objectives of this app Overall trends for basic understanding: Basic EDA Trend of household consumption + tariff (in the same line chart?)
A map showing the consumption with color gradient Something else… An overview tab to include some of the points:
Title, description and objectives of this app Overall trends for basic understanding: Basic EDA Trend of household consumption + tariff (in the same line chart?)
A map showing the consumption with color gradient Something else… An overview tab to include some of the points:
Title, description and objectives of this app Overall trends for basic understanding: Basic EDA Trend of household consumption + tariff (in the same line chart?)
A map showing the consumption with color gradient Something else… "),

                                                  tabPanel("Boxplot"),
                                                  tabPanel("Geofacet"),
                                                  tabPanel("Lineplot"),
                                                  tabPanel("Barchart")
                                       )
                              ),
                              tabPanel("Clustering", tabName = "clustering", icon = icon("stream"),
                                       navbarPage("Overview", 
                                                  tabPanel("Hierachical Clustering"),
                                                  tabPanel("Choropleth map"),
                                                  tabPanel("Time Series Clustering")
                                       )
                                       ),
                              tabPanel("Inferential Statistics", tabName = "inferential", icon = icon("info"),
                                       fluidPage(titlePanel("Inferential Statistics"),
                                                 sidebarPanel(
                                                   
                                                   textInput("txt", "Text input:", "text here"),
                                                   sliderInput("slider", "Slider input:", 1, 100, 30),
                                                   actionButton("action", "Button"),
                                                   actionButton("action2", "Button2", class = "btn-primary")
                                                 )
                                       )),
                              tabPanel("Time Series Forecasting", tabName = "time_series", icon = icon("info"),
                                       fluidPage(titlePanel("Time Series Forecasting"),
                                                 sidebarPanel(
                                                   
                                                   textInput("txt", "Text input:", "text here"),
                                                   sliderInput("slider", "Slider input:", 1, 100, 30),
                                                   actionButton("action", "Button"),
                                                   actionButton("action2", "Button2", class = "btn-primary")
                                                 )
                                       )),
                              tabPanel("Data", tabName = "data", icon = icon("info"),
                                       fluidPage(titlePanel("Data"),
                                                 sidebarPanel(
                                                   
                                                   textInput("txt", "Text input:", "text here"),
                                                   sliderInput("slider", "Slider input:", 1, 100, 30),
                                                   actionButton("action", "Button"),
                                                   actionButton("action2", "Button2", class = "btn-primary")
                                                 )
                                       )),
                 )
                 # ,
                 # 
                 #   dashboardBody(
                 #     tabItems(
                 #       tabItem(tabName = "tab_dashboard",
                 #               fluidPage(titlePanel("tab_dashboard"),
                 #                         sidebarPanel(
                 #                           textInput("txt", "Text input:", "text here"),
                 #                           sliderInput("slider", "Slider input:", 1, 100, 30),
                 #                           actionButton("action", "Button"),
                 #                           actionButton("action2", "Button2", class = "btn-primary")
                 #                         ),)),
                 #       tabItem(tabName = "tab_tweet_wall"),
                 #       tabItem(tabName = "tab_about")
                 #     )
                 #   )
                 )
               
)
  
server = function(input, output) {}


shinyApp(ui = ui, server = server)
