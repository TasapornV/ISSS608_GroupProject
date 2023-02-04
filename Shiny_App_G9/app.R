
library(shiny)


## 1. Prepare library
packages = c('colourpicker','dplyr','ggplot2','igraph','network','networkD3','plotly',
             'quantmod','RColorBrewer','readr','readxl','reshape2','shiny',
             'shinycssloaders','shinydashboard','shinythemes','shinyWidgets',
             'sna','SnowballC','tidyverse','tidyquant','tm')

for(p in packages){
  library(p, character.only = T)}

## 2. Read data file

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Singapore Energy Analysis"),
                    dashboardSidebar(sidebarMenu(
                      id = "sbm", collapsible = T,
                      HTML(paste0(
                        "<br>",
                        "<a><img style = 'display: block; margin-left: auto; margin-right: auto;' src='twitter-default-profile.jpg' width = '186'></a>",
                        "<br>",
                        "<p style = 'text-align: center;'><small><a>Cryptoverse logo disclaimer</a></small></p>",
                        "<br>"
                      )
                      ),
                      menuItem("Dashboard",tabName = "tab_dashboard",icon = icon("dashboard")),
                      menuItem("Cluster", tabName = "tab_clustering", icon = icon("stream")),
                      menuItem("Inferential", tabName = "tab_inferential", icon = icon("info")),
                      menuItem("Forecasting", tabName = "tab_forecasting", icon = icon("info"))         
                    )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        
                        tabItem(tabName = "tab_dashboard"),
                        tabItem(tabName = "tab_clustering"),
                        tabItem(tabName = "tab_inferential",
                                titlePanel("Inferential Analysis - Household consumption"),
                                
                                ),
                        tabItem(tabName = "tab_forecasting")
                      )
                    )
                    
                    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
