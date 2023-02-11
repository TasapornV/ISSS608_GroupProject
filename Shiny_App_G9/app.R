
library(shiny)


## 1. Prepare library
packages = c('colourpicker','dplyr','ggplot2','igraph','network','networkD3','plotly',
             'quantmod','RColorBrewer','readr','readxl','reshape2','shiny',
             'shinycssloaders','shinydashboard','shinythemes','shinyWidgets',
             'sna','SnowballC','tidyverse','tidyquant','tm', 'bslib')

for(p in packages){
  library(p, character.only = T)}

## 2. Read data file

ui <- dashboardPage(theme = bs_theme(bootswatch = "minty"),
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
# library(shiny)
# library(shiny.semantic)
# library(semantic.dashboard)
# library(plotly)
# library(DT)
# 
# ui <- dashboardPage(theme = shinytheme("darkly"),
#   dashboardHeader(dropdownMenuOutput("dropdown"),
#                   dropdownMenu(type = "notifications",
#                                taskItem("Project progress...", 50.777, color = "red")),
#                   dropdownMenu(icon = icon("red warning sign"),
#                                notificationItem("This is an important notification!", color = "red"))),
#   dashboardSidebar(side = "left",
#                    sidebarMenu(
#                      menuItem(tabName = "plot_tab", text = "My plot", icon = icon("home")),
#                      menuItem(tabName = "table_tab", text = "My table", icon = icon("smile")))),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "plot_tab",
#               fluidRow(
#                 valueBox("Unread Mail", 44, icon("mail"), color = "blue", width = 5)),
#               fluidRow(
#                 box(title = "Sample box", color = "blue", width = 11,
#                     selectInput(inputId =  "variable1", choices = names(mtcars),
#                                 label = "Select first variable", selected = "mpg"),
#                     selectInput(inputId =  "variable2", choices = names(mtcars),
#                                 label = "Select second variable", selected = "cyl"),
#                     plotlyOutput("mtcars_plot")),
#                 tabBox(title = "Sample box", color = "blue", width = 5,
#                        collapsible = FALSE,
#                        tabs = list(
#                          list(menu = "First Tab", content = "Some text..."),
#                          list(menu = "Second Tab", content = plotlyOutput("mtcars_plot2"))
#                        )))),
#       tabItem(tabName = "table_tab",
#               fluidRow(
#                 valueBox("Unread Mail", 144, icon("mail"), color = "blue", width = 6, size = "small"),
#                 valueBox("Spam", 20, icon("mail"), color = "red", width = 5, size = "small"),
#                 valueBox("Readed Mail", 666, icon("mail"), color = "green", width = 5, size = "small")
#               ),
#               fluidRow(
#                 box(title = "Classic box", color = "blue", ribbon = FALSE,
#                     title_side = "top left", width = 14,
#                     tags$div(
#                       dataTableOutput("mtcars_table")
#                       , style = paste0("color:", semantic_palette[["blue"]], ";"))
#                 ))))
#   )
# )
# 
# server <- function(input, output) {
#   
#   output$mtcars_plot <- renderPlotly(plot_ly(mtcars, x = ~ mtcars[ , input$variable1],
#                                              y = ~ mtcars[ , input$variable2],
#                                              type = "scatter", mode = "markers")
#   )
#   output$mtcars_plot2 <- renderPlotly(plot_ly(mtcars, x = ~ mtcars[ , input$variable1],
#                                               y = ~ mtcars[ , input$variable2],
#                                               type = "scatter", mode = "markers"))
#   
#   output$mtcars_table <- renderDataTable(mtcars, options = list(dom = 't'))
#   
#   output$dropdown <- renderDropdownMenu({
#     dropdownMenu(messageItem("User", "Test message", color = "teal", style = "min-width: 200px"),
#                  messageItem("Users", "Test message", color = "teal", icon = "users"),
#                  messageItem("See this", "Another test", icon = "warning", color = "red"))
#   })
# }
# 
# shinyApp(ui, server)