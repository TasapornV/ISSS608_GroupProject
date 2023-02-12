library(shiny)
library(ggplot2)
library(shinythemes)
library(shinydashboard)


ui = fluidPage(theme = shinytheme("slate"),
               headerPanel(title = "Singapore Energy Consumption"),
               dashboardSidebar(
                 navlistPanel(id="tabset",
                              tabPanel("Dashboard",tabName = "tab_dashboard",icon = icon("dashboard"),
                                       fluidPage(titlePanel("tab_dashboard"),
                                                 sidebarPanel(
                                                   
                                                   textInput("txt", "Text input:", "text here"),
                                                   sliderInput("slider", "Slider input:", 1, 100, 30),
                                                   actionButton("action", "Button"),
                                                   actionButton("action2", "Button2", class = "btn-primary")
                                                 ),)),
                              tabPanel("Tweet wall", tabName = "tab_tweet_wall", icon = icon("stream")),
                              tabPanel("About", tabName = "tab_about", icon = icon("info"))
                   ),

                   dashboardBody(
                     tabItems(
                       tabItem(tabName = "tab_dashboard",
                               fluidPage(titlePanel("tab_dashboard"),
                                         sidebarPanel(
                                           textInput("txt", "Text input:", "text here"),
                                           sliderInput("slider", "Slider input:", 1, 100, 30),
                                           actionButton("action", "Button"),
                                           actionButton("action2", "Button2", class = "btn-primary")
                                         ),)),
                       tabItem(tabName = "tab_tweet_wall"),
                       tabItem(tabName = "tab_about")
                     )
                   )
                 )
               
)
  
server = function(input, output) {}


shinyApp(ui = ui, server = server)
