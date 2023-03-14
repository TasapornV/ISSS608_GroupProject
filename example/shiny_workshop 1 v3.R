library(shiny)
library(shinydashboard)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)

#setwd("C:/Users/winst/Downloads/Shiny_ASAR2020/For Distribution/Shiny")
ec_geo2 <- read_excel("ec_geo2.xlsx")
ec_data <- read_csv('ec_data_workshop.csv')
ec_data_mlr <- read_csv('ec_data_v6_mlr.csv')


ec_data <- as.data.frame(ec_data)
ec_data$Floor_Level <- factor(ec_data$Floor_Level, labels = c("Ground Floor", "Bottom Half", "Top Half", "Penthouse"))
ec_data$mrt_distance_grp <- factor(ec_data$mrt_distance_grp, labels = c("< 1km", "1-2 km", "> 2km"))
#############################mlr starts from here
names(ec_data_mlr)[1] <- "Price"
names(ec_data_mlr)[2] <- "Area_sqm"
names(ec_data_mlr)[5] <- "No_of_years_owned"
names(ec_data_mlr)[4] <- "Prior_Transacted_Price"
names(ec_data_mlr)[16] <- "EastRegion"

ecregress1 = lm(Price ~ Area_sqm + PropertyAge + Prior_Transacted_Price + No_of_years_owned + GroundFloor + LowFloor + HighFloor + Penthouse + NewPurchaser + HDBPurchaser + PrivatePurchaser + WestRegion + NorthRegion + NorthEastRegion +EastRegion + CentralRegion + mrt_dist_meter + sch_dist_meter, data = ec_data_mlr)
summary(ecregress1)


#########################################################Shiny STARTS FROM HERE
ui <- dashboardPage(skin = 'yellow',
                    dashboardHeader(title = 'Executive Condominium Sales Analysis', titleWidth = 400),
                    dashboardSidebar(width = 400,
                                     sidebarMenu(id = 'sbm',
                                                 menuItem('Introduction', tabName = 'Introduction', icon = NULL),
                                                 menuItem('Descriptive', tabName = 'Descriptive', icon = NULL),
                                                 menuItem('Inferential', tabName = 'Inferential', icon = NULL)
                                                 # menuItem('CI', tabName = 'CI', icon = NULL)
                                     )
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = 'Introduction',
                                    fluidPage(
                                        titlePanel("All Executive Condominiums in Singapore"),
                                        fluidRow(
                                            column(width = 12,
                                                   box(
                                                       width = 12,
                                                       height = 500,
                                                       solidHeader = TRUE,
                                                       collapsible = FALSE,
                                                       collapsed = FALSE,
                                                       leafletOutput('map2', height = 500)
                                                   )
                                            )
                                        ),
                                        sidebarLayout(
                                            sidebarPanel( 
                                                selectInput("region", "Region:",
                                                            choices = 
                                                                list('All' = list("North East Region", "North Region", "East Region", "West Region", "Central Region"
                                                                                  ))),
                                                            
                                                                            
                                                hr(),
                                                helpText("Executive condominiums by Region."),
                                                Position = "top"
                                        ),
                                            mainPanel(
                                                leafletOutput("map", height = 500)
                                           )
                                        ),
                                                   
                                        fluidRow(
                                            column(width = 12,
                                                   box(
                                                       width = 12,
                                                       height = 800,
                                                       solidHeader = TRUE,
                                                       collapsible = FALSE,
                                                       collapsed = FALSE,
                                                       plotOutput('regionanalysis', height = 750)
                                                   )
                                            )
                                        )
                                        )
                                    ),
                                    ###end of introduction
                            tabItem(tabName = 'Descriptive',
                                    fluidPage(
                                        titlePanel("Descriptive Statistics"),
                                        fluidRow(
                                            column(width = 12,tabsetPanel(
                                                tabPanel("Price by Factors",
                                                         box(
                                                             radioButtons('xcol',
                                                                          label = tags$strong('Analyse Sales By:'),
                                                                          choices = c('Floor level' = 'Floor_Level',
                                                                                      'Disctance from MRT' = 'mrt_distance_grp',
                                                                                      'Completion date' = '`Completion Date`'),
                                                                          inline = TRUE)
                                                         ),
                                                         box(
                                                             width = 12,
                                                             height = 800,
                                                             solidHeader = TRUE,
                                                             collapsible = FALSE,
                                                             collapsed = FALSE,
                                                             plotOutput('descriptiveAnalysis', height = 750)
                                                         )
                                                ),
                                                tabPanel("Sales Count by Region",
                                                         box(
                                                             width = 12,
                                                             height = 800,
                                                             solidHeader = TRUE,
                                                             collapsible = FALSE,
                                                             collapsed = FALSE,
                                                             selectInput(
                                                                 inputId = "Year", 
                                                                 label = "Select years:", 
                                                                 choices = 2000:2019),
                                                             plotOutput(outputId  = "piechart", 
                                                                        height = "600")
                                                             )
                                                         ),
                                                tabPanel("Price By Region",
                                                         box(
                                                    width = 12,
                                                    height = 800,
                                                    solidHeader = TRUE,
                                                    collapsible = FALSE,
                                                    collapsed = FALSE,
                                                    selectInput(
                                                        inputId = "year_boxplot", 
                                                        label = "Select years:", 
                                                        choices = 2000:2019),
                                                    plotOutput(outputId  = "boxplot",
                                                               height = "600")
                                                    )
                                                    )
                                            )
                                        )
                                        #end of tabname "Descriptive" 
                                    ))),
                            
                                tabItem(tabName = 'Inferential',
                                        fluidPage(
                                            titlePanel('Multi Linear Regression Model'),
                                            sidebarLayout(
                                                sidebarPanel(
                                                    h4("Choose your preferences here:"),
                    
                                                    sliderInput("DMRT", "Distance from MRT(M)",
                                                                min = 241, max = 2466, value = 241
                                                    ),
                                                    sliderInput("DS", "Distance from school(M)",
                                                                min = 60, max = 940, value = 60
                                                    ),
                                                    sliderInput("Area", "Area(sqm):",
                                                                 min = 64, max = 324, value = 64
                                                                ),
                                                    sliderInput("PA", "Property Age:",
                                                                min = 0, max = 25, value = 1),
                                                    sliderInput("PTP", "Prior Transacted Price:",
                                                                min = 318000, max = 1880000, value = 318000),
                                                    sliderInput("NOYO", "No of Years Owned:",
                                                                min = 0, max = 23, value = 1),
                                                    
                                                    numericInput("GF", "Ground Floor(yes = 1, no = 0):",
                                                                value = 0, min = 0, max = 1),
                                                    numericInput("BH", "Bottom Half(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1),
                                                    numericInput("TH", "Top Half(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1),
                                                    numericInput("NP", "New Purchaser(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1),
                                                    numericInput("HDBP", "HDB Purchaser(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1),
                                                    numericInput("WR", "West Region(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1),
                                                    numericInput("NR", "North Region(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1),
                                                    numericInput("NER", "North East Region(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1),
                                                    numericInput("ER", "East Region(yes = 1, no = 0):",
                                                                 value = 0, min = 0, max = 1)

                                                ),
                                                mainPanel(
                                                    h4("The predicted property prices for 2020:"),
                                                    textOutput("formula"),
                                                    h4("The predicted prices from 2025 to 2040:"),
                                                    textOutput("price2025"),
                                                    textOutput("price2030"),
                                                    textOutput("price2035"),
                                                    textOutput("price2040"),
                                                    plotOutput("plot1")
                                                )
                                            )
                                        ))
                                )))#end of tabname "Inferential"
##########################################################################################Server code starts here
                                
server <- function(input, output) {
    
    pal <- colorFactor(palette = c("red", "green", "blue", "orange", "purple"),
                                           levels = c("East Region", "North East Region", 
                                                      "West Region", "North Region", "Central Region"))

    output$map <- renderLeaflet({
            ec_geo2 %>%
            filter(`Planning Region` == input$region) %>%
            leaflet() %>%
            setView(lng = 103.8522, lat = 1.347510, zoom = 11) %>%
            addTiles() %>%
            addCircleMarkers(label = ~ pjname, color = ~ pal(input$region), radius = 3, fillOpacity = 0.5) %>%
            #addSearchGoogle() %>%
            addLegend(
                "bottomleft",
                pal = pal,
                values = ~`Planning Region`,
                opacity = 1, #color transparency
                title = "Regions")
    })
    
    output$map2 <- renderLeaflet({
        ec_geo2 %>%
            leaflet() %>%
            setView(lng = 103.8522, lat = 1.347510, zoom = 11) %>%
            addTiles() %>%
            addCircleMarkers(label = ~ pjname, color = ~ pal(`Planning Region`), radius = 3, fillOpacity = 0.5) %>%
            addSearchOSM() %>%
            addLegend(
                "bottomleft",
                pal = pal,
                values = ~`Planning Region`,
                opacity = 1, #color transparency
                title = "Regions")
        
    })
    
    output$regionanalysis <- renderPlot({
        data_rv <- ec_data %>%
            group_by(`Planning Region`, Sale_Year) %>%
            summarise(region_value = mean(`Unit Price ($ psm)`, na.rm = T))
        data_rv$Sale_Year <- as.numeric(data_rv$Sale_Year)

        p <- ggplot(data_rv, aes(y = region_value, x = Sale_Year)) +
            geom_smooth(aes(col = data_rv$`Planning Region`), method = 'lm', se = FALSE) + 
            geom_point(aes(col = data_rv$`Planning Region`), size = 2.5) + 
            labs(title = 'Sales Price by Year', subtitle = paste('by', 'Region'),
                 col = 'Planning Region',x = 'Sales Year', y = 'Sales Price ($)',
                 fill = data_rv$`Planning Region`)
        return(p)

    })
    
####################################################end of introduction
    
    output$descriptiveAnalysis <- renderPlot({
        analysis <- ec_data %>%
            group_by_(.dots = input$xcol) %>%
            filter(`Completion Date` != 'Uncompleted', `Completion Date` != 'Unknown') %>%
            summarise(basket_value = mean(`Unit Price ($ psm)`, na.rm = T))
        
        p <- ggplot(analysis, aes_string(y = 'basket_value', x = input$xcol)) +
            geom_bar(aes_string(fill = input$xcol), stat = 'identity') +
            labs(title = 'Average Sales Price', subtitle = paste('by', input$xcol), 
                 x = input$xcol, y = 'Sales Price ($)',
                 fill = input$xcol)
        return(p)
            
        })
    
    output$boxplot <- renderPlot({
        ec_data %>%
            filter(Sale_Year == input$year_boxplot) %>%
            ggplot(aes(y = `Unit Price ($ psm)`, x = `Planning Region`))+
                geom_boxplot()+
                stat_summary(geom = 'point',
                             fun = 'mean',
                             colour = 'red',
                             size = 2) +
                labs(title = 'The relationship between Planning Region and Area(sqm)',
                     x = 'Planning Region', y = 'Unit Price ($ psf)')
        })
    
    output$piechart <- renderPlot({
        ec_data %>%
            filter(Sale_Year == input$Year) %>%
            group_by(`Planning Region`) %>%
            summarise(#region_value = mean(`Unit Price ($ psm)`, na.rm = T),
                count = n()) %>%
            ggplot(aes(x="", y=count, fill=`Planning Region`)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0)+
                theme_void() +
                labs(title = 'The proportion of sale counts for differnet Planning Regions',
                     x = 'Planning Region') +
                theme(plot.title = element_text(color = "Black", size = 14))
    })
#################end of descriptive analysis
#################printing the price for 2020
    output$formula <- renderText({
        mlrformula <- 5.813*10^5 + 1.268*10^3*input$Area + 1.805*10^4*(1
        +input$PA) +
        5.351*10^(-1)*input$PTP + 4.487*10^3*input$NOYO + (-4.795)*10^4*input$GF  + (-6.797)*10^4*input$BH + (-5.423)*10^4*input$TH +
        8.72*10^4*input$NP + (-5.689)*10^3*input$HDBP + (-4.223)*10^5*input$WR + (-3.559)*10^5*input$NR +
        (-2.543)*10^5*input$NER + (-2.877)*10^5*input$ER + (-1.469)*10*input$DMRT + (-2.704)*10^2*input$DS
        
        paste('Predicted Price for 2020 is:', round(mlrformula), "SGD")
        
        
    })
    

###################printing the price for 2025
    output$price2025 <- renderText({
        mlrformula <- 5.813*10^5 + 1.268*10^3*input$Area + 1.805*10^4*(1
                                                                       +input$PA) +
            5.351*10^(-1)*input$PTP + 4.487*10^3*input$NOYO + (-4.795)*10^4*input$GF  + (-6.797)*10^4*input$BH + (-5.423)*10^4*input$TH +
            8.72*10^4*input$NP + (-5.689)*10^3*input$HDBP + (-4.223)*10^5*input$WR + (-3.559)*10^5*input$NR +
            (-2.543)*10^5*input$NER + (-2.877)*10^5*input$ER + (-1.469)*10*input$DMRT + (-2.704)*10^2*input$DS
        price_2025 <- round(mlrformula + 1.805*10^4*5  + 4.487*10^3*5)
        paste('The predicted price for 2025 is:', price_2025, "SGD", ", and the growth is:", round((price_2025 - mlrformula)/mlrformula*100), "%")
    })
###################printing the price for 2030
    output$price2030 <- renderText({
        mlrformula <- 5.813*10^5 + 1.268*10^3*input$Area + 1.805*10^4*(1
                                                                       +input$PA) +
            5.351*10^(-1)*input$PTP + 4.487*10^3*input$NOYO + (-4.795)*10^4*input$GF  + (-6.797)*10^4*input$BH + (-5.423)*10^4*input$TH +
            8.72*10^4*input$NP + (-5.689)*10^3*input$HDBP + (-4.223)*10^5*input$WR + (-3.559)*10^5*input$NR +
            (-2.543)*10^5*input$NER + (-2.877)*10^5*input$ER + (-1.469)*10*input$DMRT + (-2.704)*10^2*input$DS
        price_2030 <- round(mlrformula + 1.805*10^4*10  + 4.487*10^3*10)
        paste('The predicted price for 2030 is:', price_2030, "SGD", ", and the growth is:", round((price_2030 - mlrformula)/mlrformula*100), "%")
    })
###################printing the price for 2035
    output$price2035 <- renderText({
        mlrformula <- 5.813*10^5 + 1.268*10^3*input$Area + 1.805*10^4*(1
                                                                       +input$PA) +
            5.351*10^(-1)*input$PTP + 4.487*10^3*input$NOYO + (-4.795)*10^4*input$GF  + (-6.797)*10^4*input$BH + (-5.423)*10^4*input$TH +
            8.72*10^4*input$NP + (-5.689)*10^3*input$HDBP + (-4.223)*10^5*input$WR + (-3.559)*10^5*input$NR +
            (-2.543)*10^5*input$NER + (-2.877)*10^5*input$ER + (-1.469)*10*input$DMRT + (-2.704)*10^2*input$DS
        price_2035 <- round(mlrformula + 1.805*10^4*15  + 4.487*10^3*15)
        paste('The predicted price for 2035 is:', price_2035, "SGD", ", and the growth is:", round((price_2035 - mlrformula)/mlrformula*100), "%")
    })
###################printing the price for 2040
    output$price2040 <- renderText({
        mlrformula <- 5.813*10^5 + 1.268*10^3*input$Area + 1.805*10^4*(1
                                                                       +input$PA) +
            5.351*10^(-1)*input$PTP + 4.487*10^3*input$NOYO + (-4.795)*10^4*input$GF  + (-6.797)*10^4*input$BH + (-5.423)*10^4*input$TH +
            8.72*10^4*input$NP + (-5.689)*10^3*input$HDBP + (-4.223)*10^5*input$WR + (-3.559)*10^5*input$NR +
            (-2.543)*10^5*input$NER + (-2.877)*10^5*input$ER + (-1.469)*10*input$DMRT + (-2.704)*10^2*input$DS
        price_2040 <- round(mlrformula + 1.805*10^4*20 + 4.487*10^3*20)
        paste('The predicted price for 2040 is:', price_2040, "SGD", ", and the growth is:", round((price_2040 - mlrformula)/mlrformula*100), "%")
    })
    
###################multilinear plot
    output$plot1 <- renderPlot({
        mlrformula <- 5.813*10^5 + 1.268*10^3*input$Area + 1.805*10^4*(1
                                                                       +input$PA) +
            5.351*10^(-1)*input$PTP + 4.487*10^3*input$NOYO + (-4.795)*10^4*input$GF  + (-6.797)*10^4*input$BH + (-5.423)*10^4*input$TH +
            8.72*10^4*input$NP + (-5.689)*10^3*input$HDBP + (-4.223)*10^5*input$WR + (-3.559)*10^5*input$NR +
            (-2.543)*10^5*input$NER + (-2.877)*10^5*input$ER + (-1.469)*10*input$DMRT + (-2.704)*10^2*input$DS
        
        price_2025 <- mlrformula + 1.805*10^4*5 + 4.487*10^3*5
        price_2030 <- mlrformula + 1.805*10^4*10 + 4.487*10^3*10
        price_2035 <- mlrformula + 1.805*10^4*15 + 4.487*10^3*15
        price_2040 <- mlrformula + 1.805*10^4*20 + 4.487*10^3*20
        
        
        growth1 <- (price_2025 - mlrformula)/mlrformula * 100
        growth2 <- (price_2030 - mlrformula)/mlrformula * 100
        growth3 <- (price_2035 - mlrformula)/mlrformula * 100
        growth4 <- (price_2040 - mlrformula)/mlrformula * 100
       
        
        future_price <- data.frame(growth = c(0, growth1, growth2, growth3, growth4),
                                   year = c(2020, 2025, 2030, 2035, 2040),
                                   year1 = c("2020", "2025", "2030", "2035", "2040"),
                                   price = c(mlrformula, price_2025, price_2030, price_2035, price_2040))
        
        future_price %>% 
            ggplot(aes(y = growth, x = year)) +
            geom_point(aes(col = year1, size = 5)) +
            geom_smooth(method = 'lm', se = F, col = "red", alpha = 0.15) + 
            geom_text(aes(label=round(price)),hjust=1, vjust=(-2), size = 3) + 
            ylim(0, 300) +
            theme(legend.position="top")
            
    })

        
    
}


shinyApp(ui, server)
                                
                                