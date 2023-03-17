library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(geofacet)
library(treemap)
library(ggstatsplot)
library(ggridges)

realist <- read_csv("realist_v1.csv")
realist$`Sale Date` <- as.Date(realist$`Sale Date`, "%d/%m/%Y")
realist$MonthYear <- as.Date(realist$MonthYear, "%d/%m/%Y")
full_grid <- read_csv("areagrid.csv")

ui <- dashboardPage(skin = "red",
    dashboardHeader(title = "SPIVA"),
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
            menuItem("Overview", tabName = "geofacet", icon = icon("atlas")),
            menuItem("Transactional Prices", tabName = "ridge", icon = icon("globe-americas")),
            menuItem("Property Price Comparison", tabName = "tree", icon = icon("map")),
            menuItem("Analysis", tabName = "analysis", icon = icon("gears")),
            menuItem("Property Listing", tabName = "table", icon = icon("table")),
            
            conditionalPanel("input.sidebarmenu === 'geofacet'",
                             awesomeRadio(
                                 inputId = "geoyear",
                                 label = "Select Year", 
                                 choices = c("2018" = 2018, 
                                             "2019" = 2019)
                             ),
                             awesomeRadio(
                                 inputId = "geosale",
                                 label = "Type of Sale", 
                                 choices = c("New Sale",
                                             "Resale")
                             ),
                             selectInput(
                                 inputId = "geoprop",
                                 label = "Property Type", 
                                 choices =  unique(realist$`Property Type`),
                                 selected = "Condominium"
                             ),
                             awesomeRadio(
                                 inputId = "geovarb",
                                 label = "Measure", 
                                 choices = c("Avg Price" = "Avg Price",
                                             "Median Price" = "Median Price",
                                             "Volume" = "Volume")
                             ),
                             awesomeRadio(
                                 inputId = "geoaxis",
                                 label = "Axis", 
                                 choices = c("Common" = "fixed",
                                             "Independant" = "free")
                             ),
                             awesomeRadio(
                                 inputId = "geogrid",
                                 label = "Grid",
                                 choices = c(
                                     "Show all" = "full",
                                     "Hide empty" = "active")
                             )
                             ),
            conditionalPanel("input.sidebarmenu === 'ridge'",
                             awesomeRadio(
                               inputId = "ryear",
                               label = "Select Year", 
                               choices = c("2018" = 2018, 
                                           "2019" = 2019)
                             ),
                             awesomeRadio(
                               inputId = "rsale",
                               label = "Type of Sale", 
                               choices = c("New Sale",
                                           "Resale")
                             ),
                             selectInput(
                               inputId = "rprop",
                               label = "Property Type", 
                               choices =  unique(realist$`Property Type`),
                               selected = "Condominium"
                             ),
                             selectInput(
                               inputId = "rregion",
                               label = "Select Region",
                               choices = c(unique(realist$`Planning Region`),'All'),
                               selected = 'All'
                             ),
                             awesomeRadio(
                               inputId = 'postal',
                               label = 'View by Planning Area or Postal District',
                               choices = c("Planning Area", "Postal District"),
                               selected = 'Planning Area'
                             )
                             ),
            conditionalPanel("input.sidebarmenu === 'tree'",
                             awesomeRadio(
                                 inputId = "tyear",
                                 label = "Select Year", 
                                 choices = c("2018" = 2018, 
                                             "2019" = 2019)
                             ),
                             awesomeRadio(
                                 inputId = "tsale",
                                 label = "Type of Sale", 
                                 choices = c("New Sale",
                                             "Resale")
                             ),
                             selectInput(inputId = "tprop",
                                         label = "Property Type",
                                         choices = c("All", as.vector(unique(realist$`Property Type`))),
                             ),
                             selectInput(inputId = "tplanning_region",
                                         label = "Planning Region",
                                         choices = c("All", as.vector(unique(realist$`Planning Region`))),
                             ),
                             selectInput(inputId = "tplanning_area",
                                         label = "Planning Area",
                                         choices = NULL),
                             awesomeRadio(
                                 inputId = "tmeasure",
                                 label = "Choice of Price Comparison", 
                                 choices = c("Mean Price" = "tavg",
                                             "Median Price" = "tmed")
                             ),
                             awesomeRadio(
                                 inputId = "algo",
                                 label = "Algorithm",
                                 choices = c("PivotSize" = "pivotSize",
                                             "Squarified" = "squarified")
                             )
                             ),
            conditionalPanel("input.sidebarmenu === 'analysis'",
                             awesomeRadio(
                                 inputId = "ayear",
                                 label = "Select Year", 
                                 choices = c("2018" = 2018, 
                                             "2019" = 2019)
                             ),
                             awesomeRadio(
                                 inputId = "asale",
                                 label = "Type of Sale", 
                                 choices = c("New Sale",
                                             "Resale")
                             ),
                             selectInput(
                                 inputId = "aprop",
                                 label = "Property Type", 
                                 choices =  unique(realist$`Property Type`),
                                 selected = "Condominium"
                             ),
                             selectInput(
                                 inputId = "region1",
                                 label = "Select Region",
                                 choices = unique(realist$`Planning Region`),
                                 selected = "East Region"
                             ),
                             sliderInput(
                                 inputId = "mu",
                                 label = "Mean Value Test",
                                 min = 10000,
                                 max = 25000,
                                 step= 1000,
                                 value = 10000
                             ),
                             h4("Calibrate My Model"),
                             sliderInput(
                                 inputId = 'ci',
                                 label='Confidence Interval',
                                 min= 0.90,
                                 max=0.99,
                                 step = 0.01,
                                 value=0.90
                             ),
                             selectInput(
                                 inputId = 'test',
                                 label = 'Select Testing Procedure',
                                 choices = c('Parametric'='p','Non-Parametric'='np', 'Robust'='r', 'Bayes'='bf'),
                                 selected = 'p'
                             ),
                             selectInput(
                                 inputId = 'displaypair',
                                 label = 'Type of Pairwise to Display',
                                 choices = c('All'='all','Non-Significant'='ns', 'Significant'='s'),
                                 selected = 'all'
                             ),
                             checkboxInput(
                                 inputId = 'showpair',
                                 label = 'Show Pairwise Comparison',
                                 value = FALSE
                             ),
                             actionButton(
                                 'go', 'Update Now'
                             )
                             )
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "geofacet",
                    fluidRow(
                        titlePanel(
                            h2("Transactional Data Overview by Planning Area", align = "center")
                        )
                    ),
                    fluidRow(
                      valueBoxOutput("unitsold"),
                      valueBoxOutput("average"),
                      valueBoxOutput("median")
                    ),
                    fluidRow(
                    box(plotOutput("geo", height = 800), width = "100%")
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "ridge",
                    fluidRow(
                        titlePanel(
                            h2("Transacted Unit Price", align = "center")
                        )
                    ),
                    fluidRow(
                        box(plotOutput("ridgeplot"), width = "100%")
                    )
            ),
            
            # 3rd tab content
            tabItem(tabName = "tree",
                    fluidRow(
                        titlePanel(
                            h2("Price Comparison Treemap", align = "center")
                        )
                    ),
                    fluidRow(
                        box(plotOutput("treemap", height = 800), width = "100%")
                    )
                    
            ),
            
            # 4th tab content
            tabItem(tabName = "analysis",
                    fluidRow(
                        titlePanel(
                            h2("Price Distribution Analysis ", align = "center")
                        )
                    ),
                    fluidRow(
                        box(plotOutput("anova"), width = "100%")
                    ),
                    fluidRow(
                        box(plotOutput("histo"), width = "100%")
                    )
            ),
            
            # 5th tab content
            tabItem(tabName = "table",
                    fluidRow(
                        titlePanel(
                            h2("Property Listing", align = "center")
                        )
                    ),
                    fluidRow(
                        box(DT::dataTableOutput("table"), width = "100%")
                    )
            )
        )
    )
)

server <- function(input, output, session) {
    
  ## --------- GEO FACET ------------ ##
  
    observeEvent(input$tplanning_region,{
        updateSelectInput(session,'tplanning_area',
                          choices = c("All", unique(realist$`Planning Area`[realist$`Planning Region`==input$tplanning_region])))
    })
    output$geo <- renderPlot({
        pricelist <- realist %>%
            group_by(MonthYear, `Planning Area`, Year, `Property Type`, `Type of Sale`) %>%
            summarise(avgprice = mean(Price, na.rm = TRUE),
                      volume = sum(`No. of Units`, na.rm = TRUE),
                      medprice = median(Price, na.rm = TRUE))%>%
            ungroup()
        
        pz_price <- left_join(pricelist, full_grid,  
                              by = c("Planning Area" = "name"))
        
        finalprice <- pz_price %>%
            filter(
                Year == input$geoyear,
                `Property Type` == input$geoprop,
                `Type of Sale` == input$geosale
            )
        
        active_grid <- full_grid[full_grid$name %in% unique(finalprice$`Planning Area`),]
        
        switch(input$geovarb,
               "Avg Price" = ggplot(finalprice, aes(x = MonthYear, y = avgprice)),
               "Median Price" = ggplot(finalprice, aes(x = MonthYear, y = medprice)),
               "Volume" = ggplot(finalprice, aes(x = MonthYear, y = volume))) +
            geom_line() +
            labs(x = "Month",
                 y = input$geovarb) +
            scale_x_date(date_labels = "%b") +
            theme(axis.text.x = element_text(size = 10, angle = 45),
                  strip.text = element_text(size = 10),
                  panel.background = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA, size=1)) +
            facet_geo(~`Planning Area`, grid = 
                          switch(input$geogrid,
                                 "full" = full_grid,
                                 "active" = active_grid)
                      , label = "name", scale = input$geoaxis)
    })
    
    ## ---------RIDGE PLOT------------- ##
    
    output$ridgeplot <- renderPlot({
      x<- input$rregion
      
      if(x == 'All'){
        my_data <-realist %>%
          filter(Year== input$ryear,
                 `Property Type`== input$rprop,
                 `Type of Sale`==input$rsale)
      } else {
        
        my_data <- realist %>%
          filter(`Planning Region` == x,
                 Year== input$ryear,
                 `Property Type`== input$rprop,
                 `Type of Sale`==input$rsale)
      }
      
      switch(input$postal,
             'Postal District' = ggplot(data= my_data, aes(x = Price, 
                                                           y=reorder(`Postal District`,-Price), na.rm=TRUE, 
                                                           fill = stat(x), alpha=0.8)),
             'Planning Area' = ggplot(data= my_data, aes(x = Price, 
                                                         y=reorder(`Planning Area`,-Price), na.rm=TRUE, 
                                                         fill = stat(x), alpha=0.8))) +
        stat_density_ridges(
          geom = 'density_ridges_gradient',
          calc_ecdf = TRUE,
          quantiles = c(0.025, 0.0975), alpha=0.2)+
        labs(title = "Variation within Planning Area",
             y= switch(input$postal,
                       'Planning Area'='Planning Area',
                       'Postal District' = 'Postal District'),
             x='Unit Price (SGD per psm)')

    })
    
    
    ## --------- TREE MAP ------------- ##
    
    output$treemap <- renderPlot({
        realist_summarised <- realist %>% 
            group_by(`Project`,`Planning Region`, `Planning Area`, `Property Type`, `Type of Sale`, Year) %>%
            filter(Year == input$tyear)%>%
            summarise(`Total Unit Sold` = sum(`No. of Units`, na.rm = TRUE), 
                      `Median Unit Price ($ psm)` = median(Price, na.rm = TRUE),
                      `Average Unit Price ($ psm)` = mean(Price, na.rm = TRUE))
        
        realist_PAsummarised <- realist_summarised %>%
            group_by(`Planning Area`) %>%
            summarise(`Median Unit Price of Planning Area ($ psm)` = median(`Median Unit Price ($ psm)`, na.rm = TRUE),
                      `Average Unit Price of Planning Area ($ psm)` = mean(`Average Unit Price ($ psm)`, na.rm = TRUE))
        
        realist_final <- left_join(realist_summarised, realist_PAsummarised, by = NULL, copy=FALSE) %>%
            mutate(`Difference (Median)` = `Median Unit Price ($ psm)` - `Median Unit Price of Planning Area ($ psm)`,
                   `Difference (Average)` = `Average Unit Price ($ psm)` - `Average Unit Price of Planning Area ($ psm)`)
        
        tproperty_sel <- if (input$tprop == "All" ) unique(as.vector(realist$`Property Type`)) else input$tprop
        tplanning_region_sel <- if (input$tplanning_region == "All") unique(as.vector(realist$`Planning Region`)) else input$tplanning_region
        tplanning_area_sel <- if (input$tplanning_area == "All") unique(as.vector(realist$`Planning Area`)) else input$tplanning_area
        
        realist_selected <- realist_final %>%
            filter(`Property Type` %in% tproperty_sel, `Type of Sale` == input$tsale, `Planning Region` %in% tplanning_region_sel, `Planning Area` %in% tplanning_area_sel, Project != "N.A.")
        
        if (input$tmeasure == "tmed") 
            treemap(realist_selected,
                    index = c("Planning Region", "Planning Area", "Project"),
                    vSize = "Total Unit Sold",
                    vColor = "Difference (Median)",
                    type = "value",
                    palette = "PuOr", 
                    algorithm = input$algo,
                    sortID = "Total Unit Sold",
                    title = "Property Price Comparison by Planning Region and Area",
                    title.legend = "Difference between Median Unit Price of Project and Planning Area (S$ per sq. m)",
        )
        else
            treemap(realist_selected,
                    index = c("Planning Region", "Planning Area", "Project"),
                    vSize = "Total Unit Sold",
                    vColor = "Difference (Average)",
                    type = "value",
                    palette = "PuOr", 
                    algorithm = input$algo,
                    sortID = "Total Unit Sold",
                    title = "Property Price Comparison by Planning Region and Area",
                    title.legend = "Difference between Average Unit Price of Project and Planning Area (S$ per sq. m)",
            )
    })
    
    ## --------- ANOVA GGBETWEENSTATS & HISTOPLOT ------------ ##
    
    output$anova <- renderPlot({
        input$go

        isolate({
            my_data2 <- realist %>%
                filter(Year==input$ayear,
                       `Planning Region` == input$region1,
                       `Type of Sale`==input$asale,
                       `Property Type`==input$aprop)

            ggbetweenstats(data=my_data2,
                           x= SaleQuarter,
                           y= Price,
                           mean.plotting = TRUE,
                           mean.ci = FALSE,
                           pairwise.comparisons = input$showpair,
                           pairwise.display = input$displaypair,
                           notch = TRUE,
                           type = input$test,
                           k=2,
                           messages = FALSE
            )+
              labs(x = "Quarter",
                   y = "Price (psm)") +
                coord_cartesian(ylim = c(min(my_data2$Price),(max(my_data2$Price) + 3500)))
        })
    })

    output$choice<- renderText({
        input$go

        isolate({
            input$TOP
        })
    })

    output$histo <- renderPlot({
        input$go


        isolate ({
            my_data_id <- tibble::rowid_to_column(realist, var = "rowid")

            my_data3<- my_data_id %>%
                filter(Year== input$ayear,
                       `Planning Region` == input$region1,
                       `Type of Sale`== input$asale,
                       `Property Type`== input$aprop) %>%
                spread(`Property Type`, Price) %>%
                select(rowid, input$aprop)

            gghistostats(data = my_data3,
                         x = !!(input$aprop), # !! TO UNQUOTE
                         results.subtitle = TRUE,
                         messages = TRUE,
                         bar.measure='proportion',
                         xlab = 'Unit Sale Price per PSM (SGD)',
                         title ='Check for Normality' ,
                         type = input$test,
                         bf.message = TRUE,
                         ggtheme = ggthemes::theme_tufte(),
                         bar.fill = '#baddf9',
                         normal.curve = TRUE,
                         normal.curve.color = 'black',
                         test.value = input$mu,
                         test.value.line = TRUE,
                         centrality.k = 1)
        })
    })
    
    ## --------- DATA TABLE ------------ ##
    
    output$table <- DT::renderDataTable({
        DT::datatable(data=realist %>% select(1,3,7,8,11,16,17),
                      options=list(pagelength=10),
                      rownames = FALSE)
    })
    
    ## --------- INFORMATION ------------ ##
    
    output$unitsold <- renderInfoBox({
      pricelist <- realist %>%
        group_by(Year, `Property Type`, `Type of Sale`) %>%
        summarise(avgprice = mean(Price, na.rm = TRUE),
                  volume = sum(`No. of Units`, na.rm = TRUE),
                  medprice = median(Price, na.rm = TRUE))%>%
        ungroup()
      
      finalprice <- pricelist %>%
        filter(
          Year == input$geoyear,
          `Property Type` == input$geoprop,
          `Type of Sale` == input$geosale
        )
      
      valueBox(paste(finalprice$volume), "Number of Units Sold", icon = icon("handshake"), color = "aqua")
    })
    
    output$average <- renderInfoBox({
      pricelist <- realist %>%
        group_by(Year, `Property Type`, `Type of Sale`) %>%
        summarise(avgprice = mean(Price, na.rm = TRUE),
                  volume = sum(`No. of Units`, na.rm = TRUE),
                  medprice = median(Price, na.rm = TRUE))%>%
        ungroup()
      
      finalprice <- pricelist %>%
        filter(
          Year == input$geoyear,
          `Property Type` == input$geoprop,
          `Type of Sale` == input$geosale
        )
      
      valueBox(paste("$",round(finalprice$avgprice, 0)), "Average Price", icon = icon("hand-holding-usd"), color = "teal")
    })
    
    output$median <- renderValueBox({
      pricelist <- realist %>%
        group_by(Year, `Property Type`, `Type of Sale`) %>%
        summarise(avgprice = mean(Price, na.rm = TRUE),
                  volume = sum(`No. of Units`, na.rm = TRUE),
                  medprice = median(Price, na.rm = TRUE))%>%
        ungroup()
      
      finalprice <- pricelist %>%
        filter(
          Year == input$geoyear,
          `Property Type` == input$geoprop,
          `Type of Sale` == input$geosale
        )
      
      valueBox(paste("$",finalprice$medprice), "Median Price", icon = icon("hand-holding-usd"), color = "yellow")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
