library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(leaflet)


load(file = 'data/story.RData')

story_data %>% mutate(Date = mdy(Date),
                      City = tolower(City)) %>% 
  filter(Lon !='NaN')-> story_data

# adding date variables
story_clean <- story_data %>% mutate(Month = lubridate::month(Date, label = TRUE),
                                     Year = lubridate::year(Date),
                                     Mon_Yr = lubridate::floor_date(Date, unit = "month"))

vol2 <- story_clean %>% group_by(Mon_Yr, City) %>% 
  summarize_at(vars('Volume.Sold..Liters.', 'Sale..Dollars.'),
               .funs = sum, na.rm=TRUE)
names(vol2) <- c("Mon_Yr", "City", "Litter", "Dollar")

ui <- navbarPage("Lab4, Team1",
                 tabPanel("Temporal",
                          sidebarPanel(
                            radioButtons("int1", "Select your interest:",
                                                names(vol2)[c(3,4)]),
                            selectInput("city1",
                                        "City:",
                                               c(unique(as.character(vol2$City))),
                                        multiple = TRUE, selected = "ames"),
                            
                            dateRangeInput("date1",
                                           label = 'Date range input: ',
                                           start = min(vol2$Mon_Yr),
                                           end = max(vol2$Mon_Yr),
                                           min    = min(vol2$Mon_Yr),
                                           max    = max(vol2$Mon_Yr))
                                   ),
                            mainPanel("Temporal plot", plotOutput("plot1"))
                            ),
                 
                 tabPanel("Spatial",
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              
                              
                              
                              # br() element to introduce extra vertical spacing ----
                              
                              
                              # Input: Select the variable ----
                              radioButtons("covariate", "Studied Variables:",
                                           c("Bottles.Sold","Sale..Dollars.")),
                              
                              br(),
                              # Input: Select a City ----
                              selectInput("City", "City:",
                                          choices = unique(story_data$City)),
                              # br(),
                              # # Input: Select a Store ----
                              # selectInput("Store", "Store.Name:",
                              #             choices = unique(story_data$Store.Name),
                              #             multiple=T),
                              # 
                              # br(),
                              # # Input: Select a Vendor ----
                              # selectInput("Vendor", "Vendor:",
                              #             choices = unique(story_data$Vendor.Name)),
                              # 
                              # br(),
                              # # Input: Select a Liquor Category ----
                              # selectInput("Category", "Liquor Category:",
                              #             choices = unique(story_data$Category.Name)),
                              
                              # Input: Select a time range ----
                              br(),
                              dateRangeInput('date',
                                             label = 'Date range input: yyyy-mm-dd',
                                             start = min(story_data$Date),
                                             end = max(story_data$Date),
                                             min    = min(story_data$Date),
                                             max    = max(story_data$Date)
                              ),
                              
                              # Input: Select a location range ----
                              br(),
                              sliderInput("Lat",
                                          "Latitude Range:",
                                          value =c(min(story_data$Lat), max(story_data$Lat)),
                                          min = min(story_data$Lat),
                                          max = max(story_data$Lat)),
                              
                              sliderInput("Lon",
                                          "Longitude Range:",
                                          value =c(min(story_data$Lon), max(story_data$Lon)),
                                          min = min(story_data$Lon),
                                          max = max(story_data$Lon))
                              
                              
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: Tabset w/ plot, summary, and table ----
                              tabsetPanel(type = "tabs",
                                          tabPanel("Store Map", leafletOutput("map")),
                                          tabPanel("Category Pie Chart", plotOutput("plot")),
                                          tabPanel("Table of Item", tableOutput("table")),
                                          tabPanel("Summary on Vendor", verbatimTextOutput("summary"))
                              )
                              
                            )
                          ))
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    
    
    vol2_sub <- vol2 %>%
      filter(City %in% input$city1,
             Mon_Yr >= input$date1[1] & Mon_Yr <= input$date1[2])
    
  p <- ggplot(vol2_sub, aes_string(x = "Mon_Yr", y = input$int1, group = "City", color = "City")) +
    geom_line() + ylab("Volume Sold (Liters)") + theme_bw()
  p
  })
  
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  d1 <- reactive({
    my_df <- story_data %>%
      filter(City %in% input$City,
             between(Lon, min(input$Lon),max(input$Lon)),
             between(Lat, min(input$Lat),max(input$Lat)),
             Date >= input$date[1]& Date <= input$date[2])
  })
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(data = d1() %>% select(Store.Name, Lon, Lat) %>% distinct()) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(~ Lon,
                                ~ Lat,
                                popup = ~ Store.Name,
                                clusterOptions = leaflet::markerClusterOptions())
  })
  
  
  output$plot<-renderPlot({
    ggplot(data = d1() %>% group_by(Category.Name) %>% tally(
      switch(input$covariate,
             Bottles.Sold = Bottles.Sold,
             Sale..Dollars. = Sale..Dollars.,
             Pack)
    ) %>% top_n(10,n) %>% arrange(by=n),aes(x="", y=n, fill=Category.Name)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)+theme_bw()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      ) +  ggtitle(paste("Top 10 Category Liquor based on ", input$covariate))
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d1() %>% select(Vendor.Name,input$covariate))
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d1() %>% group_by(Item.Description) %>% tally(
      switch(input$covariate,
             Bottles.Sold = Bottles.Sold,
             Sale..Dollars. = Sale..Dollars.,
             Pack)
    ) %>% arrange(desc(n)) %>% setNames(c('Item',input$covariate))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

