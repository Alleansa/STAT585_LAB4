library(qdapRegex)
library(dplyr)
library(leaflet)
library(lubridate)
library(ggplot2)

# ## DATA PREPROCESSING
# story_data <-read.csv(file ='Iowa_Liquor_Sales-Story.csv')
# 
# story_data %>% 
#   mutate(Lat = as.numeric(rm_between(Store.Location, '(', ',', extract=TRUE)),
#          Lon = as.numeric(rm_between(Store.Location, ',', ')', extract=TRUE)))->story_data
# 
# story_data %>% 
#   group_by(Store.Number,Store.Name) %>%
#   summarise(Lon = mean(Lon,na.rm = 1),
#             Lat = mean(Lat,na.rm = 1)) -> store_info
# 
# store_info <- store_info[!duplicated(store_info$Store.Number),]
# 
# 
# story_data %>% 
#   select(-Lon,-Lat,-Store.Name) %>% 
#   left_join(store_info,by='Store.Number') -> story_data

load(file = 'data/story.RData')

story_data %>% mutate(Date = mdy(Date),
                      City = tolower(City)) %>% 
  filter(Lon !='NaN')-> story_data


library(shiny) 



# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Iowa Liquor Sales-Story"),
  
  # Sidebar layout with input and output definitions ----
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
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
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
           Date >= input$date[1]& Date <= input$date[2]) %>% 
    mutate(Category.Name = toupper(Category.Name))
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

shinyApp(ui = ui, server = server)    
