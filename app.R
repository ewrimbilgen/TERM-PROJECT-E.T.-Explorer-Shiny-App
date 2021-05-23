

library(tidyverse)
library(tidytext)
library(ggmap)
library(stringr)
library(shiny)
library(dplyr)
library(RSQLite)
library(DBI)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(forecast)
library(plotly)
library(maps)
library(leaflet)
library(readr)
library(ggplot2)
library(RColorBrewer)
library(markdown)
library(knitr)



read_data <- function() {

  data <- read.csv("./Data/ETdata.csv", header=TRUE)
  print("Importing dataset...")
  data
}

Data <- read_data()

df <- read.csv("./Data/ETdata.csv", header=TRUE)

b64 <- base64enc::dataURI(file="www/Ufo-icon.png", mime="image/png")


#city state counts of sightings
state_counts <- df %>%
  filter(state != '')%>%
  count(city, state, sort=TRUE)%>%
  unite('location', -n, sep=',')

includeRmd <- function(path){
  contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
  html <- knit2html(text = contents, fragment.only = TRUE, options=c("use_xhtml","smartypants","mathjax","highlight_code", "base64_images"))
  Encoding(html) <- 'UTF-8'
  HTML(html)
}   


ui <- fluidPage(  
  tags$img(src = "Ufo-icon.jpg"),

  navbarPage(title = 'A basic analysis: Visiting by Extraterrestrials', 
                 theme = shinytheme('superhero'), 
        tabPanel('Analysis',
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("state_id", "Choose a state:",selected = "", choices = unique(Data$state)),
                     dateRangeInput("dates", "Pick a date:",
                                    start = "1920-01-01",
                                    end = "2020-01-01"),
                     actionButton('info','About data'),
                     tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  ")
                   ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel(
                         title = "Plot 1 (by state)", plotly::plotlyOutput("visiting"),
                       ),
                       tabPanel(
                         title = "Plot 2 (by cities)",plotOutput('bycountries'),
                       ),
                       tabPanel(
                         title = "Statistics",tableOutput("stsc")
                       )
                    
                   )
                   )
                   
                 )
               ),
        
        tabPanel("Map", 
                 leaflet::leafletOutput('map', width = "100%", height = 400),
                 absolutePanel(top = 130, right = 130, id = 'controls',
                               sliderInput('duration', 'how long', 'Minimum visiting duration', min = 0, max = 30000,
                                           value = 0.5, step = 0.1)),
                 selectInput("shape", "Choose a shape:",selected = "", choices = unique(Data$shape)),
                               dateRangeInput('date_range', 'Select Date', "2010-01-01", "2019-12-01")
                 ),
                 tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  "),

            
        
        tabPanel("About",
                   fluidRow(
                     column(10,includeRmd("ET.rmd"))
                   )
                 )
        
              )
  
)
                 


server <- function(input, output) {
  
  plot_visiting <- function(){
    
    Data %>%
      filter(
        state == input$state_id,
        date_sighted >= input$dates[1],
        date_sighted <= input$dates[2]
      ) %>%
      ggplot(aes(shape)) +
      geom_bar(fill="#66FF33", colour="black") +
      labs( title = 'state graph',
            x = "types of shape",
            y = "number of visits"
      )
    
  }
  
  
  output$visiting <- plotly::renderPlotly({
    plot_visiting()
  })
  
 
  
  
  byduration <- reactive({

    Data %>%
      filter(
        date_sighted >= input$date_range[1],
        date_sighted <= input$date_range[2],
        duration_sec >= input$duration,
        shape == input$shape
      )   
  })

  
  
  color <- colorFactor(topo.colors(15), Data$shape)

  output$map <- leaflet::renderLeaflet({
    byduration() %>%
      leaflet() %>% 
      addTiles() %>%
      setView( -98.58, 39.82, zoom = 5) %>% 
      addTiles() %>% 
      addCircleMarkers(popup = ~ comments, stroke=TRUE, fillOpacity=0.5,
                       color=~color(shape)
          ) %>%
          addLegend(
            "bottomleft", # Legend position
            pal=color, # color palette
            values=~shape, # legend values
            opacity = 1,
           title="Types of shape"
      )
  
  })
  
 
  
  state_counts_reac <- reactive({
    
    #city state counts
    state_counts <- df %>%
      filter(state == input$state_id)%>%
      count(city, state, sort=TRUE)%>%
      unite('location', -n, sep=',')
    
  })
  
 
  
  output$bycountries <- renderPlot({
    
    state_counts_reac() %>%
      filter(n > 20)%>%
      mutate(x = factor(location))%>%
      ggplot(aes(x, n))+
      geom_segment(aes(x=reorder(x,n), xend=x, y=0, yend=n), size=1)+
      geom_point(color='green', size=2, alpha=.6)+
      coord_flip()+
      theme_minimal() + 
      theme(axis.text.y = element_text(size = 13))+
      labs(title='Cities they like most')
    
   
  })
  
  
  observeEvent(input$info,{
    
    showModal(modalDialog(tags$img(
      src =b64 ),
      'This data reported by the National UFO Reporting Center', title='Important message',
                          
                          easyClose = FALSE
                          ))
    
  })

   

  output$stsc <- renderTable({
    Data %>%
      filter(
        state == input$state_id,
        date_sighted >= input$dates[1],
        date_sighted <= input$dates[2]
      ) %>%
      group_by(shape) %>%
      summarize(
        nb_sighted = n(),
        min_duration_min = min(duration_sec) / 60,
        max_duration_min = max(duration_sec) / 60,
        avg_duration_min = mean(duration_sec) / 60,
        Total_shape = length(shape)
      )
  })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
