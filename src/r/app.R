#Paolo Montemurro, Marco Ferri USI Hackaton 16/11/2019
# 6D
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

rsconnect::setAccountInfo(name='montep',
                          token='B2FEDDA8329AC425C8ADE97C312FF4EA',
                          secret='9jptoxzstVx8zSNkzVuohiNOEwGss8PGxyJsOMAn')

library(rsconnect)
rsconnect::deployApp('path/to/your/app')


data2       <- read.csv("data_last2.csv")

#Define intensity and size
data2$size   <- abs(rnorm(length(data2$hour),30,30))
data2$from_avgDistance <- round(data2$from_avgDistance,0)
data2$to_avgDistance <- round(data2$to_avgDistance,0)
data2$label  <- paste0('<strong>', data2$station, '</strong> <br>',"Rides starting from here:" ,data2$from_count) %>% lapply(htmltools::HTML)
data2$popup  <- as.character(paste("Avg outcoming trip: <strong>", data2$from_avgDistance,"</strong> Meters", "<br>","Avg incoming trip:  <strong> " ,data2$to_avgDistance,"</strong> Meters <br>"))

#Define page
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                    
                    h1 {
                    font-weight: 200;
                    text-align: center;
                    color: #48ca3b;
                    }
                    
                    "))
    ),
  
  sidebarLayout(position = "left",
    mainPanel(h1("pidsesello")),
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"),leafletOutput(outputId = "mymap",height=600, width = 600),leafletOutput(outputId = "mymap2",height=600, width = 600)))),
    sidebarPanel(
      h1("Student grades"),
      sliderInput(inputId = "hour", label = "Hour of the day:", min = 0,max = 23, step = 1, value = 1))
  )

server <- function(input, output) { 
  draw = FALSE
  colour_list = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026")

  #Define palette
  pal <- colorNumeric( palette = colour_list, domain = data2$from_freq)
  #Plot the map without any circle
  output$mymap <- renderLeaflet({
    leaflet(data2) %>% 
      setView(lng = 8.9565244, lat = 46.0052856, zoom = 15)  %>% #Set swizzera
      addTiles() })
  output$mymap2 <- renderLeaflet({
    leaflet(data2) %>% 
      setView(lng = 8.9565244, lat = 46.0052856, zoom = 15)  %>% #Set swizzera
      addTiles() })
  
  #Some interactivity!
  observe({
    data_hour   <- subset(data2, hour == input$hour)
    leafletProxy("mymap", data = data_hour) %>%
      clearShapes()  %>%
      addCircles(data = data_hour, lat = ~ lat, lng = ~ lon, weight = 4, color = "black",
                radius = 10*(data_hour$from_count)^0.5, fillOpacity = 0.7, fillColor = ~pal(from_freq),
                label = ~label, popup = ~popup)})
  observe({
    data_hour   <- subset(data2, hour == input$hour)
    leafletProxy("mymap2", data = data_hour) %>%
      clearShapes()  %>%
      addCircles(data = data_hour, lat = ~ lat, lng = ~ lon, weight = 4, color = "black",
                 radius = 10*(data_hour$to_count)^0.5, fillOpacity = 0.7, fillColor = ~pal(to_freq),
                 label = ~label, popup = ~popup)})
}


shinyApp(ui, server)

