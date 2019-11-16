#Paolo Montemurro, Marco Ferri USI Hackaton 16/11/2019
# 6D

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

data2       <- read.csv("data_last2.csv")
time1 = seq(0,22,2)
time2 = time1+1


#Define intensity and size
data2$size   <- abs(rnorm(length(data2$hour),30,30))
data2$from_avgDistance <- round(data2$from_avgDistance,0)
data2$to_avgDistance <- round(data2$to_avgDistance,0)
data2$label  <- paste0('<strong>', data2$station, '</strong> <br>',"Rides starting from here:" ,data2$from_count) %>% lapply(htmltools::HTML)
data2$popup  <- as.character(paste("From avg distance: ", data2$from_avgDistance,"Meters", "<br>","To avg distance" ,data2$to_avgDistance,"Meters"))

ui <- fluidPage(
  mainPanel(leafletOutput(outputId = "mymap")),
  sidebarPanel(
    sliderInput(inputId = "hour", label = "Hour of the day:", min = 0,max = 23, step = 1, value = 1))
  )

server <- function(input, output) { 
  draw = FALSE
  colour_list = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026")

  pal <- colorNumeric(
    palette = colour_list, domain = data2$from_freq)
  
  output$mymap <- renderLeaflet({
    leaflet(data2) %>% 
      setView(lng = 8.9565244, lat = 46.0052856, zoom = 15)  %>% #Set swizzera
      addTiles() })
  

  observe({
    data_hour   <- subset(data2, hour == input$hour)
    leafletProxy("mymap", data = data_hour) %>%
      clearShapes()  %>%
      addCircles(data = data_hour, lat = ~ lat, lng = ~ lon, weight = 4, color = "black",
                             radius = 10*(data_hour$from_count)^0.5, fillOpacity = 0.7, fillColor = ~pal(from_freq)
                 , label = ~label, popup = ~popup)
  })
}


shinyApp(ui, server)

