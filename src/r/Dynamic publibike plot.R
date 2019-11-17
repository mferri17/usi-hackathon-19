#Paolo Montemurro, Marco Ferri USI Hackaton 16/11/2019
# 6D
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

data2       <- read.csv("data_last2.csv")

#Define intensity and size
data2$size   <- abs(rnorm(length(data2$hour),30,30))
data2$from_avgDistance <- round(data2$from_avgDistance,0)
data2$to_avgDistance <- round(data2$to_avgDistance,0)
data2$label  <- paste0('<strong>', data2$station, '</strong> <br>',"Rides starting from here:" ,data2$from_count) %>% lapply(htmltools::HTML)
data2$popup  <- as.character(paste("Avg outcoming trip: <strong>", data2$from_avgDistance,"</strong> Meters", "<br>","Avg incoming trip:  <strong> " ,data2$to_avgDistance,"</strong> Meters <br>"))

#Define page
ui <- fluidPage(
  h1("Outgoing vs Ingoing Publibike-trips by station"),
  sidebarLayout(position = "left",
                mainPanel(
                  fluidRow(
                    splitLayout(
                      cellWidths = c("50%", "50%"),leafletOutput(outputId = "mymap",height=640, width = 480),leafletOutput(outputId = "mymap2",height=640, width = 480)))),
                sidebarPanel(width = 4,
                             h1("Hour of the day"),
                             sliderInput(inputId = "hour", label = "Hour of the day:", min = 0,max = 23, step = 1, value = 0,width = "90%"))
  ))

server <- function(input, output) { 
  colour_list = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026","#800026","#800026","#800026","#800026","#800026","#800026","#800026")
  
  #Define palette
  pal <- colorNumeric( palette = colour_list, domain = c(0,0.48))

  
  
  #Plot the map without any circle
  output$mymap <- renderLeaflet({
    leaflet(data2) %>% 
      setView(lng = 8.9565244, lat = 46.0052856, zoom = 15)  %>% #Set swizzera
      addTiles()})
  output$mymap2 <- renderLeaflet({
    leaflet(data2) %>% 
      setView(lng = 8.9565244, lat = 46.0052856, zoom = 15)  %>%
      addTiles() })
  
  #Some interactivity!
  observe({
    data_hour   <- subset(data2, hour == input$hour)
    leafletProxy("mymap", data = data_hour) %>%
      clearShapes()  %>% clearControls() %>%
      addCircles(data = data_hour, lat = ~ lat, lng = ~ lon, weight = 4, color = "black",
                 radius = 10*(data_hour$from_count)^0.5, fillOpacity = 0.7, fillColor = ~pal(from_freq),
                 label = ~label, popup = ~popup) %>%
      addLegend("bottomleft", pal = pal, values = data2$from_freq, title = " Outgoing relative usage")
  })
  observe({
    data_hour   <- subset(data2, hour == input$hour)
    leafletProxy("mymap2", data = data_hour) %>%
      clearShapes()  %>% clearControls() %>%
      addCircles(data = data_hour, lat = ~ lat, lng = ~ lon, weight = 4, color = "black",
                 radius = 10*(data_hour$to_count)^0.5, fillOpacity = 0.7, fillColor = ~pal(to_freq),
                 label = ~label, popup = ~popup) %>%
      addLegend("bottomright", pal = pal, values = data2$from_freq, title = "Ingoing relative usage")})
}


shinyApp(ui, server)
