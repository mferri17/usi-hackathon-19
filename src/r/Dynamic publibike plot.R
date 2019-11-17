#Paolo Montemurro, Marco Ferri USI Hackaton 2019
# 6D
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

data       <- read.csv("gather_bike.csv")

#Define intensity and size
data$from_avgDistance <- round(data$from_avgDistance,0)
data$to_avgDistance <- round(data$to_avgDistance,0)
data$label  <- paste0('<strong>', data$station, '</strong> <br>',"Rides starting from here:" ,data$from_count) %>% lapply(htmltools::HTML)
data$popup  <- as.character(paste("Avg outcoming trip: <strong>", data$from_avgDistance,"</strong> Meters", "<br>","Avg incoming trip:  <strong> " ,data$to_avgDistance,"</strong> Meters <br>"))

data2 <- data
# data2  <- subset(data, DoW=="A")
# data2 <- subset(data, period=="F")

#Define page
ui <- fluidPage(
  h1("Behaviour of outgoing vs ingoing Publibike-trips by station"),
  sidebarLayout(position = "left",
                mainPanel(
                  fluidRow(
                    splitLayout(
                      cellWidths = c("50%", "50%"),leafletOutput(outputId = "mymap",height=640, width = 480),leafletOutput(outputId = "mymap2",height=640, width = 480)))),
                sidebarPanel(width = 4,
                             h3("Filters"),
                             selectInput(inputId = "period", 
                                         label = "Season",
                                         choices = c("Summer","Fall"), 
                                         selected = "Fall"),
                             selectInput(inputId = "dow", 
                                         label = "Days of Week",
                                         choices = c("All","Working Days","Weekends"), 
                                         selected = "All"),
                             sliderInput(inputId = "hour", label = "Hour of the day:", min = 0,max = 23, step = 1, value = 7,width = "90%"))
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
    dow = if(input$dow == "All") "A" else { if(input$dow == "Working Days") "D" else "W" }
    season = if(input$period == "Fall") "F" else "S"
    
    data2       <- subset(data, DoW == dow)
    data2       <- subset(data2, period == season)
    data_hour   <- subset(data2, hour == input$hour)
    
    leafletProxy("mymap", data = data_hour) %>%
      clearShapes()  %>% clearControls() %>%
      addCircles(data = data_hour, lat = ~ lat, lng = ~ lon, weight = 4, color = "black",
                 radius = 10*(data_hour$from_count)^0.5, fillOpacity = 0.7, fillColor = ~pal(from_freq),
                 label = ~label, popup = ~popup) %>%
      addLegend("bottomleft", pal = pal, values = data2$from_freq, title = " Outgoing relative usage")
    
    leafletProxy("mymap2", data = data_hour) %>%
      clearShapes()  %>% clearControls() %>%
      addCircles(data = data_hour, lat = ~ lat, lng = ~ lon, weight = 4, color = "black",
                 radius = 10*(data_hour$to_count)^0.5, fillOpacity = 0.7, fillColor = ~pal(to_freq),
                 label = ~label, popup = ~popup) %>%
      addLegend("bottomright", pal = pal, values = data2$from_freq, title = "Ingoing relative usage")})
}


shinyApp(ui, server)
