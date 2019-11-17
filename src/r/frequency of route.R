library(curl)
library(dplyr)
library(shiny)
library(googleway)
library(leaflet)

data2 = read.csv("preferred_routes_undirected_latlon.csv")
data2 = data2[c(1:12),]

mykey <- "AIzaSyAWJiBl6Aa78D9Kl6BfV3tMC1lKp9UpBn8"

lapply(1:nrow(data2), function(x){
  foo <- google_directions(origin = unlist(data2[x, 4:5]),
                           destination = unlist(data2[x, 7:8]),
                           key = mykey,
                           mode = "walking",avoid = c("ferries","tolls"),
                           simplify = TRUE)
  
  
  pl <- decode_pl(foo$routes$overview_polyline$points)
  return(pl)
}

) %>%
  bind_rows(.id = "region") -> temp

colour_list = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026","#800026","#800026","#800026","#800026","#800026","#800026","#800026","#800026")
pal <- colorNumeric( palette = colour_list, domain = c(50,1605))

m <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolylines(data = temp, lng = ~lon, lat = ~lat, color = "black", opacity = 0.5,weight=5) %>%
  
  addCircles(data = data2, lng = ~B_lon, lat = ~B_lat, radius = (data2$occurences)^0.7,label = paste0(data2$B," - ",data2$A," #",data2$occurences), fillColor =  "orange",  weight = 3, color = "blue") %>%
  addCircles(data = data2, lng = ~A_lon, lat = ~A_lat, radius = (data2$occurences)^0.7,label = paste0(data2$A," - ",data2$B," #",data2$occurences), fillColor = "orange", weight =3, color = "blue")

html_legend <- " <p style='text-align:center'>Frequency of route</p><table style='width:100%'>
<tr>
<td style='text-align: center'><img src='https://i.ibb.co/JRSbwtp/circle.png' style='width:45%'></td>
<td>High</td>
</tr>
<tr>
<td style='text-align: center'><img src='https://i.ibb.co/JRSbwtp/circle.png' style='width:35%'></td>
<td >Medium</td>
</tr>  
<tr>
<td style='text-align: center'><img src='https://i.ibb.co/JRSbwtp/circle.png' style='width:20%'></td>
<td >Low</td>
</tr>
</table>"
html_title <- "<h1 style='margin:auto;text-align: center;'>Most common routes</h1>"
addControl(m,html = html_legend, position = "bottomright") %>%
  addControl(m,html=html_title, position = "topright")
#addControl(m,html = html_title,position=NULL)