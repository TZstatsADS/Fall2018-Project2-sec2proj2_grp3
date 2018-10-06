library(ggmap)
library(ggplot2)
library(dplyr)
library(DT)

load("C:/Users/maysh/Documents/avg_price_zip.RData")
load("C:/Users/maysh/Documents/subdat.RData")


shinyServer(function(input, output,session){
     #################################################################
     ##### Panel 1 : I cook food  #####################################
     #################################################################
     output$map <- renderLeaflet({
       leaflet()%>%
         setView(lng = -73.98928, lat = 40.75042, zoom = 13)%>%
         addProviderTiles("OpenStreetMap.HOT")
    
     })
  
     ## Panel *: heat map###########################################
     # ----- set uo color pallette https://rstudio.github.io/leaflet/colors.html
     # Create a continuous palette function
     pal <- colorNumeric(
       palette = "Greens",
       domain = subdat$value
       )
     leafletProxy("map",data=subdat)%>%
       addPolygons(layerId = ~ZIPCODE,
                   stroke = T, weight=1,
                   fillOpacity = 0.35,
                   color = ~pal(value),
                   highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,
                                                       bringToFront = TRUE, sendToBack = TRUE))%>%
       addLegend(pal = pal, values = ~value, opacity = 1)
  
  
     ## Panel *: click on any area, popup text about this zipcode area's information#########
     observeEvent(input$map_shape_click, {
       ## track
       if(input$click_multi == FALSE) leafletProxy('map') %>%clearGroup("click")
       click <- input$map_shape_click
       leafletProxy('map')%>%
         addMarkers(click$lng, click$lat, group="click", icon=list(iconUrl='icon/leaves.png',iconSize=c(60,60)))
    
       ##info
       zip_sel<-as.character(revgeocode(as.numeric(c(click$lng,click$lat)),output="more")$postal_code)
       zip<-paste("ZIPCODE: ",zip_sel)
       price_avg<-paste("Average Price: $",avg_price_zip.df[avg_price_zip.df$region==zip_sel,"value"],sep="")
       
       leafletProxy("map")%>%
         setView(click$lng,click$lat,zoom=14,options=list(animate=TRUE))
       
       output$zip_text<-renderText({zip})
       output$avgprice_text<-renderText({price_avg})
       
     })
  
    ## Panel *: Return to big view##################################
     observeEvent(input$click_reset_buttom,{
       if(input$click_reset_buttom){
         leafletProxy("map")%>%
           setView(lng = -73.98928, lat = 40.75042, zoom = 13)%>% 
           clearPopups()
       }
     })
  
  
})






