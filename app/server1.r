library(shiny)
library(leaflet)
library(data.table)
library(devtools)
library(MASS)
library(dplyr)
library(tigris)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)
library(XML)
library(DT)
library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(dmm)
library(gridExtra)
restaurant<-read.csv("../data/restaurant_new1.csv")
income<-read.csv("../data/income1.csv")
yelp.rate<-read.csv("../data/zip.rate1.csv")
age_sex<-read.csv("../data/age_sex - new1.csv")
pie_type<-read.csv("../data/zip.prop1.csv")
load("../data/subdat.RData")
load("../data/housing.RData")
source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")
################################# Panel: I cook food ##################################

###Basic map setting
shinyServer(function(input, output,session){
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE))%>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 13)%>%
      addProviderTiles("OpenStreetMap.HOT")
  })
  
  pal <- colorNumeric(
    palette = "Greens",
    domain = subdat$value
  )
  
  leafletProxy("map",data=subdat)%>%
    addPolygons(layerId = ~ZIPCODE,
                stroke = T, weight=1,
                fillOpacity = 0.3,
                color = ~pal(value),
                highlightOptions = highlightOptions(color='#ff0000', opacity = 0.3, weight = 2, fillOpacity = 0.4,
                                                    bringToFront = TRUE, sendToBack = TRUE))
 
###Reactions when click the polygon
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
    ##Zoom in
      leafletProxy("map")%>%
      setView(click$lng,click$lat,zoom=15,options=list(animate=TRUE))
     
    ##Show average statistics
      #Zip Code
      zip<-paste("Zipcode: ",as.character(click[1]))
      output$zip_text<-renderText({zip})
      #Median Income
      income$Median_income <- as.numeric(as.character(income$Median_income))
      Med_income<-paste("Median Income: $",income[which(income$ZIP==as.character(click[1])),]$Median_income,sep="")
      output$med_income_text<-renderText({Med_income})
      #Population Density
      Total_population<-paste("Total_population(in thousands): ",age_sex[which(age_sex[,1]==as.character(click[1])),]$Total_Population,sep="")
      output$Total_population<-renderText({Total_population})
      #Female percentage
      Female.per<-paste("Female.percentage: ", round(age_sex[which(age_sex[,1]==as.character(click[1])),]$Female.per,2),sep="")
      output$Female.per<-renderText({Female.per})
      #Yelp rate
      yelp.rate<-paste("Average Yelp rating: ", yelp.rate[which(yelp.rate[,1]==as.character(click[1])),]$Yelp_star,sep="")
      output$yelp.rate<-renderText({yelp.rate})
      #Age group percentage
      row_age<-age_sex[which(age_sex[,1]==as.character(click[1])),]
      df_age<-data.frame(matrix(NA, nrow = 8, ncol = 3))
      #Female percentage and restaurant type
      output$pie_female <- renderPlot({
        for(i in 1:8){
          df_age[i,1]<-as.numeric(i)
          df_age[i,2]<-as.character(colnames(row_age)[i+3])
          df_age[i,3]<-as.numeric(row_age[1,i+3])
        }
        colnames(df_age)[1] <-""
        ap<- ggplot(df_age, aes(x="", y=X3, fill=X2))+ geom_bar(width = 1, stat = "identity")+theme(plot.margin=grid::unit(c(0,4,0,0), "mm"))
        pie <- ap + coord_polar(theta="y")+ylab("Age Distribution")+xlab("")+theme(legend.position="left")
      
        pie_type<-pie_type[which(pie_type[,1]==as.character(click[1])),]
        pie2<-ggplot(data=pie_type, aes(x=Type, y=Percentage)) +geom_bar(stat="identity")+theme(plot.margin = unit(c(0.1,0.5,0.1,0.1), "cm"))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Restaurant Type")
        
        pie_out<-grid.arrange(pie, pie2, nrow = 2)
        pie_out },height = 450, res = 72)
    })#Observe1
    
    dataInput <- reactive({
      restaurant[which(restaurant$ZIP == input$map_shape_click[1]),]
    })
    
###Show data labels    
    observeEvent(input$map_shape_click,{
      leafletProxy('map',data = dataInput())%>%clearGroup("new")%>%
        addCircleMarkers(
                         lat=~LAT,
                         lng=~LON,
                         label=~as.character(NAME),
                         radius=5,
                         stroke=FALSE,
                         fillColor = "green",
                         fillOpacity=0.7,
                         group="new",
                         labelOptions = labelOptions(
                           noHide = T,
                           offset=c(20,-15),
                           opacity=0.7,
                           direction="left",
                           style=list(
                             background="green",
                             color="white"  )))})
                         
})
#Shiny

