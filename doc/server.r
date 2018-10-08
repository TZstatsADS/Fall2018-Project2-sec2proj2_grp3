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
getwd()
load("../data/avg_price_zip.RData")
load("../data/subdat.RData")
load("../data/housing.RData")  
source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")

color <- list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
              color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
              color3 = c("#F7FCF5","#74C476", "#005A32"))


getwd()
shinyServer(function(input, output,session) {
  
  #Esri.WorldTopoMap
  #########main map######
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
      addMarkers(data=housing,
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster"
      )
  })
  
  
  
  #############Housing#############
  
  
  # filter housing data:
  
  housingFilter=reactive({
    bedroom_filter=housing$bedrooms>input$min_bedrooms 
    bathroom_filter=housing$bathrooms>input$min_bathrooms
    price_filter=housing$price>=input$min_price & housing$price<=input$max_price
    filter=bedroom_filter & bathroom_filter & price_filter
    return(housing[filter,])
  })
  
  # show data in the map:
  observe({leafletProxy("map")%>%clearGroup("housing_cluster")%>%
      addMarkers(data=housingFilter(),
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster"
      )
  })
  # show current status of icons:
  showStatus=reactive({
    if (is.null(input$map_bounds)){
      return("cloud")
      
    }
    else{
      if(input$map_zoom<16){
        return('cloud')
      }
      else{
        return('details')
      }
    }
  })
  # hide and show clouds 
  observe({
    if(showStatus()=="cloud"){
      
      leafletProxy("map") %>%showGroup("housing_cluster")%>%clearGroup("new_added")
    }
    else{
      leafletProxy("map") %>%hideGroup("housing_cluster")
      
    }
  })
  
  
  # get the housing data in the bounds
  marksInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(housing[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    return(
      subset(housingFilter(),
             lat>= latRng[1] & lat <= latRng[2] &
               lng >= lngRng[1] & lng <= lngRng[2])
    )
  })
  
  # show housing details when zoom to one specific level
  observe({
    if(showStatus()=="details"){
      if(nrow(marksInBounds())!=0){
        leafletProxy("map")%>%clearGroup(group="new_added")%>% 
          addCircleMarkers(data=marksInBounds(),
                           lat=~lat,
                           lng=~lng,
                           label=~as.character(price),
                           radius=5,
                           stroke=FALSE,
                           fillColor = "green",
                           fillOpacity=0.7,
                           group="new_added",
                           labelOptions = labelOptions(
                             noHide = T,
                             offset=c(20,-15),
                             opacity=0.7,
                             direction="left",
                             style=list(
                               background="green",
                               color="white"  
                             )
                           )
          )
      }
      else{
        leafletProxy("map")%>%clearGroup(group="new_added")
      }
    }
  })
  
  
  # sort housing in current zoom level
  observe({
    housing_sort=marksInBounds()

    if(nrow(housing_sort)!=0){

      action=apply(housing_sort,1,function(r){
        addr=r["addr"]
        lat=r["lat"]
        lng=r["lng"]
        paste0("<a class='go-map' href='' data-lat='",lat,"'data-lng='",lng,"'>",addr,'</a>')
      }
      )

      housing_sort$addr=action
      output$rank <- renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")],escape=F)

    }
    else{

      output$rank=renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")])
    }

  })
  
  # When point in map is hovered, show a popup with housing info
  observe({
    
    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    if(showStatus()=="details"){
      isolate({
        showPopupHover(event$lat, event$lng,housing=housingFilter())
      })  
    }
    
  })
  
  # mouseout the point and cancel popup
  observe({
    
    event <- input$map_marker_mouseout
    if (is.null(event))
      return()
    
    isolate({
      leafletProxy("map") %>% clearPopups()
    })
  })
  
  # click name to go to that point
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      
      
      
      lat <- as.numeric(input$goto$lat)
      lng <- as.numeric(input$goto$lng)
      
      map %>% setView(lng = lng, lat = lat, zoom = 16)
    })
  })
  # hover the list to show info
  observe({
    if (is.null(input$showPop))
      return()
    isolate({
      remove=as.numeric(input$showPop$remove)
      map <- leafletProxy("map")
      
      if(remove==0){
        
        
        
        lat <- as.numeric(input$showPop$lat)
        lng <- as.numeric(input$showPop$lng)
        showPopupHover(lat, lng,housingFilter())   
      }
      else{
        map %>% clearPopups()
      }
      
      
    })
  })
  
  #############Search###############
  observeEvent(input$button1,{
    url = paste0('http://maps.google.com/maps/api/geocode/xml?address=',input$location,'&sensor=false')
    doc = xmlTreeParse(url) 
    root = xmlRoot(doc) 
    lati = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lat']])) 
    long = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lng']]))
    
    leafletProxy("map") %>%
      setView(lng=long, lat=lati,zoom=15)%>%
      addMarkers(lng=long,lat=lati,layerId = "1",icon=icons(
        iconUrl = "../lib/icons8-Location-50.png",iconWidth = 25, iconHeight = 25))
  })
  
  #############Reset for Panel 2############
  observeEvent(input$button2,{
    proxy<-leafletProxy("map")
    proxy %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
      removeMarker(layerId="1") %>%
      addMarkers(data=housing,
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster")
    updateTextInput(session, inputId="location", value = "")
    
    updateSelectInput(session, "check2_class",selected="")
    updateSelectInput(session, "check2_age",selected="")
    updateSelectInput(session, "check2_crime",selected = "")
    updateSelectInput(session, "check2_trans",selected = "")
    updateSelectInput(session, "check2_rt",selected = "")
  })
  
  
  #############Clear button###########
  observeEvent(input$clear, {
    leafletProxy('map')%>% setView(lng = -73.971035, lat = 40.775659, zoom = 12)
  })
  
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
  
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE))%>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      setView(lng = -73.96, lat = 40.75042, zoom = 13)%>%
      
      addPolygons(layerId = ~ZIPCODE,data=subdat,
                  stroke = T, weight=1,
                  fillOpacity = 0.35,
                  color = ~pal(value),
                  highlightOptions = highlightOptions(color='#ff0000', opacity = 0.5, weight = 4, fillOpacity = 0.9,
                                                      bringToFront = TRUE, sendToBack = TRUE))%>%
      addLegend(data=subdat,pal = pal, values = ~value, opacity = 0.5)
    
  })
  
  ## Panel *: click on any area, popup text about this zipcode area's information#########
  observeEvent(input$map_shape_click, {
    ## track
    if(input$click_multi == FALSE) leafletProxy('map1') %>%clearGroup("click")
    click <- input$map_shape_click
    leafletProxy('map1')%>%
      addMarkers(click$lng, click$lat, group="click", icon=list(iconUrl='icon/leaves.png',iconSize=c(60,60)))
    
    ##info
    zip_sel<-as.character(revgeocode(as.numeric(c(click$lng,click$lat)),output="more")$postal_code)
    zip<-paste("ZIPCODE: ",zip_sel)
    price_avg<-paste("Average Price: $",avg_price_zip.df[avg_price_zip.df$region==zip_sel,"value"],sep="")
    
    leafletProxy("map1")%>%
      setView(click$lng,click$lat,zoom=14,options=list(animate=TRUE))
    
    output$zip_text<-renderText({zip})
    output$avgprice_text<-renderText({price_avg})
    
  })
  
  
  ## Panel *: Return to big view##################################
  observeEvent(input$click_reset_buttom,{
    if(input$click_reset_buttom){
      leafletProxy("map1")%>%
        setView(lng = -73.96, lat = 40.75042, zoom = 13)%>% 
        clearPopups()
    }
  })
  
  ###Panel 2: Owner's Choice
  
  # areas  <- reactive({
  #   cond.0 <- if(is.null(input$check2_ty)){paste0("Studio <= ", input$check2_pr, " |is.na(Studio) == TRUE")
  #   } else if("Studio" %in% input$check2_ty){paste0("Studio <= ", input$check2_pr)
  #   } else{"Studio <= 5400 |is.na(Studio) == TRUE"}
  #   
  #   cond.1 <- if(is.null(input$check2_ty)){paste0("X1B <= ", input$check2_pr, " |is.na(X1B) == TRUE")
  #   } else if("1B" %in% input$check2_ty){paste0("X1B <= ", input$check2_pr)
  #   } else{"X1B <= 5400 |is.na(X1B) == TRUE"}
  #   
  #   cond.2 <- if(is.null(input$check2_ty)){paste0("X2B <= ", input$check2_pr, " |is.na(X2B) == TRUE")
  #   } else if("2B" %in% input$check2_ty) {paste0("X2B <= ", input$check2_pr)
  #   } else{"X2B <= 5400 |is.na(X2B) == TRUE"}
  #   
  #   cond.3 <- if(is.null(input$check2_ty)){paste0("X3B <= ", input$check2_pr, " |is.na(X3B) == TRUE")
  #   } else if("3B" %in% input$check2_ty) {paste0("X3B <= ", input$check2_pr)
  #   } else{"X3B <= 5400 |is.na(X3B) == TRUE"}
  #   
  #   cond.4 <-  if(is.null(input$check2_ty)){paste0("X4B <= ", input$check2_pr, " |is.na(X4B) == TRUE")
  #   } else if("4B" %in% input$check2_ty) {paste0("X4B <= ", input$check2_pr)
  #   } else{"X4B <= 5400 |is.na(X4B) == TRUE"}
  #   
  #   cond.ame <- if(is.null(input$check2_re)){"ranking.American <= 46 |is.na(ranking.American) == TRUE"
  #   } else if("American" %in% input$check2_re){"ranking.American <= 23"
  #   } else {"ranking.American <= 46 |is.na(ranking.American) == TRUE"}
  #   
  #   cond.chi <- if(is.null(input$check2_re)){"ranking.Chinese <= 46 |is.na(ranking.Chinese) == TRUE"
  #   } else if("Chinese" %in% input$check2_re) {"ranking.Chinese <= 23"
  #   } else {"ranking.Chinese <= 46 |is.na(ranking.Chinese) == TRUE"}
  #   
  #   cond.ita <-  if(is.null(input$check2_re)){"ranking.Italian <= 46 |is.na(ranking.Italian) == TRUE"
  #   } else if("Italian" %in% input$check2_re) {"ranking.Italian <= 23"
  #   } else {"ranking.Italian <= 46 |is.na(ranking.Italian) == TRUE"}
  #   
  #   cond.jap <- if(is.null(input$check2_re)){"ranking.Japenses <= 46 |is.na(ranking.Japenses) == TRUE"
  #   } else if("Japanese" %in% input$check2_re) {"ranking.Japenses <= 23"
  #   } else {"ranking.Japenses <= 46 |is.na(ranking.Japenses) == TRUE"}
  #   
  #   cond.piz <- if(is.null(input$check2_re)){"ranking.Pizza <= 46 |is.na(ranking.Pizza) == TRUE"
  #   } else if("Pizza" %in% input$check2_re) {"ranking.Pizza <= 23"
  #   } else {"ranking.Pizza <= 46 |is.na(ranking.Pizza) == TRUE"}
  #   
  #   cond.oth <- if(is.null(input$check2_re)){"ranking.Others <= 46 |is.na(ranking.Others) == TRUE"
  #   } else if("Others" %in% input$check2_re) {"ranking.Others <= 23"
  #   } else {"ranking.Others <= 46 |is.na(ranking.Others) == TRUE"}
  #   
  #   trans.fil <- if(input$check2_tr == "It's everything."){
  #     1:16
  #   } else if(input$check2_tr == "Emmm."){
  #     1:32
  #   } else {
  #     c(1:46, NA)
  #   }
  #   
  #   club.fil <- if(input$check2_cb == "Let's party!"){1:16
  #   } else if(input$check2_cb == "Drink one or two."){
  #     1:32
  #   } else {
  #     c(1:46, NA)
  #   }
  #   
  #   theatre.fil<- if(input$check2_ct == "Theatre goers."){1:16
  #   } else if(input$check2_ct == "It depends."){
  #     1:32
  #   } else {
  #     c(1:46, NA)
  #   }
  #   
  #   market.fil <- if(input$check2_ma == "Love it!"){
  #     1:16
  #   } else if(input$check2_ma == "It depends."){
  #     1:32
  #   } else {
  #     c(1:46, NA)
  #   }
  #   
  #   areas <- (rank_all %>%
  #               filter(eval(parse(text = cond.apt.0)), eval(parse(text = cond.apt.1)), eval(parse(text = cond.apt.2)),
  #                      eval(parse(text = cond.apt.3)), eval(parse(text = cond.apt.4)),
  #                      eval(parse(text = cond.ame)), eval(parse(text = cond.chi)), eval(parse(text = cond.ita)),
  #                      eval(parse(text = cond.jap)), eval(parse(text = cond.piz)), eval(parse(text = cond.oth)),
  #                      ranking.trans %in% trans.fil, ranking.bar %in% club.fil,
  #                      ranking.theatre %in% theatre.fil, ranking.market %in% market.fil
  #               ) %>%
  #               select(zipcode))[,1]
  #   return(areas)
  # })
  # 
  # output$rank <- renderDataTable(show %>% 
  #                                  filter(Zipcode %in% areas()),
  #                                 options = list("sScrollX" = "100%", "bLengthChange" = FALSE))
  
  #############
  ##map
  # observe({
  #   if(length(areas())!=0){
  #     leafletProxy("map3")%>%clearGroup(group="new_added")%>% 
  #       addPolygons(data=subset(subdat, subdat$ZIPCODE%in% areas()),
  #                   weight = 2,
  #                   color = "#34675C",
  #                   fillColor = "#B3C100",
  #                   fillOpacity=0.7,
  #                   group="new_added",
  #                   noClip = TRUE, label = ~ZIPCODE)
  #   }
  #   
  #   else{
  #     leafletProxy("map3")%>%clearGroup(group="new_added")
  #   }
  # })
  
  
})




