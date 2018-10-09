# library(shiny)
# library(leaflet)
# library(data.table)
# library(devtools)
# library(MASS)
# library(dplyr)
# library(tigris)
# library(sp)
# library(maptools)
# library(broom)
# library(httr)
# library(rgdal)
# library(RColorBrewer)
# library(XML)
# library(DT)
# library(tidyr)
# library(ggplot2)
# library(ggmap)

library(ggmap)
library(ggplot2)
library(dplyr)
library(DT)


load("../data/avg_price_zip.RData")
load("../data/subdat.RData")
load("../data/housing.RData")  
source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")

filter_data <- read.csv("../data/filter_data_used.csv", as.is = T)
table_display <- read.csv("../data/new_data/income.csv", as.is = T)

color <- list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
              color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
              color3 = c("#F7FCF5","#74C476", "#005A32"))

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

  output$map3 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      setView(lng = -73.98097, lat = 40.7562, zoom = 12)
    })

  observeEvent(input$click_back_button,{
    if(input$click_back_button){
      leafletProxy("map3") %>%
        setView(lng = -73.98097, lat = 40.7562, zoom = 12)
      }
    })

  areas <- reactive({
    cond.0 <- if(is.null(input$check2_type)){paste0("pop_american >= ", input$check2_ppr, " | is.na(pop_american) == TRUE")
    } else if("American" %in% input$check2_type){paste0("pop_american >= ", input$check2_ppr)
    } else{"pop_american >= 500 | is.na(pop_american) == TRUE"}

    cond.1 <- if(is.null(input$check2_type)){paste0("pop_quickmeal >= ", input$check2_ppr, " | is.na(pop_quickmeal) == TRUE")
    } else if("Quick Meal" %in% input$check2_type){paste0("pop_quickmeal >= ", input$check2_ppr)
    } else{"pop_quickmeal >= 500 | is.na(pop_quickmeal) == TRUE"}

    cond.2 <- if(is.null(input$check2_type)){paste0("pop_asian >= ", input$check2_ppr, " | is.na(pop_asian) == TRUE")
    } else if("Asian" %in% input$check2_type){paste0("pop_asian >= ", input$check2_ppr)
    } else{"pop_asian >= 500 | is.na(pop_asian) == TRUE"}

    cond.3 <- if(is.null(input$check2_type)){paste0("pop_chinese >= ", input$check2_ppr, " | is.na(pop_chinese) == TRUE")
    } else if("Chinese" %in% input$check2_type){paste0("pop_chinese >= ", input$check2_ppr)
    } else{"pop_chinese >= 500 | is.na(pop_chinese) == TRUE"}

    cond.4 <- if(is.null(input$check2_type)){paste0("pop_dessert >= ", input$check2_ppr, " | is.na(pop_dessert) == TRUE")
    } else if("Dessert" %in% input$check2_type){paste0("pop_dessert >= ", input$check2_ppr)
    } else{"pop_dessert >= 500 | is.na(pop_dessert) == TRUE"}

    cond.5 <- if(is.null(input$check2_type)){paste0("pop_european >= ", input$check2_ppr, " | is.na(pop_european) == TRUE")
    } else if("European" %in% input$check2_type){paste0("pop_european >= ", input$check2_ppr)
    } else{"pop_european >= 500 | is.na(pop_european) == TRUE"}

    cond.6 <- if(is.null(input$check2_type)){paste0("pop_italian >= ", input$check2_ppr, " | is.na(pop_italian) == TRUE")
    } else if("Italian" %in% input$check2_type){paste0("pop_italian >= ", input$check2_ppr)
    } else{"pop_italian >= 500 | is.na(pop_italian) == TRUE"}

    cond.7 <- if(is.null(input$check2_type)){paste0("pop_mexican >= ", input$check2_ppr, " | is.na(pop_mexican) == TRUE")
    } else if("Mexican" %in% input$check2_type){paste0("pop_mexican >= ", input$check2_ppr)
    } else{"pop_mexican >= 500 | is.na(pop_mexican) == TRUE"}

    cond.8 <- if(is.null(input$check2_type)){paste0("pop_seafood >= ", input$check2_ppr, " | is.na(pop_seafood) == TRUE")
    } else if("Seafood" %in% input$check2_type){paste0("pop_seafood >= ", input$check2_ppr)
    } else{"pop_seafood >= 500 | is.na(pop_seafood) == TRUE"}

    cond.9 <- if(is.null(input$check2_type)){paste0("pop_other >= ", input$check2_ppr, " | is.na(pop_other) == TRUE")
    } else if("Others" %in% input$check2_type){paste0("pop_other >= ", input$check2_ppr)
    } else{"pop_other >= 500 | is.na(pop_other) == TRUE"}

    cond.class1 <- if("Upper" %in% input$check2_class){"class_1 == 1"} else {"is.na(class_1) == FALSE"}

    cond.class2 <- if("Middle" %in% input$check2_class){"class_2 == 1"} else {"is.na(class_2) == FALSE"}

    cond.class3 <- if("Working" %in% input$check2_class){"class_3 == 1"} else {"is.na(class_3) == FALSE"}

    cond.class4 <- if("Lower" %in% input$check2_class){"class_4 == 1"} else {"is.na(class_4) == FALSE"}

    cond.age1 <- if(is.null(input$check2_age)){"below5 <= 43 | is.na(below5) == TRUE"
    } else if("<5" %in% input$check2_age){"below5 <= 20"
    } else {"below5 <= 43 | is.na(below5) == TRUE"}

    cond.age2 <- if(is.null(input$check2_re)){"X5_14 <= 43 | is.na(X5_14) == TRUE"
    } else if("5-14" %in% input$check2_re) {"X5_14 <= 20"
    } else {"X5_14 <= 46 | is.na(X5_14) == TRUE"}

    cond.age3 <-  if(is.null(input$check2_re)){"X15_24 <= 43 | is.na(X15_24) == TRUE"
    } else if("15-24" %in% input$check2_re) {"X15_24 <= 20"
    } else {"X15_24 <= 43 | is.na(X15_24) == TRUE"}

    cond.age4 <- if(is.null(input$check2_re)){"X25_34 <= 43 | is.na(X25_34) == TRUE"
    } else if("25-34" %in% input$check2_re) {"X25_34 <= 20"
    } else {"X25_34 <= 43 | is.na(X25_34) == TRUE"}

    cond.age5 <- if(is.null(input$check2_re)){"X35_44 <= 43 | is.na(X35_44) == TRUE"
    } else if("35-44" %in% input$check2_re) {"X35_44 <= 20"
    } else {"X35_44 <= 43 | is.na(X35_44) == TRUE"}

    cond.age6 <- if(is.null(input$check2_re)){"X45_54 <= 43 | is.na(X45_54) == TRUE"
    } else if("45-54" %in% input$check2_re) {"X45_54 <= 20"
    } else {"X45_54 <= 43 |is.na(X45_54) == TRUE"}

    cond.age7 <- if(is.null(input$check2_re)){"X55_64 <= 43 | is.na(X55_64) == TRUE"
    } else if("55-64" %in% input$check2_re) {"X55_64 <= 20"
    } else {"X55_64 <= 43 | is.na(X55_64) == TRUE"}

    cond.age8 <- if(is.null(input$check2_re)){"X65above <= 43 | is.na(X65above) == TRUE"
    } else if("65+" %in% input$check2_re) {"X65above <= 20"
    } else {"X65above <= 43 | is.na(X65above) == TRUE"}

    cond.crime <- if(input$check2_crime == "Very Safe"){"crime_level == 'Safe'"
    } else if(input$check2_crime == "Safe"){"crime_level == 'Safe' | crime_level == 'Relatively Safe'"
    } else if(input$check2_crime == "A Little Dangerous"){"crime_level == 'Safe' | crime_level == 'Relatively Safe' | crime_level == 'Relatively Dangerous'"
    } else {"is.na(crime_level) == FALSE"}

  #   market.fil <- if(input$check2_market == "Love it!"){
  #     1:16
  #   } else if(input$check2_market == "It depends."){
  #     1:32
  #   } else {
  #     c(1:46, NA)
  #   }
    
    trans.fil <- if(input$check2_trans == "1"){
      25
    } else if(input$check2_trans == "2"){
      15
    } else {
      1
    }
  #   
  #   
  #   theatre.fil<- if(input$check2_ct == "Theatre goers."){1:16
  #   } else if(input$check2_ct == "It depends."){
  #     1:32
  #   } else {
  #     c(1:46, NA)
  #   }
  #   
  #   
    areas <- (filter_data %>%
                filter(eval(parse(text = cond.0)), eval(parse(text = cond.1)), eval(parse(text = cond.2)), eval(parse(text = cond.3)),
                       eval(parse(text = cond.4)), eval(parse(text = cond.5)), eval(parse(text = cond.6)), eval(parse(text = cond.7)),
                       eval(parse(text = cond.8)), eval(parse(text = cond.9)),
                       eval(parse(text = cond.class1)), eval(parse(text = cond.class2)), eval(parse(text = cond.class3)),
                       eval(parse(text = cond.class4)),
                       eval(parse(text = cond.age1)), eval(parse(text = cond.age2)), eval(parse(text = cond.age3)),
                       eval(parse(text = cond.age4)), eval(parse(text = cond.age5)), eval(parse(text = cond.age6)),
                       eval(parse(text = cond.age7)), eval(parse(text = cond.age8)),
                       eval(parse(text = cond.crime)),
                       Transportation.stations > trans.fil
                  # ranking.trans %in% trans.fil, ranking.bar %in% club.fil,
                  # ranking.theatre %in% theatre.fil, ranking.market %in% market.fil
                ) %>%
                select(zipcode))[,1]
    return(areas)
  })
  
  output$table2 <- renderDataTable(filter_data[,c(1,2,12,30:35,37,38)] %>% 
                                     filter(
                                       zipcode %in% areas()), 
                                   options = list("sScrollX" = "100%", "bLengthChange" = FALSE))
 
  # Panel 2 Map
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
  
  #############Reset for Panel 2############
  observeEvent(input$button2,{
    # proxy<-leafletProxy("map")
    # proxy %>%
    #   setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
    #   removeMarker(layerId="1") %>%
    # updateTextInput(session, inputId="location", value = "")
    updateSliderInput(session, "check2_ppr",value = 500)
    updateSelectInput(session, "check2_type",selected="")
    updateSelectInput(session, "check2_class",selected = "")
    updateSelectInput(session, "check2_age",selected="")
    updateSelectInput(session, "check2_crime",selected = "")
    updateSelectInput(session, "check2_trans",selected = "")
    updateSelectInput(session, "check2_ct",selected = "")
  })


})




