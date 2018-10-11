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
library(tidyr)
library(ggplot2)
library(ggmap)
library(sp)
library(maptools)
library(dmm)
library(gridExtra)
restaurant<-read.csv("../data/restaurant_new2.csv")
income<-read.csv("../data/income1.csv")
yelp.rate<-read.csv("../data/zip.rate1.csv")
age_sex<-read.csv("../data/age_sex - new2.csv", header = T)
pie_type<-read.csv("../data/zip.prop2.csv")
load("../data/subdat.RData")

filter_data <- read.csv("../output/filter_data_used.csv", as.is = T)
table_display <- read.csv("../output/table_display.csv", as.is = T, check.names = FALSE)

shinyServer(function(input, output,session) {
  
  #Esri.WorldTopoMap
  #########main map######
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
                highlightOptions = highlightOptions(color='#ff1900', opacity = 0.3, weight = 2, fillOpacity = 0.4,
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
    Total_population<-paste("Total population (in k's): ",age_sex[which(age_sex[,1]==as.character(click[1])),]$Total_Population,sep="")
    output$Total_population<-renderText({Total_population})
    #Female percentage
    Female.per<-paste("Female Ratio: ", round(age_sex[which(age_sex[,1]==as.character(click[1])),]$Female.per,2),sep="")
    output$Female.per<-renderText({Female.per})
    #Yelp rate
    yelp.rate<-paste("Average Yelp Rating: ", yelp.rate[which(yelp.rate[,1]==as.character(click[1])),]$Yelp_star,sep="")
    output$yelp.rate<-renderText({yelp.rate})
    #Age group percentage and restaurant type
    row_age<-age_sex[which(age_sex[,1]==as.character(click[1])),]
    df_age<-data.frame(matrix(NA, nrow = 8, ncol = 3))
    output$pie_age_rest <- renderPlot({
      for(i in 1:8){
        df_age[i,1]<-as.numeric(i)
        df_age[i,2]<-as.character(colnames(row_age)[i+3])
        df_age[i,3]<-as.numeric(row_age[1,i+3])
      }
      colnames(df_age)[1]<-""
      df_age[,2]<-c("<5","5-14","15-24","25-34","35-44","45-54","55-64","65+")
      ap<- ggplot(df_age, aes(x="", y=X3, fill=X2))+ geom_bar(width = 1, stat = "identity")+theme(plot.margin=grid::unit(c(0,4,0,0), "mm")) + theme_void()
      pie <- ap + coord_polar("y", start=0)+xlab("")+ylab("")+theme(legend.position="left")+ggtitle("Age Distribution")+theme(plot.title = element_text(hjust = 0.5, face = "bold"))+labs(fill = "Age Groups")

      pie_type<-pie_type[which(pie_type[,1]==as.character(click[1])),]
      pie2<-ggplot(data=pie_type, aes(x=Type, y=Percentage)) +geom_bar(stat="identity", fill = "darkblue")+theme(plot.margin = unit(c(0.1,0.5,0.1,0.1), "cm"))+theme_classic()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ theme(legend.position="none")+xlab("Restaurant Type")+ylab("% of all restaurants")+ggtitle("Retaurant Type Distribution")+theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      pie_age_rest<-grid.arrange(pie2, pie, nrow = 2)
      pie_age_rest},height = 450, res = 72)
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
  
  ### Panel 2: Owner's Choice
  
  output$map3 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('OpenStreetMap.HOT') %>%
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
    
    cond.class1 <- if(is.null(input$check2_class)){"is.na(class_1) == FALSE"
    } else if(!("Upper" %in% input$check2_class)){"class_1 == 0"} else {"is.na(class_1) == FALSE"}
    
    cond.class2 <- if(is.null(input$check2_class)){"is.na(class_2) == FALSE"
    }else if(!("Middle" %in% input$check2_class)){"class_2 == 0"} else {"is.na(class_2) == FALSE"}
    
    cond.class3 <- if(is.null(input$check2_class)){"is.na(class_3) == FALSE"
    } else if(!("Working" %in% input$check2_class)){"class_3 == 0"} else {"is.na(class_3) == FALSE"}
    
    cond.class4 <- if(is.null(input$check2_class)){"is.na(class_4) == FALSE"
    } else if(!("Lower" %in% input$check2_class)){"class_4 == 0"} else {"is.na(class_4) == FALSE"}
    
    cond.age1 <- if(is.null(input$check2_age)){"below5 <= 43 | is.na(below5) == TRUE"
    } else if("<5" %in% input$check2_age){"below5 <= 30"
    } else {"below5 <= 43 | is.na(below5) == TRUE"}
    
    cond.age2 <- if(is.null(input$check2_age)){"X5_14 <= 43 | is.na(X5_14) == TRUE"
    } else if("5-14" %in% input$check2_age) {"X5_14 <= 30"
    } else {"X5_14 <= 43 | is.na(X5_14) == TRUE"}
    
    cond.age3 <-  if(is.null(input$check2_age)){"X15_24 <= 43 | is.na(X15_24) == TRUE"
    } else if("15-24" %in% input$check2_age) {"X15_24 <= 30"
    } else {"X15_24 <= 43 | is.na(X15_24) == TRUE"}
    
    cond.age4 <- if(is.null(input$check2_age)){"X25_34 <= 43 | is.na(X25_34) == TRUE"
    } else if("25-34" %in% input$check2_age) {"X25_34 <= 30"
    } else {"X25_34 <= 43 | is.na(X25_34) == TRUE"}
    
    cond.age5 <- if(is.null(input$check2_age)){"X35_44 <= 43 | is.na(X35_44) == TRUE"
    } else if("35-44" %in% input$check2_age) {"X35_44 <= 30"
    } else {"X35_44 <= 43 | is.na(X35_44) == TRUE"}
    
    cond.age6 <- if(is.null(input$check2_age)){"X45_54 <= 43 | is.na(X45_54) == TRUE"
    } else if("45-54" %in% input$check2_age) {"X45_54 <= 30"
    } else {"X45_54 <= 43 |is.na(X45_54) == TRUE"}
    
    cond.age7 <- if(is.null(input$check2_age)){"X55_64 <= 43 | is.na(X55_64) == TRUE"
    } else if("55-64" %in% input$check2_age) {"X55_64 <= 30"
    } else {"X55_64 <= 43 | is.na(X55_64) == TRUE"}
    
    cond.age8 <- if(is.null(input$check2_age)){"X65above <= 43 | is.na(X65above) == TRUE"
    } else if("65+" %in% input$check2_age) {"X65above <= 30"
    } else {"X65above <= 43 | is.na(X65above) == TRUE"}
    
    cond.crime <- if(input$check2_crime == "Very Safe"){"crime_level == 'Safe'"
    } else if(input$check2_crime == "Safe"){"crime_level == 'Safe' | crime_level == 'Relatively Safe'"
    } else if(input$check2_crime == "A Little Dangerous"){"crime_level == 'Safe' | crime_level == 'Relatively Safe' | crime_level == 'Relatively Dangerous'"
    } else {"is.na(crime_level) == FALSE"}
    
    market.fil <- if(input$check2_market == "Many"){
      40
    } else if(input$check2_market == "A few"){
      21
    } else {
      0
    }
    
    trans.fil <- if(input$check2_trans == "Many"){
      25
    } else if(input$check2_trans == "A few"){
      15
    } else {
      1
    }
    
    theatre.fil <- if(input$check2_ct == "Many"){
      10
    } else if(input$check2_ct == "A few"){
      5
    } else {
      0
    }
    
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
                       Transportation.stations >= trans.fil, markets >= market.fil, (Theatre >= theatre.fil | is.na(Theatre) == TRUE)
                ) %>%
                select(zipcode))[,1]
    return(areas)
  })
  
  col_display <- reactive({
    columns <- names(table_display)[22:30]
    
    if("Others" %in% input$check2_type){
      columns <- c("People/Others", "Others Proportion", columns)
    } 
    if("Seafood" %in% input$check2_type){
      columns <- c("People/Seafood", "Seafood Proportion", columns)
    } 
    if("Mexiacan" %in% input$check2_type){
      columns <- c("People/Mexiacan", "Mexiacan Proportion", columns)
    } 
    if("Italian" %in% input$check2_type){
      columns <- c("People/Italian", "Italian Proportion", columns)
    } 
    if("European" %in% input$check2_type){
      columns <- c("People/European", "European Proportion", columns)
    }
    if("Dessert" %in% input$check2_type){
      columns <- c("People/Dessert", "Dessert Proportion", columns)
    }
    if("Chinese" %in% input$check2_type){
      columns <- c("People/Chinese", "Chinese Proportion", columns)
    } 
    if("Asian" %in% input$check2_type){
      columns <- c("People/Asian", "Asian Proportion", columns)
    } 
    if("Quick Meal" %in% input$check2_type){
      columns <- c("People/QuickMeal", "QuickMeal Proportion", columns)
    } 
    if("American" %in% input$check2_type){
      columns <- c("People/American", "American Proportion", columns)
    } 
    
    columns <- c("Zipcode", columns)
    return(columns)
  })
  
  output$table2 <- renderDataTable(table_display[, names(table_display) %in% col_display()] %>% 
                                     filter(
                                       Zipcode %in% areas()), 
                                   options = list("sScrollX" = "100%", "bLengthChange" = FALSE))
  
  # Panel 2 Map
  observe({
    if(length(areas())!=0){
      leafletProxy("map3")%>%clearGroup(group="new_added")%>%
        addPolygons(data=subset(subdat, subdat$ZIPCODE%in% areas()),
                    weight = 2,
                    color = "#34675C",
                    fillColor = "#cbefb3",
                    fillOpacity=0.7,
                    group="new_added",
                    noClip = TRUE, label = ~ZIPCODE)
    }
    else{
      leafletProxy("map3")%>%clearGroup(group="new_added")
    }
  })
  
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
    updateSelectInput(session, "check2_crime",selected = "Acceptable number of crimes")
    updateSelectInput(session, "check2_market",selected = "")
    updateSelectInput(session, "check2_trans",selected = "")
    updateSelectInput(session, "check2_ct",selected = "")
  })
  
  
})




