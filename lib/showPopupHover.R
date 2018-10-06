showPopupHover <- function(lat, lng,housing) {
  selected_point <- housing[abs(housing$lat-lat)<0.0001 & abs(housing$lng-lng)<0.0001,]
  if(nrow(selected_point)==0){
    return()
  }
  lotSize=NULL
  YearBuilt=NULL
  appliances=NULL
  images=NULL
  zpid=getZillowId(selected_point$addr[1])
  
  if(!is.null(zpid)){
    DeepResults=getDeepSearchResults(zpid)  
    lotSize=DeepResults$lotSize
    YearBuilt=DeepResults$YearBuilt
    appliances=DeepResults$appliances
    images=DeepResults$images
  }
  lotSize=ifelse(is.null(lotSize),"-",lotSize)
  YearBuilt=ifelse(is.null(YearBuilt),"-",YearBuilt)
  appliances=ifelse(is.null(appliances),"-",appliances)
  image=ifelse(is.null(images),"no results found",images$image$url)
  
  content <- as.character(tagList(
    tags$div(id="container",
      tags$strong(HTML(sprintf("%s",
                             selected_point$addr[1]
      ))), tags$br(),
      
        tags$div( id="first",
          sprintf("price: %s", selected_point$price[1]), tags$br(),
          sprintf("bedrooms: %s", selected_point$bedrooms[1]), tags$br(),
          sprintf("bathrooms: %s", selected_point$bathrooms[1]),tags$br(),
          sprintf("lotSize: %s", lotSize),tags$br(),
          sprintf("YearBuilt: %s", YearBuilt),tags$br(),
          sprintf("appliances: %s", appliances),tags$br()
      ),
      tags$div( id="second",
        tags$img(src=image,height="150",width="150")
      )  
    
    )
    
  ))
  leafletProxy("map") %>% 
    addPopups(lng, lat, content,options=popupOptions(autoPan=TRUE,closeButton = FALSE))
    
}



