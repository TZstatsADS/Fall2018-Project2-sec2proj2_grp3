library(XML)
getZillowId=function(addr,zwsid="X1-ZWz1904xhek16z_6zfs6"){
  # first search zillow id:
  addr=gsub("#.*","",addr)
  addr=trimws(addr, which = c("right"))
  addr=gsub("\\s","+",addr)
  base_url="http://www.zillow.com/webservice/GetSearchResults.htm?"
  zwsid_url=paste("zws-id=",zwsid,"&",sep="")
  addr_url=paste("address=",addr,"&",sep="")
  city_url="citystatezip=New+York%2C+NY"
  
  url=paste(base_url,zwsid_url,addr_url,city_url,sep="")
  data=xmlToList(xmlParse(url))
  zpid=data$response$results$result$zpid
  
  return(zpid) # if there is no result, return null
  
  
}
getDeepSearchResults=function(zpid,zwsid="X1-ZWz1904xhek16z_6zfs6"){
  base_url="http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?"
  zwsid_url=paste("zws-id=",zwsid,"&",sep="")
  zpid_url=paste("zpid=",zpid,sep="")
  url=paste(base_url,zwsid_url,zpid_url,sep="")
  data=xmlToList(xmlParse(url))
  response=data$response
  images=response$images
  details=response$editedFacts
  lotSize=details$lotSizeSqFt
  YearBuilt=details$yearBuilt
  appliances=details$appliances
  return(list(lotSize=lotSize,YearBuilt=YearBuilt,appliances=appliances,images=images))
}

