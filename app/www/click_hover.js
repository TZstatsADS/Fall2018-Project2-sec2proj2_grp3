// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(event) {
  event.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var lng = $el.data("lng");
  
  
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: lng,
   
  });
});


$(document).on("mouseover", ".go-map", function(event) {
  event.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var lng = $el.data("lng");
  
  
  Shiny.onInputChange("showPop", {
    lat: lat,
    lng: lng,
    remove:0
   
  });
});


$(document).on("mouseleave", ".go-map", function(event) {
  event.preventDefault();
  
  
  
  Shiny.onInputChange("showPop", {
	remove:1
   
  });
});
