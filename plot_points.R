
library(rstudioapi)

template_html <- function(title, body, js_head = NULL, css = NULL, js_end = NULL){
  return(paste("<html>",'<head>','<title>',title,'</title>','</head>','<body>',body,js_end,'</body>','</html>',sep="\n"))
}

google_maps_marker <- function(var_name, lat, lng, label = NULL, meta = NULL){
  if(is.null(label)){
    label_str <- ''
  }else{
    label_str <- paste0(' label: "',label,'",')
  }
  return(paste(
    'var ', var_name, ' = new google.maps.Marker({',
    ' position: {lat: ',lat,', lng: ', lng,'},',
    label_str,
    ' map: map});',sep="\n"
  ))
}

google_maps_api_html <- function(lats,lngs,api_key){
  
  markers <- character(length(lats))
  for(i in seq_along(markers)){
    var_name <- paste0('node',i)
    markers[i] <- google_maps_marker(var_name, lats[i], lngs[i], i-1)
  }
  
  js_script <- paste('<script>',
  'var map;',
  'function initMap() {',
  'map = new google.maps.Map(document.getElementById("map"), {',
  'zoom: 10, scaleControl: true, ',
  'center: {lat:',round(mean(lats, na.rm = T),2),', lng: ',round(mean(lngs, na.rm = T),2),'}',
  '});',
  paste(markers, collapse="\n"),
  '}',
  '</script>',
  '<script async defer
src="https://maps.googleapis.com/maps/api/js?key=',api_key,'&signed_in=true&callback=initMap" >',
  '</script>',
  sep="\n")
  
  return(template_html('google_map',"<div id='map' style='height:500px;width:500px;'></div>",js_end = js_script))
}

plot_points <- function(lats, lngs, api_key, meta = NULL){

  html_string <- google_maps_api_html(lats,lngs,api_key)
  tmpHTML <- paste(tempfile(),'html',sep='.')
  con <- file(description = tmpHTML, 'w')
#   x <- c("var geojson_data = ",x,";")
  writeLines(html_string, con = con)
  close(con)
  viewer(tmpHTML)
}






