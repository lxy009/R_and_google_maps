
library(httr)
library(stringr)
library(jsonlite)

#google_maps_distance functions calls Google Maps Distance Matrix API
# origin and destination are both vectors of characters
# api_key is character with your API key
# resp_type is default to json, can also be xml
# mode defaults to driving and should correspond to a valid input for google maps: driving, walking, bicycling, transit
# transit_mode is only utilized if mode = 'transit'. possible values: bus, subway, train, tram, rail. default is bus
# depature time must be in the future. it is in UNIX time, seconds since midnight 1970-01-01 UTC, OR the string value 'now'
# traffic model is only utilized if mode = 'driving' and depature time is NOT null
#
# function will break if resp_type is not xml or json
# function will create url query with correct syntax for origin and destination
#    this relies on stringr library function
# http request preformed by httr library function
#
# will break if request returns in error
# will break if google maps denies request
google_maps_distance <- function(origin,destination,api_key,resp_type = 'json',mode = 'driving',transit_mode = 'bus',
                                 departure_time = NULL,traffic_model = 'best_guess'){
  
  #check to make sure response type is XML or JSON
  if(! resp_type %in% c('json','xml')){
    stop('resp_type must either be json or xml')
  }
  
  #need to replace spaces with + character
  origin <- str_replace_all(origin, " ", "+")
  destination <- str_replace_all(destination, " ", "+")
  #separate vector elements with | character
  origin <- str_c(origin, collapse = "|")
  destination <- str_c(destination, collapse = "|")
  
  query_list <- list('origins' = origin,
                     'destinations' = destination,
                     'mode' = mode,
                     'key' = api_key
  )
  if(mode == 'transit'){
    query_list <- append(query_list, list('transit_mode' = transit_mode))
  }else{
    if(mode == 'driving' & !is.null(depature_time)){
      #make sure time is in the future or now
      if(is.character(depature_time)){
        if(depature_time == 'now'){
          query_list <- append(query_list, list('departure_time' = departure_time, 'traffic_model' = traffic_model))
        }else{
          stop(paste("Depature time can only except string value of 'now', not",departure_time))
        }
      }else{
        if(depature_time > as.numeric(Sys.time())){
          query_list <- append(query_list, list('departure_time' = departure_time, 'traffic_model' = traffic_model))  
        }else{
          stop(paste('Depature time',as.POSIXct(departure_time),'is not in the future'))
        }
      } 
    }
  }
  
  base_url <- 'https://maps.googleapis.com/maps/api/distancematrix/'
  resp <- GET(paste0(base_url,resp_type), query = query_list)
  
  #check http status
  if(status_code(resp) != 200){
    stop(status_code(resp))  
  }
  #check if content status
  if(content(resp)$status == 'REQUEST_DENIED'){
    stop(content(resp)$error_message)
  }
  
  return(resp)
}

#takes output from google_maps_distance function and parses json string
#  uses jsonlite pacakge 
#  returns 2 matrices, one for duration and one for distance
#  rows and columns named based on destination
#  attribute 'unit' is attached to each matrix to denote unit of values
#  THIS DOES NOT EXTRACT DURATION_IN_TRAFFIC AT THIS POINT
parse_google_maps_distance_matrix <- function(x){
  x <- fromJSON(txt = x)
  
  dests <- x$destination_addresses
  origs <- x$origin_addresses
  
  #make 2 matrices, 1 for distance, 1 for duration
  dist_matrix <- matrix(NA, length(origs), length(dests))
  colnames(dist_matrix) <- dests
  rownames(dist_matrix) <- origs
  dura_matrix <- dist_matrix
  attr(dist_matrix,'unit') <- 'km'
  attr(dura_matrix,'unit') <- 'sec'
  
  #extract content
  for(i in 1:length(origs)){
    df_temp <- x$rows[i,][[1]]
    dist_matrix[i,] <- df_temp$distance$value
    dura_matrix[i,] <- df_temp$duration$value
  }
  
  return(list('distance_matrix' = dist_matrix,'duration_matrix' = dura_matrix))
}