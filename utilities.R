
library(httr)
library(stringr)
library(jsonlite)

#google_maps_distance functions calls Google Maps Distance Matrix API
# origin and destination are both vectors of characters
# api_key is character with your API key
# resp_type is default to json, can also be xml
# 
# function will break if resp_type is not xml or json
# function will create url query with correct syntax for origin and destination
#    this relies on stringr library function
# http request preformed by httr library function
#
# will break if request returns in error
# will break if google maps denies request
google_maps_distance <- function(origin,destination,api_key,resp_type = 'json'){
  
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
                     'key' = api_key
  )
  
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