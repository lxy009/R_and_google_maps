
#test parse_google_maps_distance_matrix
load('test01.RData')
x <- parse_google_maps_distance_matrix(to_save)
test_res <- c(attr(x$distance_matrix,'unit') == 'km', 
t2 <-attr(x$duration_matrix,'unit') == 'sec', 
all(as.matrix(x$duration_matrix) ==  matrix(c(53827,44740,11629,17268),2,2)),
all(as.matrix(x$distance_matrix) ==  matrix(c(1528711,1299998,114493,296820),2,2))
)
paste(paste("Test",seq(4)),c("distance matrix unit is km",
                      "duration matrix unit is sec",
                      "duration matrix was turned into R matrix correctly",
                      "distance matrix was turned into R matrix correctly"),test_res,sep=" - ")

#test google_maps_distance
your_api_key = 'AIzaSyCRWTjF0XcKykk_eLgbLG5J0MvDSOb90Y0'
#test 1 - correct http request
r <- google_maps_distance(origin = c('Vancouver BC','Seattle'), 
                          destination = c('San Francisco','Victoria BC'),
                          api_key = your_api_key)
substr(r$url,1,135) == "https://maps.googleapis.com/maps/api/distancematrix/json?origins=Vancouver%2BBC%7CSeattle&destinations=San%2BFrancisco%7CVictoria%2BBC&"
#test 2 - dumb key should through message that API key is invalid
try(google_maps_distance(origin = 'Dallas TX', destination = 'Austin, TX', api_key = 'duh'))
trimws(strsplit(geterrmessage(),"\n")[[1]][2]) == 'The provided API key is invalid.'
#test 3 - test traffic model
r <- google_maps_distance(origin = 'MCO Orlando Airport', 
                          destination = 'North, 4600 World Dr, Orlando, FL 32830',
                          departure_time = round(as.numeric(as.POSIXct(Sys.time())) + 24*60*60,0),
                          api_key = your_api_key)
ncol(fromJSON(txt = content(r,'text'))$rows[1,][[1]]) == 4


