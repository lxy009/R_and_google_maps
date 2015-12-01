# R_and_google_maps
Functions for use in R to work with Google Maps API
#utilities.R
contains 2 functions.
google_maps_distance() submits http request to distance matrix API.
parse_google_maps_distance_matrix() takes output from google_maps_distance() and creates 2 R matrices.
One matrix for distance and the other for duration.
#test.R
contain test scripts for the 2 functions.
#test01.RData
contains a sample output from google_maps_distance().
used for testing purposes
