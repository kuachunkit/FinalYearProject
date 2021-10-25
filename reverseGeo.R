#This function reverse the geocode of a location using the Open Street Map API
#The function has two parameters which are the location's latitude and longitude
# and returns the address (state/ territory) of the respective location

#install packages
#"httr" package is used to return API call
#"jsonlite" is used to handel json data
package = c("httr", "jsonlite")
not_installed <- package[!(package %in% installed.packages()[ , "Package"])]
if(length(not_installed)) install.packages(not_installed)  

library(httr)
library(jsonlite)

#reverse geocode function
reversegeo = function(latitude, longitude){
  
  #query using the API
  res = GET("https://nominatim.openstreetmap.org/reverse?format=json"
            ,query = list(lat = latitude,
                          lon = longitude, zoom = 8))
  
  #convert the query from JSON
  api.data = fromJSON(rawToChar(res$content))
  
  #extract the address
  address = api.data$address
  
  return(address)
}









