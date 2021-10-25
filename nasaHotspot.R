#This function takes in NASA fire data in Australia
#convert the latitude and longitude data into state and territory
#aggregate the data and return a data frame 

#param: NASA fire map data csv file (https://firms.modaps.eosdis.nasa.gov/download/)
#return: data frame with aggregated data for modelling


nasafire = function(data){
  
  #Read data and extract high confidence hotspots
  
  #read data
  nasa = read.csv(data)
  
  nasa$confidence = as.factor(nasa$confidence)
  
  #extract high confidence
  nasa.high = nasa[nasa$confidence == "h", ]
  
  nasa.high$acq_date = as.Date(nasa.high$acq_date, format = "%Y-%m-%d")
  
  nasa.high$statesterritory = NA
  
  #Converting latitude and longitude to Australian States and Territory 
  
  #identify Western Australia hotspots
  nasa.high[nasa.high$longitude <= 129, ]$statesterritory = "Western Australia"
  
  #identify Northern Territory hotspots
  nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude > -26 &
               nasa.high$longitude <= 138), ]$statesterritory = "Northern Territory"
  
  #identify South Australia hotspots
  nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude <= -26 &
               nasa.high$longitude <= 141), ]$statesterritory = "South Australia"
  
  #identify Tasmania hotspots
  nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude <= -39.159 
  ), ]$statesterritory = "Tasmania"
  
  #identify Queensland hotspots
  nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude > -28.15 &
               nasa.high$longitude > 138), ]$statesterritory = "Queensland"
  
  #identify New South Wales hotspots
  nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude > -33.97 &
               nasa.high$latitude <= -29.19), ]$statesterritory = "New South Wales"
  
  #identify Victoria hotspots
  nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude <= -37.515
  ), ]$statesterritory = "Victoria"
  
  # nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude <= -34.23 &
  #              nasa.high$longitude <= 142.225), ]$statesterritory = "Victoria"
  # nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude <= -36.133 &
  #              nasa.high$longitude <= 148), ]$statesterritory = "Victoria"
  # nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude > -36.804 & nasa.high$latitude <= -35.9208 &
  #              nasa.high$longitude > 148.224), ]$statesterritory = "New South Wales"
  # nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude > -37.28 & nasa.high$latitude <= -33.97 &
  #              nasa.high$longitude > 149.4), ]$statesterritory = "New South Wales"
  # nasa.high[(is.na(nasa.high$statesterritory) & nasa.high$latitude > -35.8 & nasa.high$latitude <= -33.97 &
  #              nasa.high$longitude > 144.38 & nasa.high$longitude <= 148.75), ]$statesterritory = "New South Wales"
  
  #get NSW council
  nsw.lga = read.csv("NSW LGA.csv")
  colnames(nsw.lga) = c("council")
  
  #source reverse geocode function
  source("reverseGeo.R")
  
  #get address for remaining locations
  for (obs in 1:nrow(nasa.high)){
    #check if value is na
    #if value is na, state/ territory has not been identified
    #use reverse geocode function to get location's state/territory
    if(is.na(nasa.high$statesterritory[obs])){ 
      
      #reverse geocode function
      address = reversegeo(nasa.high$latitude[obs], nasa.high$longitude[obs])
      
      #limitations of API used 
      #if a location in NSW was entered, API does not return state name
      #hence use county name to identify location in NSW
      if (!is.null(address$county) & is.null(address$state)){
        if(address$county %in% nsw.lga$council){
          nasa.high$statesterritory[obs] = "New South Wales"
        }
      } 
      else{
        nasa.high$statesterritory[obs] = address$state
      }
      
    }
    
  }
  
  #create new column for region of the state
  #each state was splited into few regions for modelling purposes
  #region was determined with correlation of the fire hotspots
  nasa.high$region = NA
  
  #identifying regions in Victoria
  nasa.high[nasa.high$statesterritory == "Victoria" & nasa.high$longitude > 145.5, ]$region = "East"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "Victoria" & nasa.high$latitude > -37.15, ]$region = "North West"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "Victoria", ]$region = "South West"
  
  #identifying regions in South Australia
  nasa.high[nasa.high$statesterritory == "South Australia" & nasa.high$latitude > -31.5, ]$region = "North"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "South Australia", ]$region = "South"
  
  #identifying regions in New South Wales
  nasa.high[nasa.high$statesterritory == "New South Wales" & nasa.high$latitude > -33.15 & nasa.high$longitude < 147.2,]$region = "North West"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "New South Wales" & nasa.high$latitude > -32.3, ]$region = "North East"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "New South Wales", ]$region = "South"
  
  #identifying regions in Queensland
  nasa.high[nasa.high$statesterritory == "Queensland" & nasa.high$latitude > -22.2, ]$region = "North"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "Queensland", ]$region = "South"
  
  #identifying regions in Northern Territory
  nasa.high[nasa.high$statesterritory == "Northern Territory" & nasa.high$latitude > -19.45, ]$region = "North"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "Northern Territory", ]$region = "South"
  
  #identifying regions in Tasmania
  nasa.high[nasa.high$statesterritory == "Tasmania" & nasa.high$latitude > -42.25, ]$region = "North"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "Tasmania", ]$region = "South"
  
  #identifying regions in Western Australia
  nasa.high[nasa.high$statesterritory == "Western Australia" & nasa.high$latitude < -27.9 & nasa.high$longitude < 124.3, ]$region = "South West"
  nasa.high[is.na(nasa.high$region) & nasa.high$statesterritory == "Western Australia", ]$region = "North and Mining Area"
  
  ##identifying regions in ACT
  nasa.high[is.na(nasa.high$region),]$region = "Central"
  
  
  
  #create a new data frame that shows the amount of bushfire occurance
  #for each date, state and region of the respective state
  occurance = aggregate(list(nasa.high$frp), list(nasa.high$acq_date, nasa.high$statesterritory, nasa.high$region), length)
  colnames(occurance) = c("Date", "StateTerritory", "Region", "Amount")
  
  return(occurance)
}


#---- Export csv ----
occurance = nasafire("fire_archive_V1_144036.csv")
write.csv(occurance, file = "occurance.csv")




          