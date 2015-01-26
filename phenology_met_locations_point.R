
data_type_point_slope = "point" # POINT OR SLOPE!!!!!!!!

# Okay, this is pretty bad coding, but if there is a station with clearly bad
# data upon further observation, put its ID in this character vector:
bad_station_ids <- c("994400")

# Source
source("Determine_Percent_Data_GHCN.R")
source("Determine_Percent_Data_GSOD.R")

# Load some required packages:
loaded <- require("plotrix")
if(!loaded){
  print("trying to install plotrix")
  install.packages("plotrix")
  loaded <- require("plotrix")
  if(loaded){
    print("plotrix installed and loaded")
    library(plotrix)
  } 
  else {
    stop("Could not install plotrix. You need to figure out how to install that manually before this function will work!")
  }    
}

loaded <- require("Imap")
if(!loaded){
  print("trying to install Imap")
  install.packages("Imap")
  loaded <- require("Imap")
  if(loaded){
    print("Imap installed and loaded")
    library(Imap)
  } 
  else {
    stop("Could not install Imap. You need to figure out how to install that manually before this function will work!")
  }    
}

if (data_type_point_slope == "point"){
  # Make some folders for the data
  if(!file.exists("Met_Data_Point/")){
    dir.create("Met_Data_Point/")
  }
  # Make some folders for the data
  if(!file.exists("Formatted_Met_Data_Point/")){
    dir.create("Formatted_Met_Data_Point/")
  }
}else{
  # Make some folders for the data
  if(!file.exists("Met_Data_Slope/")){
    dir.create("Met_Data_Slope/")
  }
  # Make some folders for the data
  if(!file.exists("Formatted_Met_Data_Slope/")){
    dir.create("Formatted_Met_Data_Slope/")
  }
}



## load point data file
if(data_type_point_slope == "point"){
  dat <- read.csv("data_file_150114_point.csv")
}else{
  dat <- read.csv("data_file_slope.csv")
}

# Throw out all of the data that doesn't have at least ONE of {LeafFall50,
# LeafFall80, LeafFall100,LAI_zero}:
#has_LF50 <- !is.na(dat$LeafFall50)
#has_LF80 <- !is.na(dat$LeafFall80)
#has_LF100 <- !is.na(dat$LeafFall100)
#has_LAI <- !is.na(dat$LAI_zero)

#has_some_useful_phen_data <- has_LF50 | has_LF80 | has_LF100 | has_LAI
#dat <- dat[has_some_useful_phen_data,]

names <- colnames(dat)
lat.unique <- unique(x=dat$Latitude)
lon.unique <- unique(x=dat$Longitude)

location <- data.frame(lat = dat$Latitude,lon = dat$Longitude)

loc.unique <- unique(location)

loc.unique <- na.omit(loc.unique)
loc.index <- as.integer(row.names(loc.unique))
unique_phen_sites <- dat[loc.index,]                ## unique sites

# Making a column to put chosen met station IDs in...
unique_phen_sites$Met_Station_ID = as.character(NA)

# Making a column to put chosen met station distance from phen site in...
unique_phen_sites$Distance_km = NA

# The data in unique_phen_sites is all factor/categorical data. Convert at least
# lat/lon to numeric:

# This seems to only be necessary on some computers... not sure what it depends on...
#unique_phen_sites <- transform(unique_phen_sites, 
#                               Latitude = as.numeric(levels(Latitude))[Latitude],
#                               Longitude = as.numeric(levels(Longitude))[Longitude])

stations_gsod <- read.csv("isd-history.csv",na.strings = c("-99999","-999999"))
stations_gsod <- na.omit(stations_gsod)
stations_gsod$LAT<- stations_gsod$LAT/1000
stations_gsod$LON<- stations_gsod$LON/1000

if(!file.exists("stations_ghcn_trimmed.csv")) {
  source("get_ghcn_data.R")  
}
stations_ghcn_trimmed <- read.csv("stations_ghcn_trimmed.csv")

# Join the two datasets:
# gsod_ghcn_data <- data.frame(rep("gsod",nrow(stations_gsod)),
#                                       1:nrow(stations_gsod),
#                                       as.numeric(stations_gsod$LAT), 
#                                       as.numeric(stations_gsod$LON),
#                                       as.numeric(substr(stations_gsod$BEGIN,1,4)),
#                                       as.numeric(substr(stations_gsod$END,1,4)),
#                                       rep("NA",nrow(stations_gsod)))

gsod_ghcn_data <- as.data.frame(cbind(rep("gsod",nrow(stations_gsod)),
                                     1:nrow(stations_gsod),
                                     as.numeric(stations_gsod$LAT), 
                                     as.numeric(stations_gsod$LON),
                                     as.numeric(substr(stations_gsod$BEGIN,1,4)),
                                     as.numeric(substr(stations_gsod$END,1,4)),
                                     rep("NA",nrow(stations_gsod))))
colnames(gsod_ghcn_data) <- c("dataset","orig.row.num","LAT","LON","BEGIN.YR","END.YR","ELEMENT")

# Add ghcn data to gsod_ghcn_data
ghcn_data_to_bind = as.data.frame(cbind(rep("ghcn",nrow(stations_ghcn_trimmed)),
                                        1:nrow(stations_ghcn_trimmed),
                                        as.numeric(stations_ghcn_trimmed$LATITUDE), 
                                        as.numeric(stations_ghcn_trimmed$LONGITUDE),
                                        as.numeric(stations_ghcn_trimmed$FIRSTYEAR),
                                        as.numeric(stations_ghcn_trimmed$LASTYEAR),
                                        as.character(stations_ghcn_trimmed$ELEMENT)))
colnames(ghcn_data_to_bind) <- c("dataset","orig.row.num","LAT","LON","BEGIN.YR","END.YR","ELEMENT")

# Concatenating GSOD and GHCN data frames
gsod_ghcn_data <- rbind(gsod_ghcn_data,ghcn_data_to_bind)

# Okay, for some reason, none of these are the right classes of data, so let's fix that:
gsod_ghcn_data <- transform(gsod_ghcn_data, orig.row.num = as.integer(levels(orig.row.num))[orig.row.num],
                            LAT = as.numeric(levels(LAT))[LAT],
                            LON = as.numeric(levels(LON))[LON],
                            BEGIN.YR = as.integer(levels(BEGIN.YR))[BEGIN.YR],
                            END.YR = as.integer(levels(END.YR))[END.YR])

# Er... this is weird... type in summary(gsod_ghcn_data) and you'll notice that
# the minimum longitude is -802.33, and the minumum BEGIN.YR is 1763... suprising. Anyway...

# Loop over stations -- everthing, man , 
 for(ST in 1:nrow(unique_phen_sites)){ # for 1:number of phenology sites
  print(sprintf("Processing pheno site %i of %i...",ST,nrow(unique_phen_sites)))
  
  # latitude and longitude of the phenology site:
  phen_lat <- unique_phen_sites$Latitude[ST]
  phen_lon <- unique_phen_sites$Longitude[ST]
  
  
  nearby_data <- subset(gsod_ghcn_data, (abs(gsod_ghcn_data$LAT - phen_lat) < 2) 
                        & (abs(gsod_ghcn_data$LON - phen_lon) < 2))
  nearby_data$ghcn_id = NA
  nearby_data$ghcn_id[(nearby_data$dataset == "ghcn")] = as.character(stations_ghcn_trimmed$ID[nearby_data$orig.row.num[(nearby_data$dataset == "ghcn")]])
  
  distance <- rep(NA,nrow(nearby_data))
  
  # For each potential nearest met station...
  for(MET_ST in 1:nrow(nearby_data)){
    
    lat_MET_ST <- nearby_data$LAT[MET_ST]
    lon_MET_ST <- nearby_data$LON[MET_ST]
    
    # ...calculate the distances to the nearby met stations:
    distance[MET_ST] <- gdist(lon.1=lon_MET_ST,lat.1=lat_MET_ST,
                              lon.2=phen_lon,lat.2=phen_lat, units = "km")
  }
  # Let's put them in order of increasing distance:
  nearby_data <- cbind(nearby_data, distance)  
  
  # Remove NA Distance
  na_dist = is.na(nearby_data$distance)
  nearby_data = subset(nearby_data,!na_dist)
  
  nearby_data <- nearby_data[with(nearby_data,order(distance)),] # Closest is in row 1
  
  # Find the met stations that have overlapping POR with the desired PHEN POR. 
  phen_begin <- as.numeric(unique_phen_sites$Year_Sampled_Start[ST])
  phen_end <- as.numeric(unique_phen_sites$Year_Sampled_End[ST])[unique_phen_sites$Year_Sampled_End[ST]]
  
  if (is.na(phen_end)){
    phen_end = phen_begin
  }
  
  nearby_data = subset(nearby_data, nearby_data$BEGIN.YR <= phen_begin & 
                         (nearby_data$END.YR) >= phen_end)
  
  # Now find the closest one that has at least XX% data.
  # We'll use a little while loop. Keep looping until either a station is found or we're out of stations.
  required_data_completeness <- 0.7 # 70%
  no_nearby_stns <- FALSE
  BREAK <- FALSE
  
  # If there are no stations in nearby_data, then skip the loop entirely
  if(nrow(nearby_data) == 0) {
    BREAK <- TRUE
    no_nearby_stns <- TRUE
    # Making a column to put chosen met station IDs in...
    unique_phen_sites$Met_Station_ID[ST] = "NoData"
    unique_phen_sites$Distance_km[ST] = NA
  }
  
  count = 1
  
  
  while(!BREAK){
    
    print(sprintf("Checking met station %i for pheno site %i...",count,ST))
    count = count+1
    
    # Figure out whether gsod or ghcn:
    dataset <- as.character(nearby_data$dataset[1])
    distance_met_to_pheno <- nearby_data$distance[1]
    
    # Deal with gsod and ghcn data seperately
    if(dataset=="gsod") {
      # Get the WBAN and USAF ids for the closest met station:
      WBAN_id <- stations_gsod$WBAN[nearby_data$orig.row.num[1]]
      USAF_id <- stations_gsod$USAF[nearby_data$orig.row.num[1]]
      
      fraction_complete <- Determine_Percent_Data_GSOD(USAF_id,WBAN_id,
                                                       phen_begin,
                                                       phen_end,
                                                       data_type_point_slope,
                                                       required_data_completeness)
      
      # If there is sufficient data, then it's good!
      good_data <- (fraction_complete >= required_data_completeness)
      
      # But if the station is on our list of bad stations, then it's not good:
      if (any((WBAN_id == bad_station_ids) | (USAF_id == bad_station_ids))) {
        good_data = FALSE
        print("Station is on our list of stations with bad data quality.")
      }
      
      if (good_data){
        
        BREAK = TRUE
        unique_phen_sites$Met_Station_ID[ST] = USAF_id
        unique_phen_sites$Distance_km[ST] = distance_met_to_pheno
        
      }else{
        nearby_data <- nearby_data[-1,]
        print("Met station does not meet requirements. Skipping.")
      }
    }else{ # GHCN
      st_id <- nearby_data$ghcn_id[1]
      
      ghcn_current_st <- subset(stations_ghcn_trimmed,stations_ghcn_trimmed$ID==st_id)
      
      check_elements_mat <- matrix(data=0,nrow=nrow(ghcn_current_st),ncol=3)
      
      check_elements_mat[ghcn_current_st$ELEMENT=="PRCP",1] = 2
      check_elements_mat[ghcn_current_st$ELEMENT=="TMAX",1] = 3
      check_elements_mat[ghcn_current_st$ELEMENT=="TMIN",1] = 5
      
      check_elements_mat[ghcn_current_st$FIRSTYEAR <= phen_begin,2] = 1
      check_elements_mat[ghcn_current_st$LASTYEAR >= phen_end,3] = 1
      # Multiply the columns together:
      prod_vec <- apply(check_elements_mat,1,prod)
      # Remove the zeros (ie the records that don't overlap with the pheno site time span):
      prod_vec[prod_vec==0] <- NA
      product <- prod(prod_vec,na.rm=TRUE)      
      
      # product should be a multiple of 30 if there is data for the appropriate time span for all variables
      if(product%%30 == 0) {
        
        # Download the data and check its completeness
        fraction_complete <- Determine_Percent_Data_GHCN(st_id,
                                                         phen_begin,
                                                         phen_end,
                                                         data_type_point_slope,
                                                         required_data_completeness)
        
        
        # If there is sufficient data, then it's good!
        good_data <- (fraction_complete >= required_data_completeness)
        
        # But if the station is on our list of bad stations, then it's not good:
        if (any(st_id == bad_station_ids)) {
          good_data = FALSE
          print("Station is on our list of stations with bad data quality.")
        }
        
        if (good_data){ # Station is good, exit the loop
          BREAK = TRUE
          unique_phen_sites$Met_Station_ID[ST] = st_id    
          unique_phen_sites$Distance_km[ST] = distance_met_to_pheno
          
        }else{
          # Otherwise, delete all rows with that ID (same station)
          nearby_data = subset(nearby_data,ghcn_id != st_id)
          print("Met station does not meet requirements. Skipping.")
        }
        
        
        
      }else{ # We don't have all three elements
        nearby_data = subset(nearby_data,ghcn_id != st_id)
      }    
      
    }
    
    # If there are no stations left in nearby_data, then exit the loop
    if(nrow(nearby_data) == 0) {
      BREAK <- TRUE
      no_nearby_stns <- TRUE
      unique_phen_sites$Met_Station_ID[ST] = "NoData"
      unique_phen_sites$Distance_km[ST] = NA
    }
    
  }
  
  if (data_type_point_slope == "point"){
    write.csv(unique_phen_sites,file="PointMetData.csv")
  }else{
    write.csv(unique_phen_sites,file="SlopeMetData.csv")
  }
  
  
}

