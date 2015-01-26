
# First step:  we need to put the met data *back* in with the point data in "data_file_150114.csv":
AG_data <- read.csv("data_file_150114_point.csv")

# Add in some new columns:
AG_data[c("Precip_total_Aug","Precip_total_Sep","Precip_total_Oct",  
          "Tmin_mean_Aug","Tmin_mean_Sep","Tmin_mean_Oct",
          "Tmin_median_Aug","Tmin_median_Sep","Tmin_median_Oct",
          "Tmax_mean_Aug","Tmax_mean_Sep","Tmax_mean_Oct",
          "Tmax_median_Aug","Tmax_median_Sep","Tmax_median_Oct",
          "Tmean_Aug","Tmean_Sep","Tmean_Oct",
          "CDD_from_20C_Aug","CDD_from_20C_Sep","CDD_from_20C_Oct",
          "CDD_from_25C_Aug","CDD_from_25C_Sep","CDD_from_25C_Oct",	
          "CDD_from_20C_ASO","CDD_from_25C_ASO",
          "photoperiod_mean_Aug","photoperiod_mean_Sep","photoperiod_mean_Oct")] <- NA


#####################################
# Aggregate the met data!

# Load some required packages:
loaded <- require("geosphere")
if(!loaded){
  print("trying to install package geosphere")
  install.packages("geosphere")
  loaded <- require("geosphere")
  if(loaded){
    print("geosphere installed and loaded")
    library(geosphere)
  } 
  else {
    stop("Could not install geosphere. You need to figure out how to install that manually before this function will work!")
  }    
}


# LOAD MASTER DATA FILE
master_data <- AG_data

master_data$Site_ID = NA
master_data$Met_Station_ID = NA

# LOAD UNIQUE DATA FILE (POINT)
unique_data <- read.csv("PointMetData.csv")
unique_data$Site_ID = NA

# Add site ids and met stn ids:
for (ROW in 1:nrow(unique_data)){
  
  # Find the row(s) in master data that corresponds to row in unique
  unique_lat = as.numeric(unique_data$Latitude[ROW])
  unique_lon = as.numeric(unique_data$Longitude[ROW])
  
  # Match the lat/lon values (might be some rounding errors, so give a tolerance)
  matching_sites = ( abs(master_data$Latitude-unique_lat) < 0.01 & 
                       abs(master_data$Longitude-unique_lon) < 0.01 )
  
  # Assign site ID, 1000 + ROW 
  master_data$Site_ID[matching_sites] = ROW + 1000
  unique_data$Site_ID[ROW] = ROW + 1000
  
  # Put Met Station ID in master data frame
  master_data$Met_Station_ID[matching_sites] = as.character(unique_data$Met_Station_ID[ROW])
  
  # Put distance from met station to site:
  master_data$Met_Station_Distance_km[matching_sites] = unique_data$Distance_km[ROW]
  
}


# Now, we'll get the aggregated (Aug, Sep, Oct) met data for each.
# There are no southern hemispehere phenology sites
for(ROW in 1:nrow(master_data)) {
  
  # Find met station ID
  met_stat_id = master_data$Met_Station_ID[ROW]  
  
  has_met_lat_lon <- (met_stat_id != "NoData") & !is.na(met_stat_id)
  if ( has_met_lat_lon ) {
    # We have a met station within 2 degrees, and so can input data:
    
    # Find the year
    start_year_phen =  as.numeric(master_data$Year_Sampled_Start[ROW])
    end_year_phen = as.numeric(as.character(master_data$Year_Sampled_End[ROW]))
    
    if (is.na(end_year_phen)){
      end_year_phen = start_year_phen
    }
    
    # load met data
    met_filename = paste('Formatted_Met_Data_Point/',met_stat_id,'.csv',sep='')
    met_data = read.csv(met_filename)
    
    # Make a quick day-of-year vector to go along with the met data:
    DOY <- as.numeric(strftime(met_data$Date, format = "%j"))
    
    #### # And this is the day of leaf-fall by whichever metric:
    #### pheno_day <- response_var_data$pheno_day[ROW]
    
    # Fill in the aggregated met data!
    
    ############ AUGUST ############ 
    # We can easily fill in values for August
    mask_Aug <- (DOY <= 243) & (DOY >= 213)
    master_data$Precip_total_Aug[ROW] <- 31*mean(met_data$ppt_mm[mask_Aug],na.rm=TRUE)
    master_data$Tmin_mean_Aug[ROW] <- mean(met_data$tmin_C[mask_Aug], na.rm=TRUE)
    master_data$Tmin_median_Aug[ROW] <- median(met_data$tmin_C[mask_Aug], na.rm=TRUE)
    master_data$Tmax_mean_Aug[ROW] <- mean(met_data$tmax_C[mask_Aug], na.rm=TRUE)
    master_data$Tmax_median_Aug[ROW] <- median(met_data$tmax_C[mask_Aug], na.rm=TRUE)
    
    # We need a mean daily temperature series for Growing/Chilling Degree Days:
    # Should the growing degree days start at the begining of the growing series 
    mean_temp_Aug <- 0.5*(met_data$tmin_C[mask_Aug]+met_data$tmax_C[mask_Aug])
    master_data$Tmean_Aug[ROW] <- mean(mean_temp_Aug, na.rm=TRUE)  
    
    master_data$CDD_from_20C_Aug[ROW] <- 31*mean(20-mean_temp_Aug[mean_temp_Aug<20], na.rm=TRUE)
    master_data$CDD_from_25C_Aug[ROW] <- 31*mean(25-mean_temp_Aug[mean_temp_Aug<25], na.rm=TRUE)
    
    master_data$photoperiod_mean_Aug[ROW] <- mean(daylength(lat=master_data$Latitude[ROW],
                                                                  doy=as.Date(met_data$Date[mask_Aug])))
    
    ############ SEPTEMBER ############ 
    mask_Sep <- (DOY <= 273) & (DOY >= 244)
    master_data$Precip_total_Sep[ROW] <- 30*mean(met_data$ppt_mm[mask_Sep],na.rm=TRUE)
    master_data$Tmin_mean_Sep[ROW] <- mean(met_data$tmin_C[mask_Sep], na.rm=TRUE)
    master_data$Tmin_median_Sep[ROW] <- median(met_data$tmin_C[mask_Sep], na.rm=TRUE)
    master_data$Tmax_mean_Sep[ROW] <- mean(met_data$tmax_C[mask_Sep], na.rm=TRUE)
    master_data$Tmax_median_Sep[ROW] <- median(met_data$tmax_C[mask_Sep], na.rm=TRUE)
    master_data$Tmean_Sep[ROW] <- mean(0.5*(met_data$tmin_C[mask_Sep]+met_data$tmax_C[mask_Sep]), 
                                             na.rm=TRUE)
    
    #CDD for september
    mean_temp_Sep <- 0.5*(met_data$tmin_C[mask_Sep]+met_data$tmax_C[mask_Sep])
    master_data$Tmean_Sep[ROW] <- mean(mean_temp_Sep, na.rm=TRUE)  
    
    master_data$CDD_from_20C_Sep[ROW] <- 30*mean(20-mean_temp_Sep[mean_temp_Sep<20], na.rm=TRUE)
    master_data$CDD_from_25C_Sep[ROW] <- 30*mean(25-mean_temp_Sep[mean_temp_Sep<25], na.rm=TRUE)
    
    
    # The 30-day photoperiod is the mean over all days in September
    master_data$photoperiod_mean_Sep[ROW] <- mean(daylength(lat=master_data$Latitude[ROW],
                                                                  doy=as.Date(met_data$Date[mask_Sep])))  
    
    ############ OCTOBER ############ 
    
    mask_Oct <- (DOY <= 304) & (DOY >= 274)
    master_data$Precip_total_Oct[ROW] <- 31*mean(met_data$ppt_mm[mask_Oct],na.rm=TRUE)
    master_data$Tmin_mean_Oct[ROW] <- mean(met_data$tmin_C[mask_Oct], na.rm=TRUE)
    master_data$Tmin_median_Oct[ROW] <- median(met_data$tmin_C[mask_Oct], na.rm=TRUE)
    master_data$Tmax_mean_Oct[ROW] <- mean(met_data$tmax_C[mask_Oct], na.rm=TRUE)
    master_data$Tmax_median_Oct[ROW] <- median(met_data$tmax_C[mask_Oct], na.rm=TRUE)
    master_data$Tmean_Oct[ROW] <- mean(0.5*(met_data$tmin_C[mask_Oct]+met_data$tmax_C[mask_Oct]), 
                                             na.rm=TRUE)
    mean_temp_Oct <- 0.5*(met_data$tmin_C[mask_Oct]+met_data$tmax_C[mask_Oct])
    master_data$Tmean_Oct[ROW] <- mean(mean_temp_Oct, na.rm=TRUE)  
    
    master_data$CDD_from_20C_Oct[ROW] <- 31*mean(20-mean_temp_Oct[mean_temp_Oct<20], na.rm=TRUE)
    master_data$CDD_from_25C_Oct[ROW] <- 31*mean(25-mean_temp_Oct[mean_temp_Oct<25], na.rm=TRUE)
    
    # The 30-day photoperiod is the mean over all days in October
    master_data$photoperiod_mean_Oct[ROW] <- mean(daylength(lat=master_data$Latitude[ROW],
                                                                  doy=as.Date(met_data$Date[mask_Oct])))
    
    
  } # end if has_met_lat_lon  
} # end for loop over ROW

# # Okay, finally, growing and chilling degree days should only be NA if there is no
# # temperature data for that data range. If the mean temperature is not NA, but
# # the GDD/CDD is NA, we should set GDD/CDD to zero instead of NA (this is just
# # an artifact of the method we use to calculate GDD/CDD -- a good method for
# # other reasons).
# # Fix NA GDD/CDD:
no_mean_temp_data_Aug <- is.na(master_data$Tmean_Aug)
no_mean_temp_data_Sep <- is.na(master_data$Tmean_Sep)
no_mean_temp_data_Oct <- is.na(master_data$Tmean_Oct)

CDD_NA_20C_Aug <- is.na(master_data$CDD_from_20C_Aug)
CDD_NA_20C_Sep <- is.na(master_data$CDD_from_20C_Sep)
CDD_NA_20C_Oct <- is.na(master_data$CDD_from_20C_Oct)

CDD_NA_25C_Aug <- is.na(master_data$CDD_from_25C_Aug)
CDD_NA_25C_Sep <- is.na(master_data$CDD_from_25C_Sep)
CDD_NA_25C_Oct <- is.na(master_data$CDD_from_25C_Oct)

master_data$CDD_from_20C_Aug[CDD_NA_20C_Aug & !no_mean_temp_data_Aug] <- 0
master_data$CDD_from_20C_Sep[CDD_NA_20C_Sep & !no_mean_temp_data_Sep] <- 0
master_data$CDD_from_20C_Oct[CDD_NA_20C_Oct & !no_mean_temp_data_Oct] <- 0
master_data$CDD_from_25C_Aug[CDD_NA_25C_Aug & !no_mean_temp_data_Aug] <- 0
master_data$CDD_from_25C_Sep[CDD_NA_25C_Sep & !no_mean_temp_data_Sep] <- 0
master_data$CDD_from_25C_Oct[CDD_NA_25C_Oct & !no_mean_temp_data_Oct] <- 0

# Caluculate the cummulative CDD for the whole Aug-Oct periods:
master_data$CDD_from_20C_ASO <- master_data$CDD_from_20C_Aug + 
  master_data$CDD_from_20C_Sep + master_data$CDD_from_20C_Oct
master_data$CDD_from_25C_ASO <- master_data$CDD_from_25C_Aug + 
  master_data$CDD_from_25C_Sep + master_data$CDD_from_25C_Oct





# Save new master plus met csv:
write.csv(master_data, file="Master_Plus_Met_wCDD_150126.csv")








#########################################
#########################################
#########################################
#########################################
#########################################

AG_plus_met <- master_data

# Calculate the mean sample year for single year studies:
AG_plus_met$Year_Sampled_End[is.na(AG_plus_met$Year_Sampled_End)] <-
  AG_plus_met$Year_Sampled_Start[is.na(AG_plus_met$Year_Sampled_End)]
AG_plus_met$Mean_Sample_Year <- 0.5*(AG_plus_met$Year_Sampled_Start + 
                                           AG_plus_met$Year_Sampled_End)
hist(AG_plus_met$Mean_Sample_Year) # Not too surprising... most data is recent

# Pick out the data we might be dealing with (just for simplicity in looking at data frame):
relevant_data <- AG_plus_met[c("Latitude","Longitude",
                                   "Experimental_Unit","pheno_method","Measurement_Category",
                                   "Mean_Sample_Year",
                                   "DOY",
                                   "Precip_total_Aug","Precip_total_Sep","Precip_total_Oct",	
                                   "Tmin_mean_Aug","Tmin_mean_Sep","Tmin_mean_Oct",
                                   "Tmin_median_Aug","Tmin_median_Sep","Tmin_median_Oct",
                                   "Tmax_mean_Aug","Tmax_mean_Sep","Tmax_mean_Oct",
                                   "Tmax_median_Aug","Tmax_median_Sep","Tmax_median_Oct",
                                   "Tmean_Aug","Tmean_Sep","Tmean_Oct",
                                   "CDD_from_20C_Aug","CDD_from_20C_Sep","CDD_from_20C_Oct",
                                   "CDD_from_25C_Aug","CDD_from_25C_Sep","CDD_from_25C_Oct",	
                                   "CDD_from_20C_ASO","CDD_from_25C_ASO",
                                   "photoperiod_mean_Aug","photoperiod_mean_Sep","photoperiod_mean_Oct")]
