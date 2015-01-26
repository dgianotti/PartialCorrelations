# Determine Percent Data
Determine_Percent_Data_GHCN <- function(st.id,
                                        year.start.pheno,
                                        year.end.pheno,
                                        data.type,
                                        completeness.required){

  if (data.type=="point"){
    dir.create(paste("Met_Data_Point/",st.id,sep=""))
  }else{
    dir.create(paste("Met_Data_Slope/",st.id,sep="")) 
  }
  
  downloadURL = paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/",sep = "")
  file_name = paste(st.id,".dly",sep ="")
  
  if(data.type =="point"){
    try(download.file(paste(downloadURL,file_name,sep=""), 
                      paste("Met_Data_Point/",st.id,"/",file_name,sep="")),silent=TRUE)
  }else{
    try(download.file(paste(downloadURL,file_name,sep=""), 
                      paste("Met_Data_Slope/",st.id,"/",file_name,sep="")),silent=TRUE)
  } 

  daily_widths = c(5,1,1,1)
  widths = c(11,4,2,4,rep(daily_widths,31))
  columns = c("ID","YEAR","MONTH","ELEMENT","VAL01","MF01","QF01","SF01",
              "VAL02","MF02","QF02","SF02",
              "VAL03","MF03","QF03","SF03",
              "VAL04","MF04","QF04","SF04",
              "VAL05","MF05","QF05","SF05",
              "VAL06","MF06","QF06","SF06",
              "VAL07","MF07","QF07","SF07",
              "VAL08","MF08","QF08","SF08",
              "VAL09","MF09","QF09","SF09",
              "VAL10","MF10","QF10","SF10",
              "VAL11","MF11","QF11","SF11",
              "VAL12","MF12","QF12","SF12",
              "VAL13","MF13","QF13","SF13",
              "VAL14","MF14","QF14","SF14",
              "VAL15","MF15","QF15","SF15",
              "VAL16","MF16","QF16","SF16",
              "VAL17","MF17","QF17","SF17",
              "VAL18","MF18","QF18","SF18",
              "VAL19","MF19","QF19","SF19",
              "VAL20","MF20","QF20","SF20",
              "VAL21","MF21","QF21","SF21",
              "VAL22","MF22","QF22","SF22",
              "VAL23","MF23","QF23","SF23",
              "VAL24","MF24","QF24","SF24",
              "VAL25","MF25","QF25","SF25",
              "VAL26","MF26","QF26","SF26",
              "VAL27","MF27","QF27","SF27",
              "VAL28","MF28","QF28","SF28",
              "VAL29","MF29","QF29","SF29",
              "VAL30","MF30","QF30","SF30",
              "VAL31","MF31","QF31","SF31")
  
  # Open GHCN Data
  if (data.type=="point"){
    ghcn_data = read.fwf(paste("Met_Data_Point/",st.id,"/",file_name,sep=""),
                         widths,header=FALSE,col.names=columns)
  }else{
    ghcn_data = read.fwf(paste("Met_Data_Slope/",st.id,"/",file_name,sep=""),
                         widths,header=FALSE,col.names=columns)
  }
    
  
  # Throw out everything but Tmax, Tmin, Precip
  ghcn_data = subset(ghcn_data,ghcn_data$ELEMENT == "PRCP" | ghcn_data$ELEMENT == "TMAX" | ghcn_data$ELEMENT == "TMIN")
  
  # Throw out unnesccesary years
  ghcn_data = subset(ghcn_data, ghcn_data$YEAR >= year.start.pheno & ghcn_data$YEAR <= year.end.pheno )
  
  # Throw out ID and flags
  cols_to_keep = c("YEAR","MONTH","ELEMENT","VAL01",
                   "VAL02","VAL03","VAL04","VAL05","VAL06","VAL07","VAL08","VAL09",
                   "VAL10","VAL11","VAL12","VAL13","VAL14","VAL15","VAL16","VAL17",
                   "VAL18","VAL19","VAL20","VAL21","VAL22","VAL23","VAL24","VAL25",
                   "VAL26","VAL27","VAL28","VAL29","VAL30","VAL31")
  
  ghcn_data =ghcn_data[,cols_to_keep] 
  
  if(nrow(ghcn_data) == 0){
    percent_data = 0
    return(percent_data)
  }
  
  # change -9999 to NA
  ghcn_data[ghcn_data==-9999] = NA
  
  # Create Dates
  formatted_data = data.frame(Date=seq(from=as.Date(paste(year.start.pheno,"-01-01",sep="")), 
                               to=as.Date(paste(year.end.pheno,"-12-31",sep="")), by=1),
                              tmax_C = NA, tmin_C = NA, ppt_mm = NA)

  month_lengths <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  for (ROW in 1:nrow(ghcn_data)){

    formatted_data_vec = as.vector(unlist(ghcn_data[ROW,4:34]))
    
    # If February in leap year:
    if( (ghcn_data$YEAR[ROW]%%4 == 0) & (ghcn_data$MONTH[ROW] == 2) ){ 
      month_lengths[2] = 29
      formatted_data_vec = formatted_data_vec[1:month_lengths[ghcn_data$MONTH[ROW]]]
      month_lengths[2] = 28        
    }else { # Not leap year February:
      formatted_data_vec = formatted_data_vec[1:month_lengths[ghcn_data$MONTH[ROW]]]
    }
    
    # Calculate the start index:
    first_date = as.Date(paste(ghcn_data$YEAR[ROW],ghcn_data$MONTH[ROW],"01",sep="-"))
    start_index = which(formatted_data$Date==first_date)
    end_index = start_index + length(formatted_data_vec) - 1
    
    if (as.character(ghcn_data$ELEMENT[ROW]) == "PRCP"){ 
      formatted_data$ppt_mm[start_index:end_index] = formatted_data_vec/10 # unit convertion from tenths of mm
    }else if(as.character(ghcn_data$ELEMENT[ROW]) == "TMAX"){
      formatted_data$tmax_C[start_index:end_index] = formatted_data_vec/10 # unit convertion from tenths of deg-C
    }else{ #TMIN
      formatted_data$tmin_C[start_index:end_index] = formatted_data_vec/10 # unit convertion from tenths of deg-C
    }
    
  }
  
  percent_data = nrow(na.omit(formatted_data))/nrow(formatted_data)
  
  if (percent_data >= completeness.required){
    if (data.type =="point"){
      write.csv(file = paste("Formatted_Met_Data_Point/",st.id,".csv",sep=""), formatted_data, row.names=FALSE)
      unlink(paste("Met_Data_Point","/",st.id,sep=""),recursive=TRUE)
    }else{
      write.csv(file = paste("Formatted_Met_Data_Slope/",st.id,".csv",sep=""), formatted_data, row.names=FALSE)
      unlink(paste("Met_Data_Slope","/",st.id,sep=""),recursive=TRUE)
    }
  }
  
  return(percent_data)
}


