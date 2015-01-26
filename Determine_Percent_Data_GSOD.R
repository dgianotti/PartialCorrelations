# Determine Percent Data
Determine_Percent_Data_GSOD <- function(isd.station.usaf,
                                        isd.station.wban,
                                        year.start.pheno,
                                        year.end.pheno,
                                        data.type,
                                        completeness.required){
  
  
  USAF = isd.station.usaf
  if (data.type =="point"){
    dir.create(paste("Met_Data_Point/",USAF,sep=""))
  }else{
    dir.create(paste("Met_Data_Slope/",USAF,sep=""))
  }
  
  
  WBAN = isd.station.wban
  if (nchar(WBAN) == 4) {
    WBAN = paste("0",WBAN,sep="")
  }
  
  year_st = year.start.pheno
  year_end = year.end.pheno
  

  year_list = year_st:year_end
  
  for (YEAR in year_list) {
    
    downloadURL = paste("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/",YEAR,"/",sep = "")
    file_name = paste(USAF,"-",WBAN,"-",YEAR,".op.gz",sep ="")
    
    if (data.type =="point"){
      try(download.file(paste(downloadURL,file_name,sep=""), 
                        paste("Met_Data_Point/",USAF,"/",file_name,sep="")),silent=TRUE)
    }else{
      try(download.file(paste(downloadURL,file_name,sep=""), 
                        paste("Met_Data_Slope/",USAF,"/",file_name,sep="")),silent=TRUE)
    } 
  }
  
  
  if (data.type =="point"){
    files=dir(paste("Met_Data_Point","/",USAF,sep=""),"*.op.gz",full.names=T)
  }else{
    files=dir(paste("Met_Data_Slope","/",USAF,sep=""),"*.op.gz",full.names=T)
  }
  
  start_year=min(substr(files,nchar(files)-9,nchar(files)-6))
  end_year=max(substr(files,nchar(files)-9,nchar(files)-6))
  alldates=data.frame(Date=seq(from=as.Date(paste(start_year,"-01-01",sep="")), 
                                to=as.Date(paste(end_year,"-12-31",sep="")), by=1))
  
  stn=matrix()
  tmin=matrix()
  tmax=matrix()
  ppt=matrix()
  Date=matrix()
  
  for (tmpfile in files){
    tmpstring<-grep("MAX",readLines( gzfile(tmpfile)),value=TRUE,invert=TRUE)
    stn<-c(stn,as.numeric(as.character(substring(tmpstring,1,5))))
    tmax<-c(tmax,as.numeric(as.character(substring(tmpstring,103,108))))
    tmin<-c(tmin,as.numeric(as.character(substring(tmpstring,111,116))))
    ppt<-c(ppt,as.numeric(as.character(substring(tmpstring,119,123))))
    Date<-c(Date,as.Date(yearmoda<-substring(tmpstring,15,22),
                           "%Y%m%d")) }
  
  stn<-as.numeric(stn)
  ppt<-as.numeric(ppt)
  tmax<-as.numeric(tmax)
  tmin<-as.numeric(tmin)
  Date<-as.Date(as.numeric(Date), origin="1970-01-01")
  forcing=data.frame(stn=stn,ppt=ppt,tmax=tmax,tmin=tmin,
                     Date=as.Date(Date))
  forcing=na.omit(forcing)
  forcing=merge(alldates,forcing,all=TRUE)
  forcing$tmax_C <- round((forcing$tmax-32)*5/9, digits=2)
  forcing$tmin_C <- round((forcing$tmin-32)*5/9, digits=2)
  forcing$ppt_mm <- forcing$ppt*25.4
  forcing$ppt_mm[forcing$ppt_mm > 999]=0.0
  
  forcingV2 <- forcing[c("Date","tmax_C","tmin_C","ppt_mm")]
  percent_data = nrow(na.omit(forcingV2))/nrow(forcingV2)
  
  if (percent_data >= completeness.required){
    if (data.type =="point"){
      write.csv(file = paste("Formatted_Met_Data_Point/",USAF,".csv",sep=""), forcingV2, row.names=FALSE)
      unlink(paste("Met_Data_Point","/",USAF,sep=""),recursive=TRUE)
    }else{
      write.csv(file = paste("Formatted_Met_Data_Slope/",USAF,".csv",sep=""), forcingV2, row.names=FALSE)
      unlink(paste("Met_Data_Slope","/",USAF,sep=""),recursive=TRUE)
    }
  }
  
  return(percent_data)
}

  
  