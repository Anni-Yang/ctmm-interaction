## get overlap time
# setup input folder, the folder of interaction events
input<-'D:/contact/'
# setup output folder 
output<-'D:/contact1/'
listfile<-list.files(input,full.names = T)
namefile<-list.files(input)
gpsdata<-read.csv('simData.csv')
gpsdata$datetime<-as.POSIXct(gpsdata$time,origin='1970-01-01',tz='UTC') # we set up a random original date since it's simulation
for(i in 1:length(listfile)){
  print(i)
  dat<-read.csv(listfile[i])
  dat$datetime1.1<-as.POSIXct(dat$datetime1,origin='1970-01-01',tz='UTC')
  dat$datetime2.1<-as.POSIXct(dat$datetime2,origin='1970-01-01',tz='UTC')
  
  dat$overlap=0
  dat$date1<-format.Date(dat$datetime1.1,format='%Y-%m-%d')
  dat$date2<-format.Date(dat$datetime2.1,format='%Y-%m-%d')
  dat$date_pair<-paste0(dat$date1,'_',dat$date2)
  for(j in 1:length(unique(dat$date_pair))){
    temp3=dat[dat$date_pair==unique(dat$date_pair)[j],]
    temp<-gpsdata[gpsdata$host==dat$id1[1],]
    temp1<-gpsdata[gpsdata$host==dat$id2[1],]
    if(difftime(temp3$date1[1],temp3$date2[1],units = 'days')<=0){
      temp1$datetime<-temp1$datetime-abs(difftime(temp3$date1[1],temp3$date2[1],units = 'days'))
    }else {
      temp$datetime<-temp$datetime-abs(difftime(temp3$date1[1],temp3$date2[1],units = 'days'))
    }
    temp$date<-as.Date(temp$datetime)
    temp1$date<-as.Date(temp1$datetime)
    temp<-temp[temp$date%in%temp1$date,]
    dat$overlap[dat$date_pair==unique(dat$date_pair)[j]]<-abs(difftime(temp$date[1],temp$date[nrow(temp)],units = 'days'))+1
  }
  dat$datetime1.1<-dat$datetime2.1<-dat$date1<-dat$date2<-dat$date_pair<-NULL
  write.csv(dat,paste0(output,'/',namefile[i]),row.names = F)
}

