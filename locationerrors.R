#### location errors
dat<-read.csv('simData.csv')

pred<-c()
list<-list.files('./Simulation/CRAWL/4h',full.names = T)
for(i in 1:10){
  load(list[i])
  pred<-rbind(pred,sf_pred_points)
}

pred$time_s<-as.numeric(pred$datetime)-as.numeric(as.POSIXct('2022-09-21 09:21:00',tz='UTC',format='%Y-%m-%d %H:%M:%S'))
dat<-dat[dat$time<=max(pred$time_s),]

dat$x_diff<-dat$y_diff<-0
dat$x_diff[dat$host==pred$deployid & dat$time==pred$time_s]<-pred$x[pred$deployid==dat$host & pred$time_s==dat$time]-dat$x[dat$host==pred$deployid & dat$time==pred$time_s]
dat$y_diff[dat$host==pred$deployid & dat$time==pred$time_s]<-pred$y[pred$deployid==dat$host & pred$time_s==dat$time]-dat$y[dat$host==pred$deployid & dat$time==pred$time_s]

dat$dist_diff<-sqrt(dat$x_diff^2+dat$y_diff^2)
dat$group<-'4h'
write.csv(dat,'./Simulation/DataDropping/4h/ctmm/compare_crawl.csv',row.names = F)

dat<-c()
temp<-c('5min','10min','30min','1h','2h','4h')
for(i in 1:length(temp)){
  temp1<-read.csv(paste0('./Simulation/DataDropping/',temp[i],'/ctmm/compare_crawl.csv'))
  temp1$gp<-i
  dat<-rbind(dat,temp1)
}


tiff("./Simulation/Case1/dist_diff.tiff",width=6,height=6
     ,units="in",res=300,compression="lzw",type="cairo",family="arial")

boxplot(abs(dat$dist_diff)~dat$gp,outline=F,col='gray',xaxt='n',xlab='GPS Intervals',ylab = 'Errors')
axis(1,1:6,temp)
dev.off()