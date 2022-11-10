dat<-read.csv('simData.csv')

# get the contact pairs
pairs<-t(combn(unique(dat$host),2))

# setup working directory to save output
outfile<-'D:/contact/'
# setup temporal threshold in seconds
tdist<-5*24*60*60
# setup distance threshold in meters
spdist<-2
  
for(i in 1:nrow(pairs)){
    cat("Working on pairs",pairs[i,] , "\n")
    host1<-dat[dat$host==pairs[i,1],]
    host2<-dat[dat$host==pairs[i,2],]
    contactpairs<-data.frame(id1=as.character(),id2=as.character(),datetime1=as.numeric(),
                             datetime2=as.numeric(), X1=as.numeric(),Y1=as.numeric(),
                             X2=as.numeric(),Y2=as.numeric(),dist=as.numeric(),
                             x=as.numeric(),y=as.numeric())
    for(j in 1:nrow(host1)){
     # print(j)
      host2.tmp<-host2[abs(host2$time-host1$time[j])<=tdist,]
      host1.tmp<-host1[j,]
      if(nrow(host2.tmp)>0){
        coordinates(host2.tmp)<-c('x','y')
        coordinates(host1.tmp)<-c('x','y')
        host2.tmp$dist<-spDistsN1(host2.tmp,host1.tmp)
        host2.tmp<-host2.tmp[host2.tmp$dist<spdist,]
        if(nrow(host2.tmp@data)>0){
          contacts<-data.frame(id1=rep(host1.tmp$host,nrow(host2.tmp@data)),id2=host2.tmp$host,
                               datetime1=rep(host1.tmp$time,nrow(host2.tmp@data)),datetime2=host2.tmp$time,
                               X1=rep(host1.tmp@coords[,1],nrow(host2.tmp@data)),Y1=rep(host1.tmp@coords[,2],nrow(host2.tmp@data)),
                               X2=host2.tmp@coords[,1],Y2=host2.tmp@coords[,2],dist=host2.tmp$dist,
                               x=rowMeans(cbind(rep(host1.tmp@coords[,1],nrow(host2.tmp@data)),host2.tmp@coords[,1])),
                               y=rowMeans(cbind(rep(host1.tmp@coords[,2],nrow(host2.tmp@data)),host2.tmp@coords[,2])))
          contactpairs<-rbind.data.frame(contactpairs,contacts)
        }
      }
    }
    if(nrow(contactpairs)>0){
      write.csv(contactpairs,paste0(outfile,'/',host1$host[1],'_',host2$host[1],'.csv'),row.names = F)
    }
  }


