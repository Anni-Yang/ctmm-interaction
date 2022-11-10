################################## contacts based on interpolated trajectory #################################
# get the predicted trajectory
id<-0:9
pairs<-combn(id,2)

pred<-c()
list<-list.files('./CRAWL/1h',full.names = T)
for(i in 1:10){
  load(list[i])
  pred<-rbind(pred,sf_pred_points)
}

dat<-pred
dat$time<-as.numeric(pred$datetime)-as.numeric(as.POSIXct('2022-09-21 09:21:00',tz='UTC',format='%Y-%m-%d %H:%M:%S'))

### run the interactionEvents_loop.R code to extract contact and overlaptime.R calculate overlap time
library(dplyr)
library(readr)
# get all the interaction events with overlaptime calculated in one single file
data_all <- list.files(path = "./CRAWL/contact/1h1/",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows 

# calculate interaction weights
data_all$weights<-1/data_all$overlap*exp(-(abs(data_all$datetime1-data_all$datetime2)/(60*24*60)))

# define ids for animals
ids<-seq(0,9)

## direct interaction
# generate an empty kernel for direct interaction
kernel_dir<-matrix(0,nrow = length(ids),ncol = length(ids))
rownames(kernel_dir)<-colnames(kernel_dir)<-ids

# subset for direct interaction
temp<-data_all[data_all$datetime1==data_all$datetime2,]

for(j in 1:nrow(temp)){
  kernel_dir[row.names(kernel_dir)==min(c(temp$id1[j],temp$id2[j])),colnames(kernel_dir)==max(c(temp$id1[j],temp$id2[j]))]<-kernel_dir[row.names(kernel_dir)==min(c(temp$id1[j],temp$id2[j])),colnames(kernel_dir)==max(c(temp$id1[j],temp$id2[j]))]+
    temp$weights[j]
}
library(raster)

kernel_dir.ras1<-raster(log(kernel_dir))

kernel_dir[kernel_dir==0]<-NA

# plot the kernel
colfunc <- c("#e0f3db", '#084081')
kernel_dir.ras<-raster(log(kernel_dir))
kernel_dir.ras.shp<-rasterToPolygons(kernel_dir.ras1)
tiff("mat_dir1h.tiff",width=6,height=5.86
     ,units="in",res=300,compression="lzw",type="cairo",family="arial")
plot(kernel_dir.ras,axes = FALSE,box=FALSE,col = colfunc[2], axis.args=list(at=c(0.1,0.286),
                                                                            labels=c(0.1,0.286), 
                                                                            cex.axis=0.6),mgp=c(0.5,1,.5))
plot(kernel_dir.ras.shp,add=T,border='gray80',lwd=0.3)
at=c()
at[1]<-res(kernel_dir.ras)[1]/2
for(i in 2:nrow(kernel_dir)){
  at[i]<-at[1]+res(kernel_dir.ras)[1]*(i-1)
}
axis(3,at=at,labels = ids,cex.axis=0.6,las=2)

axis(2,at=at,labels = ids[length(ids):1],cex.axis=0.6,las=2)
lines(c(1,0),c(0,1),col='gray80')
box(lty =1)
dev.off()

## indirect interaction
kernel_ind<-matrix(0,nrow = length(ids),ncol = length(ids))
rownames(kernel_ind)<-colnames(kernel_ind)<-ids

# subset for indirect interaction
temp<-data_all[data_all$datetime1!=data_all$datetime2,]
# summarize the indirect interaction weights for each pair
temp1<-temp[temp$datetime1-temp$datetime2>0,]
if(nrow(temp1)>0){
  for(j in 1:nrow(temp1)){
    kernel_ind[temp1$id1[j],temp1$id2[j]]<- kernel_ind[temp1$id1[j],temp1$id2[j]] + temp1$weights[j]
  }
}
temp1<-temp[temp$datetime1-temp$datetime2<0,]  
if(nrow(temp1)>0){
  for(j in 1:nrow(temp1)){
    kernel_ind[temp1$id2[j],temp1$id1[j]]<- kernel_ind[temp1$id2[j],temp1$id1[j]] + temp1$weights[j]
  }
}

kernel_ind.ras1<-raster(log(kernel_ind))


kernel_ind[kernel_ind==0]<-NA


colfunc <- c('#ffffe5',
             '#fff7bc',
             '#fee391',
             '#fec44f',
             '#fe9929',
             '#ec7014',
             '#cc4c02',
             '#993404',
             '#662506')
kernel_ind.ras<-raster(log(kernel_ind))
kernel_ind.ras.shp<-rasterToPolygons(kernel_ind.ras1)

tiff("mat_ind1h.tiff",width=6,height=5.86
     ,units="in",res=300,compression="lzw",type="cairo",family="arial")
plot(kernel_ind.ras,axes = FALSE,box=FALSE,col = colfunc, breaks = log(c(0.131,0.27,0.632,1.48,3.463,8.105,18.968,44.389,103.879,max(kernel_ind,na.rm = T))),
     axis.args=list(at=log(c(0.131,0.27,0.632,1.48,3.463,8.105,18.968,44.389,103.879,max(kernel_ind,na.rm = T))),
                    labels=round(c(0.131,0.27,0.632,1.48,3.463,8.105,18.968,44.389,103.879,max(kernel_ind,na.rm = T)),3), 
                    cex.axis=0.6),mgp=c(0.5,1,.5))

plot(kernel_ind.ras.shp,add=T,border='gray80',lwd=0.3)
at=c()
at[1]<-res(kernel_ind.ras)[1]/2
for(i in 2:nrow(kernel_ind)){
  at[i]<-at[1]+res(kernel_ind.ras)[1]*(i-1)
}
axis(3,at=at[c(1:10)],labels = ids,cex.axis=0.6,las=2)
axis(2,at=at[11-c(1:10)],labels = ids,cex.axis=0.6,las=2)
lines(c(1,0),c(0,1),col='gray80')
box(lty =1)
dev.off()

