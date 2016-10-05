library(XML)
setwd("/Users/Martha/Desktop/Sample")   


BOAZ<-function(foldername){
  src_files<-list.files(foldername)      
  file_count <-length(src_files)
  lat.min<-37.4285 ; lat.max<-37.7014
  lon.min<-126.764 ; lon.max<-127.184
  fivePoints<-matrix("NA",ncol=12)
  
  for( i in 1:file_count){                
    root <- xmlRoot(xmlTreeParse(paste(foldername, "/", src_files[i], sep="")))[[2]]
    nrows<-xmlSize(root)-1
    points<-round(c(2,(nrows*0.25)+1,(nrows*0.5)+1,(nrows*0.75)+1,nrows+1))
    quan<-NULL ; des<-NULL
  
      for(k in 1:5){
      quan<-c(quan,as.numeric(c(xmlSApply(root[[points[k]]],xmlAttrs)[1:2])))
      des<- c(des, lat.min<=quan[2*k-1]&quan[2*k-1]<=lat.max & lon.min<=quan[2*k]&quan[2*k]<=lon.max)
         }

    if(sum(des)>0){
      ncols<-xmlSize(root[[2]])
      path.mat<-rep(NA,nrows*ncols)
      for (j in 2:(nrows+1)){
        path.mat[(ncols*(j-2)+1):(ncols*(j-2)+ncols)]<-xmlSApply(root[[j]],xmlAttrs)
      }
      path.mat<-as.numeric(path.mat)
      path.mat<-as.data.frame(matrix(path.mat,ncol=ncols,byrow=T))
      names(path.mat)<-names(root[[2]]) 
      total_dist<-as.numeric(path.mat[nrow(path.mat),"rr_location_total_distance"])
      write.csv(path.mat,  
                 file=paste("work/",foldername,"/",
                            substr(src_files[i],1,nchar(src_files[i])-4),".csv",sep=""),
                 row.names=F) 
       points_with_id<-c(substr(src_files[i],1,nchar(src_files[i])-4),quan,total_dist)
       fivePoints<-rbind(fivePoints,points_with_id)

    }
    
    else {}
  }
   colnames(fivePoints)<-c("fileID","Start(lat)","Start(lon)","Q1(lat)","Q1(lon)",
                           "Q2(lat)","Q2(lon)","Q3(lat)","Q3(lon)","End(lat)","End(lon)","Moving_dist")
   write.csv(fivePoints[-1,],paste("work/",foldername,"/","fivePoints.csv",sep=""),row.names=F)
   write.csv(subset(fivePoints,select=c("Start(lat)","Start(lon)","fileID")),
             file=paste("work/",foldername,"/startPoints.csv",sep=""),row.names=F)
   write.csv(subset(fivePoints,select=c("fileID","End(lat)","End(lon)","fileID")),
             file=paste("work/",foldername,"/endPoints.csv",sep=""),row.names=F)
}

