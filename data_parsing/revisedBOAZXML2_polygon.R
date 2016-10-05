library(XML)
library(rgdal)

setwd("/Users/Martha/Desktop/Sample")   

# 필요변수들.
polypath <- "boundary/dong"
polylayer <- "bnd_dong_11_2014"
# 서울 경계 파일의 projection 방법 입력.
polycrs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs")
pointcrs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

GetPolygon <- function(folderpath, layername, polygondatacrs=NULL, pointcrs=NULL)
{
  # 서울 경계 파일을 Sample폴더 안에 boundary라는 폴더를 만들어서 위치시켜야 함.
  # 서울 경계 파일(Polygon형태의 shp파일) 읽기를 먼저 실행하고 함수에 입력.
  sp_polygon <- readOGR(dsn = folderpath, layer = layername)
  
  if(is.na(proj4string(sp_polygon)) & !is.null(polygondatacrs))
    proj4string(sp_polygon) <- polygondatacrs
  
  # 서울 경계 파일의 projection 방법을 경로 데이터들과 같게 변환.(EPSG 4326으로)
  if(!is.na(proj4string(sp_polygon)) & !is.null(pointcrs))
    sp_polygon <- spTransform(sp_polygon, pointcrs)
  
  return(sp_polygon)
}

GetAdmname <- function(movedata, adm_polygon, movedatacrs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
{
  fileclass <- class(movedata)
  if(fileclass == "SpatialPoints"){
    
    sp_points <- movedata
    
  }else if(fileclass == "numeric"){
    
    sp_points <- data.frame(matrix(movedata, nrow = length(movedata)/2, ncol = 2, byrow = TRUE))
    colnames(sp_points) <- c("latitude", "longitude")
    sp_points <- sp_points[2:1]
    coordinates(sp_points) <- c("longitude", "latitude")
    
  }else if(fileclass == "data.frame"){
    
    sp_points <- movedata[2:1]
    coordinates(sp_points) <- c("longitude", "latitude")
    
  }else{
    print(paste0("Error filetype: ", fileclass))
    return(0)
  }
  
  # 경로 데이터의 projection 방법 입력.(EPSG 4326)
  if(is.na(proj4string(sp_points)))
    proj4string(sp_points) <- movedatacrs
  
  # SpatialPolygon안에 경로데이터 점들이 위치하는지 TRUE/FALSE 로 반환.
  isinpolygon <- !is.na(over(sp_points, as(adm_polygon, "SpatialPolygons")))
  
  # 서울안에 위치하는 점들의 비율/수.
  print(paste("Ratio",ratio <- mean(isinpolygon), sep=": "))
  print(paste("Total",cnt <- sum(isinpolygon), sep=": "))
  
  # 각 점이 어느 동에 위치하는지 붙이기.
  pointsattr <- over(sp_points, adm_polygon)
  # shp파일로 만들어서 qgis에서 불러올 시 동이름이 속성 테이블에 붙음.
  sp_points$dong <- pointsattr$adm_dr_nm # 어떤 polygon파일을 불러오냐에 따라 "adm_dr_nm"부분 달라짐.
  
  return(sp_points$dong)
}

BOAZ<-function(foldername){
  src_files<-list.files(foldername)      
  file_count <-length(src_files)
  #lat.min<-37.4285 ; lat.max<-37.7014
  #lon.min<-126.764 ; lon.max<-127.184
  fivePoints<-matrix("NA",ncol=12)
  polygondata<-GetPolygon(polypath, polylayer, polycrs, pointcrs)
  
  start_dong<-vector(); end_dong <-vector()
  
  
  for( i in 1:file_count){                
    root <- xmlRoot(xmlTreeParse(paste(foldername, "/", src_files[i], sep="")))[[2]]
    nrows<-xmlSize(root)-1
    points<-round(c(2,(nrows*0.25)+1,(nrows*0.5)+1,(nrows*0.75)+1,nrows+1))
    quan<-NULL ; des<-NULL
  
      for(k in 1:5){
      quan<-c(quan,as.numeric(c(xmlSApply(root[[points[k]]],xmlAttrs)[1:2])))
      }
    des <- !is.na(GetAdmname(quan, polygondata)); print(des)

    if(sum(des)>0){
      ncols<-xmlSize(root[[2]])
      path.mat<-rep(NA,nrows*ncols)
      for (j in 2:(nrows+1)){
        path.mat[(ncols*(j-2)+1):(ncols*(j-2)+ncols)]<-xmlSApply(root[[j]],xmlAttrs)
      }
      path.mat<-as.numeric(path.mat)
      path.mat<-as.data.frame(matrix(path.mat,ncol=ncols,byrow=T))
      names(path.mat)<-gsub("rr_location_", "", names(root[[2]]))
      total_dist<-as.numeric(path.mat[nrow(path.mat),"total_distance"])
      
      points_adm<-GetAdmname(path.mat, polygondata)
      path.mat$dong<-points_adm
      
      start_dong<-c(start_dong, points_adm[1])
      end_dong<-c(end_dong, points_adm[nrows])
    
      write.csv(path.mat,  
                 file=paste("work/",foldername,"/",
                            substr(src_files[i],1,nchar(src_files[i])-4),".csv",sep=""),
                 row.names=F) 
       points_with_id<-c(substr(src_files[i],1,nchar(src_files[i])-4),quan,total_dist)
       fivePoints<-rbind(fivePoints,points_with_id)

    }
    
    else {}
  }
   fivePoints <- as.data.frame(fivePoints[-1,])
   fivePoints$Start_dong <- start_dong
   fivePoints$End_dong <- end_dong
   colnames(fivePoints)<-c("fileID","Start(lat)","Start(lon)","Q1(lat)","Q1(lon)",
                           "Q2(lat)","Q2(lon)","Q3(lat)","Q3(lon)","End(lat)","End(lon)","Moving_dist","Start_dong","End_dong")
   write.csv(fivePoints,paste("work/",foldername,"/","fivePoints.csv",sep=""),row.names=F)
   write.csv(subset(fivePoints,select=c("fileID","Start(lat)","Start(lon)", "Start_dong")),
             file=paste("work/",foldername,"/startPoints.csv",sep=""),row.names=F)
   write.csv(subset(fivePoints,select=c("fileID","End(lat)","End(lon)", "End_dong")),
             file=paste("work/",foldername,"/endPoints.csv",sep=""),row.names=F)
}
