# XML Parsing.
library(XML)
# Polygon 읽고, 점들이 속한 동 확인하는 함수 있음.
library(rgdal)

# 서울 폴리곤 읽어오는 함수.
GetPolygon <- function(folderpath, layername, polygondatacrs=NULL, exp_crs=NULL)
{
  # 서울 경계 파일을 Sample폴더 안에 boundary라는 폴더를 만들어서 위치시켜야 함.
  # 서울 경계 파일(Polygon형태의 shp파일) 읽기를 먼저 실행하고 함수에 입력.
  sp_polygon <- readOGR(dsn = folderpath, layer = layername)
  
  if(is.na(proj4string(sp_polygon)) & !is.null(polygondatacrs))
    proj4string(sp_polygon) <- polygondatacrs
  
  # 서울 경계 파일의 좌표계를 경로 데이터들과 같게 변환.(EPSG 4326으로)
  if(!is.na(proj4string(sp_polygon)) & !is.null(exp_crs))
    sp_polygon <- spTransform(sp_polygon, exp_crs)
  
  return(sp_polygon)
}

# 각 점들이 속하는 동 이름을 반환하는 함수.
GetAdmname <- function(sp_points, adm_polygon, movedatacrs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
{
  fileclass <- class(sp_points)
  # 경로 데이터를 어떤 데이터 형식으로 받냐에 따라 다르게 처리. SpatialPoints 타입으로 모두 변환해줌.
  if(fileclass == "SpatialPoints"){
    # SpatialPoints일경우 처리안함.
    
  }else if(fileclass == "numeric"){
    
    sp_points <- data.frame(matrix(sp_points, nrow = length(sp_points)/2, ncol = 2, byrow = TRUE))
    colnames(sp_points) <- c("latitude", "longitude")
    sp_points <- sp_points[2:1]
    coordinates(sp_points) <- c("longitude", "latitude")
    
  }else if(fileclass == "data.frame"){
    
    sp_points <- sp_points[2:1]
    coordinates(sp_points) <- c("longitude", "latitude")
    
  }else{
    print(paste0("Error filetype: ", fileclass))
    return(0)
  }
  
  # 경로 데이터의 투영 좌표계 정보 입력.(기본값 EPSG 4326)
  if(is.na(proj4string(sp_points)))
    proj4string(sp_points) <- movedatacrs
  
  # SpatialPolygon안에 경로데이터 점들이 위치하는지 TRUE/FALSE 로 반환.
  #isinpolygon <- !is.na(over(sp_points, as(adm_polygon, "SpatialPolygons")))
  
  # 각 점이 어느 동에 위치하는지 붙이기.
  pointsattr <- over(sp_points, adm_polygon)
  # shp파일로 만들어서 qgis에서 불러올 시 동이름이 속성 테이블에 붙음.
  sp_points$dong <- pointsattr$adm_dr_nm # 어떤 polygon파일을 불러오냐에 따라 "adm_dr_nm"부분 달라짐.
  
  # 각 점들의 동 정보가 담긴 벡터 반환.
  return(sp_points$dong)
}

# csv 파일을 shp 파일로 변환하는 함수.
CSV_to_SHP <- function(foldername, datacrs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
{
  list <- list.files(paste("work/",foldername,sep="")) #추출된 폴더의 csv 파일 리스트
  # 시작점, 끝점, 사분위점만 찍힌csv파일 제거.
  list <- list[-match(c("fivePoints.csv", "endPoints.csv", "startPoints.csv"), list)]
  count <- length(list) #csv 파일 리스트 개수
  
  #변수가 22개일때 23개일때 각각 변수명 바꾸기. 10자 넘어가면 변환안됨... 22는 moving_distance 없음.
  var_22 <- c("latitude","longitude","elevation","ele_gain",
              "ele_min","ele_max","grade","grade_min",
              "grade_max","s","s_max","s_avg",
              "s_avg_m","direction","accuracy","total_dis",
              "total_cal","total_time","ttm",
              "activity","event_f","save_date") 
  var_23 <- c("latitude","longitude","elevation","ele_gain",
              "ele_min","ele_max","grade","grade_min",
              "grade_max","s","s_max","s_avg",
              "s_avg_m","direction","accuracy","total_dis",
              "total_cal","total_time","moving_dis","ttm",
              "activity","event_f","save_date") 
  for (i in 1:count) {
    shp <- read.csv(paste("work/", foldername, "/", list[i], sep=""))
    col <- colnames(shp)
    if (length(col)==22) {
      colnames(shp) <- var_22
    } else {
      colnames(shp) <- var_23
    }
    coordinates(shp) <-~ longitude + latitude
    
    # PRJ 파일 생성되게 하려면 투영좌표계 정보를 입력해주어야 함.
    proj4string(shp) <- datacrs
    
    #반드시 미리 먼저 sample/work/shp폴더를 만들어 놔야 shp파일 생성됨.
    writeOGR(shp,
             paste("work/shp/",foldername,sep=""), #이 경로에
             substr(list[i], 1, nchar(list[i])-4), #이런 파일 이름으로
             driver = "ESRI Shapefile") #좌표계설정인듯
  }
}

# 이동경로 점 사이의 간격을 늘리는 함수. (모든 계산은 초 단위로 함) 원하는 초 간격 입력. Index반환.
IDX_ChangeInterval <- function(start_time, end_time, len, exp_interval=10)
{
  ############주경 추가 시작
  start_time <- as.POSIXlt(start_time / 1000, origin="1970-01-01")
  end_time <- as.POSIXlt(end_time / 1000, origin="1970-01-01")
  total_time_range <- as.numeric(end_time-start_time, unit = "secs")
  time_interval <- total_time_range / (len - 1)
  distance <- round(exp_interval / time_interval)
  ind <- seq(1, len, by = distance)
  
  if (len != ind[length(ind)]){
    ind <- c(ind, len)
  }
  
  return(ind)
  ############주경 추가 끝
  
}

#### 메인 BOAZ 함수 ####

BOAZ <- function(foldername)
{
  time_funcstart <- Sys.time()
  
  src_files<-list.files(foldername)      
  file_count <-length(src_files)
  # lat.min<-37.4285 ; lat.max<-37.7014
  # lon.min<-126.764 ; lon.max<-127.184
  fivePoints<-matrix("NA",ncol=12)
  
  # 서울시 폴리곤을 읽기위한 변수들. (폴더 경로, 파일이름, 투영좌표계) + 자전거 경로의 투영좌표계.
  polypath <- "boundary/dong"
  polylayer <- "bnd_dong_11_2014"
  polycrs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs")
  pointcrs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # 서울시 폴리곤 읽기.
  polygondata<-GetPolygon(polypath, polylayer, polycrs, pointcrs)
  
  for( i in 1:file_count){
    
    root <- xmlRoot(xmlTreeParse(paste(foldername, "/", src_files[i], sep="")))[[2]]
    nrows<-xmlSize(root)-1
    points<-round(c(2,(nrows*0.25)+1,(nrows*0.5)+1,(nrows*0.75)+1,nrows+1))
    quan <- vector(mode = "numeric", length = 10) ; #des <- vector(mode = "logical", length = nrows)
    
    # for(k in 1:5){
    #   quan<-c(quan,as.numeric(c(xmlSApply(root[[points[k]]],xmlAttrs)[1:2])))
    #   des<- c(des, lat.min<=quan[2*k-1]&quan[2*k-1]<=lat.max & lon.min<=quan[2*k]&quan[2*k]<=lon.max)
    # }
    
    for(k in 1:5){
      quan[(2*k-1):(2*k)] <- as.numeric(c(xmlSApply(root[[points[k]]],xmlAttrs)[1:2]))
    }
    
    # 각 점들의 동 정보 확인.
    points_adm <- GetAdmname(quan, polygondata)
    # 서울시에 속하지 않을경우 그 점의 동 정보값은 NA로 반환되기 때문에, 속할경우 TRUE값을 가지게 처리.
    des <- !is.na(points_adm); #print(des)
    
    if(sum(des)>0){
      ncols<-xmlSize(root[[2]])
      
      # 몇초 간격으로 점을 찍고 싶은지 입력할 수 있음. 원하는 시간간격에 맞게 뽑힌 점들의 Index를 반환.
      ind <- IDX_ChangeInterval(as.numeric(xmlAttrs(root[[2]][[ncols]])), as.numeric(xmlAttrs(root[[nrows+1]][[ncols]]))
                                , nrows, 10)
      # time interval 변경에 따라 함께 변경된 nrows(점개수).
      nrows <- length(ind)
      path.mat<-rep(NA,nrows*ncols)
      ind <- ind + 1
      for (j in 1:nrows){
        path.mat[(ncols*(j-1)+1):(ncols*(j-1)+ncols)]<-xmlSApply(root[[ind[j]]],xmlAttrs)
      }
      
      path.mat<-as.numeric(path.mat)
      path.mat<-as.data.frame(matrix(path.mat,ncol=ncols,byrow=T))
      names(path.mat)<-gsub("rr_location_", "", names(root[[2]]))
      total_dist<-as.numeric(path.mat[nrow(path.mat),"total_distance"])
      
      write.csv(path.mat,  
                file=paste("work/",foldername,"/",
                           substr(src_files[i],1,nchar(src_files[i])-4),".csv",sep=""),
                row.names=F) 
      points_with_id<-c(substr(src_files[i],1,nchar(src_files[i])-4),quan,total_dist)
      fivePoints<-rbind(fivePoints,points_with_id)
      
    }
  }
  
  fivePoints <- fivePoints[-1,]
  colnames(fivePoints)<-c("fileID","Start(lat)","Start(lon)","Q1(lat)","Q1(lon)",
                          "Q2(lat)","Q2(lon)","Q3(lat)","Q3(lon)","End(lat)","End(lon)","Moving_dist")
  write.csv(fivePoints,paste("work/",foldername,"/","fivePoints.csv",sep=""),row.names=F)
  write.csv(subset(fivePoints,select=c("fileID","Start(lat)","Start(lon)")),
            file=paste("work/",foldername,"/startPoints.csv",sep=""),row.names=F)
  write.csv(subset(fivePoints,select=c("fileID","End(lat)","End(lon)")),
            file=paste("work/",foldername,"/endPoints.csv",sep=""),row.names=F)
  
  # CSV를 SHP로 변환하는 함수 실행. Sample/work/shp/low or high 안에 생성.
  CSV_to_SHP(foldername)
  
  # 전체 함수 끝난 시간
  time_funcend <- Sys.time()
  
  # BOAZ함수 전체 실행에 걸린 시간.
  print(paste("Elapsed Time :", time_funcend - time_funcstart, "mins",  sep=" "))
  
}

####
setwd("Sample")
BOAZ("low")
