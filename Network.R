####ReadMe####
#startPoints.csv와 endPoints.csv의 경우는 따로 뽑아야 함.
#코드 실행전, 경로에 'FlowMapper_1124'폴더 안에 있는 모든 파일이 있어야함!!!
#setwd()확인
#윈도우와 맥의 호환성을 위해 인코딩은 "EUC-KR"로 통일시킴.
#함수 실행 후 해당 경로에 생기는 파일은 총 4개.
#modified_edge : 출도착경로와 행정코드
#modified_adj : 인접행렬
#node_coordinates.txt : 인접행렬 순의 x y 좌표(Centroid)
#flow_mat.txt : "modified_adj"에서 변수명을 제거한 txt파일.


library(igraph)
library(sp)
library(rgdal)

#1120 함수===================================================================================

setwd("/Users/Dowon/Desktop/BikeProject/07_qgis/1120_Network/")

#Fun1: 전체 폴리곤 읽어오는 함수=============================================================

GetPolygon <- function(folderpath, layername, polygondatacrs=NULL, exp_crs=NULL)
{
  # 전체 경계 파일을 Sample폴더 안에 boundary라는 폴더를 만들어서 위치시켜야 함.
  # 전체 경계 파일(Polygon형태의 shp파일) 읽기를 먼저 실행하고 함수에 입력.
  sp_polygon <- readOGR(dsn = folderpath, layer = layername)
  
  if(is.na(proj4string(sp_polygon)) & !is.null(polygondatacrs))
    proj4string(sp_polygon) <- polygondatacrs
  
  # 전체 경계 파일의 좌표계를 경로 데이터들과 같게 변환.(EPSG 4326으로)
  if(!is.na(proj4string(sp_polygon)) & !is.null(exp_crs))
    sp_polygon <- spTransform(sp_polygon, exp_crs)
  
  return(sp_polygon)
}

#Fun2: 행정 코드 추출 함수===================================================================
#GetAdmname과 다른점: poinstattr[,3]대신 [,2] 로 읽어 동코드 읽음 

GetAdmcode <- function(sp_points, adm_polygon, 
                       movedatacrs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
{
  fileclass <- class(sp_points)
  # 경로 데이터를 어떤 데이터 형식으로 받냐에 따라 다르게 처리. 
  #SpatialPoints 타입으로 모두 변환해줌.
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
  
  # 각 점의 동코드 위치하는지 붙이기.
  pointsattr <- over(sp_points, adm_polygon)
  # shp파일로 만들어서 qgis에서 불러올 시 동코드가 속성 테이블에 붙음.
  sp_points$cd <- pointsattr[,2] # 어떤 polygon파일을 불러오냐에 따라 동코드 부분 달라짐.
  
  
}


#Fun3: Edge List 함수=========================================================================
edge<-function(folder_name){
  
  #1. 필요파일 부르기
  start<-read.csv("startPoints.csv",
                  header=T,
                  sep=",",
                  stringsAsFactors = F,
                  fileEncoding = "EUC-KR")
  end<-read.csv("endPoints.csv",
                header=T,
                sep=",",
                stringsAsFactors = F,
                fileEncoding = "EUC-KR")
  gdong<-read.csv("gdong_sigungu.csv",
                  header=T,
                  stringsAsFactors = F,
                  fileEncoding = "EUC-KR")
  sdong<-read.csv("서울시_동.csv",
                  header=T,
                  stringsAsFactors=F,
                  fileEncoding = "EUC-KR")
  ds_centroid<-read.csv("dong_sigungu.csv",
                        header=T,
                        stringsAsFactors=F,
                        fileEncoding = "EUC-KR")
  
  
  #2. 수도권 폴리곤 만들기 
  poly_path<-"boundary/total/dong"
  polylayer <- "sudogwon_dong"  # 수도권 동 폴리곤 1개만 있는 파일.
  polycrs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs")
  pointcrs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  sudogwon_dong<-GetPolygon(poly_path, polylayer, polycrs, pointcrs)
  
  
  #3. start, end 파일에 행정코드 붙이기 
  #출발지 행정코드 Start_cd
  start_points<-start[,1:2]
  colnames(start_points)<-c("latitude","longitude")
  start$Start_cd<-as.numeric(as.character(GetAdmcode(start_points,sudogwon_dong)))
  #factor를 chr로, chr를 num으로 바꿔줘야함 
  
  #도착지 행정코드 End_cd
  end_points<-end[,1:2]
  colnames(end_points)<-c("latitude","longitude")
  end$End_cd<-as.numeric(as.character(GetAdmcode(end_points,sudogwon_dong)))
  
  
  #4. edge_list 만들기
  edge_list<-merge(start,end, by="fileID") 
  edge_list<-edge_list[,c(4,8,5,9)]                       #출발동, 도착동, 출발코드, 도착코드 
  edge_list<-na.omit(edge_list)                           #수도권동 폴리곤과 조인했으므로 NA 값은 수도권이 아닌 동임!
  #na.omit으로 수도권만 남김
  
  #5. 경기도,인천의 '동'을 '시군구'단위로 바꾸기_ #5 반복문 대략 5~10분 시간 소요  
  for(i in 1:nrow(edge_list)){
    for (j in 1:nrow(gdong)){
      if(edge_list[i,3] == gdong[j,2]){               #출발지동 코드 == 경기도 동코드
        edge_list[i,3]<-as.numeric(substr(edge_list[i,3],1,5)) #경기도동코드 -> 시군구코드
        edge_list[i,1]<-gdong[j,4]                             #경기도동이름 -> 시군구이름
      }
      
      if(edge_list[i,4]== gdong[j,2]){                #도착지동 코드 == 경기도 동코드 
        edge_list[i,4]<-as.numeric(substr(edge_list[i,4],1,5)) #경기도동코드 -> 시군구코드
        edge_list[i,2]<-gdong[j,4]                             #경기도동이름 -> 시군구이름  
        
      }
    }
  } 
  write.csv(edge_list,
            "modified_edge.csv",
            row.names=F,
            fileEncoding = "EUC-KR") #출발동_도착동_출발행코_도착행코
  
  
  #6. 랜덤한 순서의 인접행렬 만들기
  G<-graph.data.frame(edge_list, directed=T)
  random.A<-as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE,attr=NULL)
  random.A<-as.matrix(random.A)
  
  
  #7. 서울시 동 + 엣지리스트에 포함된 경기, 인천 시군구 개수 세기
  #시점 
  #엣지리스트의 경기, 인천 코드 자릿수는 5개이므로 nchar = 5인 행정 코드만 선택
  S.gyunggi<-subset(edge_list, nchar(Start_cd)==5, select = Start_cd)
  S.uniq<-unique(S.gyunggi$Start_cd)                                  #유니크한 코드_36개의 시군구
  
  #끝점
  E.gyunggi<-subset(edge_list, nchar(End_cd)==5, select = End_cd)
  E.uniq<-unique(E.gyunggi$End_cd)                                    #유니크한 코드_35개의 시군구
  
  #경기, 인천 시군구 개수 gyunggi.n
  gyunggi.n<-NROW(union(S.uniq,E.uniq)) #출발 도착 합해서 총 38개의 시군구
  
  #출발지, 도착지 코드 정렬
  sorted.code<-sort(union(S.uniq, E.uniq))
  
  #정렬된 코드 sorted.code로 gdong 파일의 시군구명 뽑기
  sigungu_cd<-as.numeric(substr(gdong$adm_dr_cd,1,5))     #gdong파일의 코드를 5자리로 바꾸고
  gyunggi.sigungu<-unique(subset(gdong[,4],sigungu_cd%in%sorted.code)) #sorted.code와 같은 코드로 시군구명 뽑음 
  
  
  #8. 빈 인접행렬의 dimension 구하기 
  N<- 423+gyunggi.n                  #서울시동개수(423)+경기도 시군구 개수(gyunggi.n)
  N
  
  
  #9. 빈 인접행렬 만들기
  empty.A<-matrix(0,N,N)             #461*461
  
  colnames(empty.A)<-c(sdong[,3],gyunggi.sigungu)
  rownames(empty.A)<-colnames(empty.A)
  
  
  #10. 빈 인접행렬에 랜덤한 순서의 인접행렬 값 넣기 
  for (i in colnames(random.A)){
    for (j in rownames(random.A)){
      empty.A[j, i] <- random.A[j, i]
    }
  }
  write.csv(empty.A, "modified_adj.csv", fileEncoding = "EUC-KR")
  
  #11. node_coordinates.txt
  all.code <- c(sdong[,2], sorted.code)
  all.code <- as.data.frame(all.code)
  colnames(all.code) <- "node_cd"
  merge <- merge(all.code, ds_centroid, by="node_cd", all.x=T)
  merge <- merge[,4:5]
  
  write.table(merge, "node_coordinates.txt", row.names = FALSE, col.names = FALSE, sep=" ")
  write.table(empty.A, "flow_mat.txt",row.names = FALSE, col.names = FALSE, sep="\t", quote = FALSE)
}

edge("1120_Network") #edge 저장됨
