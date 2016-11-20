library(igraph)
library(sp)
library(rgdal)
library(plyr)

#1120 함수===================================================================================
setwd("C:/Users/user/Desktop/빅캠_자전거프로젝트/Sample/1120_Network/")

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
    start<-read.csv("startPoints.csv", header=T, sep=",", stringsAsFactors = F)
    end<-read.csv("endPoints.csv",header=T, sep=",", stringsAsFactors = F)
    gdong<-read.csv("gdong_sigungu.csv", header=T,stringsAsFactors = F)
    sdong<-read.csv("서울시_동.csv",header=T,stringsAsFactors=F)
    
    
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
    edge_list<-na.omit(edge_list)
    
    
    #5. 경기도,인천의 '동'을 '시군구'단위로 바꾸기 
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
    write.csv(edge_list, "modified_edge.csv", row.names=F)
    
    
    #6. 랜덤한 순서의 인접행렬 만들기
    G<-graph.data.frame(edge_list, directed=T)
    random.A<-as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE,attr=NULL)
    random.A<-as.matrix(random.A)
    
    
    #7. 서울시 동 + 엣지리스트에 포함된 경기, 인천 시군구 개수 세기
    #엣지리스트의 경기, 인천 시군구 개수 세기
    #시점 

    S.gyunggi<-subset(edge_list, substr(edge_list$Start_cd,1,2)==23|
                          substr(edge_list$Start_cd,1,2)==31, select = c(Start_adm,Start_cd))
    S.uniq<-unique(S.gyunggi$Start_cd)
    a<-arrange(unique(subset(S.gyunggi, Start_cd%in% S.uniq)), Start_cd)
    
    
    #끝점
    E.gyunggi<-subset(edge_list, substr(edge_list$End_cd,1,2)==23|
                          substr(edge_list$End_cd,1,2)==31, select = c(End_adm,End_cd))
    E.uniq<-unique(E.gyunggi$End_cd)
    b<-arrange(unique(subset(E.gyunggi, End_cd%in% E.uniq)), End_cd)
    
    gyunggi.n<-NROW(union(a[,1],b[,1]))
    gyunggi.sigungu<-union(a[,1],b[,1])
    
    
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
    
    write.csv(empty.A, "modified_adj.csv")
}

edge("1120_Network") #edge 저장됨





#구체적인 작업과정:(edge함수 하나씩 해봄)===================================================
#경기도 동, 시군구 파일만들기=============================================================== 
gdong<-read.csv('경기_동.csv',sep=",", header=T, stringsAsFactors = F)
gsigungu<-read.csv('경기_시군구.csv',sep=",",header=T,stringsAsFactors=F)
head(gdong)
head(gsigungu)

#하나 예시----------------------------------------------------------------------------------
substr(gdong[1,2],1,5)
grepl(gsigungu[1,2], substr(gdong[1,2],1,5))

#gdong의 동 코드 5자리와 gsigungu 코드가 같으면 시군구 이름을 gdong에 붙임------------------

for (i in 1:nrow(gdong)){
    for(j in 1:nrow(gsigungu)){
        if (grepl(gsigungu[j,2], substr(gdong[i,2], 1,5)))
            gdong[i,'sigungu_nm']<-gsigungu[j,3]
    }
}

head(gdong)
write.csv(gdong,"gdong_sigungu.csv",row.names=F) #경기도 동, 시군구파일 합침

#수도권 동 파일 붙이기  ----------------------------------------------------------------

poly_path<-"boundary/total/dong"
polylayer <- "sudogwon_dong"  # 수도권 동 폴리곤 1개만 있는 파일.
polycrs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs")
pointcrs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

sudogwon_dong<-GetPolygon(poly_path, polylayer, polycrs, pointcrs)

#출발지 동코드 붙이기 ----------------------------------------------------------------------

start<-read.csv("startPoints.csv", header=T, sep=",", stringsAsFactors = F)
start_points<-start[,1:2]

#위도, 경도 순 
colnames(start_points)<-c("latitude","longitude")
start$Start_cd<-as.numeric(as.character(GetAdmcode(start_points,sudogwon_dong)))

#도착지 동코드 붙이기 ----------------------------------------------------------------------

end<-read.csv("endPoints.csv",header=T, sep=",", stringsAsFactors = F)
end_points<-end[,1:2]

colnames(end_points)<-c("latitude","longitude")
end$End_cd<-as.numeric(as.character(GetAdmcode(end_points,sudogwon_dong)))
#head(end)



#start, end 파일을 edge list로===============================================================
library(igraph)
setwd("C:/Users/user/Desktop/빅캠_자전거프로젝트/Sample/1120_Network")

#행정 코드 붙은지 확인
head(start)
head(end)

#edge_list 생성 
edge_list<-merge(start,end, by="fileID") 

#출발 동, 출발코드, 도착동, 도착코드만 남기기 
edge_list<-edge_list[,c(4,8,5,9)]


#코드 앞의 두자리로 서울 경기 인천 외 삭제===================================================
# 서울,경기, 인천 외에는 code na 생성됨 (sudogwon_dong 코드 없는 경우니까)

#NA인 행 
edge_list[!complete.cases(edge_list),]

edge_list<-na.omit(edge_list)    #3개 없어짐

#수도권 외 지역 진짜 없어졌는지 확인--------------------------------------------------------- 
sum(edge_list[,1]=='번3동'|edge_list[,3]=='번3동')       #있어야 정상

sum(edge_list[,1]=='효자2동'|edge_list[,3]=='효자2동')   #춘천시 효자2동: 없어져야함
sum(edge_list[,1]=='서면'|edge_list[,3]=='서면')         #춘천시 서면: 없어져야함
sum(edge_list[,1]=='우천면'|edge_list[,3]=='우천면')     #춘천시 우천면: 없어져야 함

#dim(edge_list)

#엣지리스트에 있는 경기도의 동을 '시군구'단위로 바꾸는 과정==================================
#하나 예시-----------------------------------------------------------------------------------
gdong<-read.csv("gdong_sigungu.csv", header=T,stringsAsFactors = F)

edge_list[330,]                                         #별내면: 경기도 위치한 동단위 
gdong[gdong[,3]=="별내면",]                             #경기도 동 파일에 "별내면"있는지 확인 

if(edge_list[330,4]==gdong[450,2]){
edge_list[330,4]<-as.numeric(substr(edge_list([330,2],1,5))
edge_list[330,3]<-gdong[450,4]}

edge_list[330,]                                         #별내면 -> 남양주시로 바뀜 

#전체 과정-----------------------------------------------------------------------------------
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


head(edge_list,300)
write.csv(edge_list, "modified_edge.csv", row.names=F)


#인접행렬 만들기=============================================================================
library(igraph)
edge_list<-read.csv("modified_edge.csv", header=T,sep=",", stringsAsFactors = F)

G<-graph.data.frame(edge_list, directed=T)
random.A<-as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE,attr=NULL)
random.A<-as.matrix(random.A)
#dim(random.A)                  #454*454 

#엣지리스트의 경기, 인천 시군구 개수 세기
#시점 

S.gyunggi<-subset(edge_list, substr(edge_list$Start_cd,1,2)==23|
                        substr(edge_list$Start_cd,1,2)==31, select = c(Start_adm,Start_cd))
S.uniq<-unique(S.gyunggi$Start_cd)
a<-arrange(unique(subset(S.gyunggi, Start_cd%in% S.uniq)), Start_cd)



#끝점
#경기인천 고르고
E.gyunggi<-subset(edge_list, substr(edge_list$End_cd,1,2)==23|
                       substr(edge_list$End_cd,1,2)==31, select = c(End_adm,End_cd))
#유니크 코드 세고 
E.uniq<-unique(E.gyunggi$End_cd)

#유니크 동 찾기
b<-arrange(unique(subset(E.gyunggi, End_cd%in% E.uniq)), End_cd)

gyunggi.n<-NROW(union(a[,1],b[,1]))
gyunggi.sigungu<-union(a[,1],b[,1])

#빈 인접행렬의 dimension: 서울시 423개의 동 + 엣지리스트의 수도권 시군구 개수 
N<- 423+gyunggi.n
N

#빈 인접행렬 만들기
empty.A<-matrix(0,N,N) #461*461
sdong<-read.csv("서울시_동.csv",header=T,stringsAsFactors=F)

colnames(empty.A)<-c(sdong[,3],gyunggi.sigungu)
rownames(empty.A)<-colnames(empty.A)
View(empty.A)


for (i in colnames(random.A)){
    for (j in rownames(random.A)){
        empty.A[j, i] <- random.A[j, i]
    }
}

View(empty.A)

#1120 끝==================================================================================
#참고: 주경언니 코드----------------------------------------------------------------------
a=matrix(c(1:15),5,5) #실제 경로 연산 결과
colnames(a)<-c("가","나","다","라","마")
rownames(a)<-c("가","나","다","라","마")

b=matrix(0,7,7) #빈 메트릭스
colnames(b)<-sample(c("가","까","나","다","ㄸ","라","마"))
rownames(b)<-colnames(b)

a
b
for (i in colnames(a)){
    for (j in rownames(a))
        b[j, i] <- a[j, i]
}


#인접중심도 계산============================================================================
sort(degree(G, mode="out"),decreasing=T)[1:20]
sort(degree(G, mode="in"), decreasing=T)[1:20]   #degree 세는 함수 


graph.density(G)        #그래프의 집중도
vcount(G)               #노드의 개수 
V(G)$name               #노드의 이름
E(G)                    #edge 출력

plot(G, vertex.size=20, main = "Networks of Bicycle Routes")

#Adjacency Matrix
A<-read.csv("adjacency_matrix.csv", header=T, row.names =1)

#출발지 degree
out.D<-rowSums(A)

#도착지 degree
in.D<-colSums(A)


A$out.d<-out.D
A$in.d<-in.D

degree<-A[,-c(1:654)]
