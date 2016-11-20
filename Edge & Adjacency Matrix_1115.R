library(igraph)

#1115 함수===================================================================================

edge<-function(folder_name){
    setwd(paste0("C:/Users/user/Desktop/빅캠_자전거프로젝트/Sample/", folder_name))
    
    #1. 필요파일 부르기
    start<-read.csv("startPoints.csv", sep=",", header=T, stringsAsFactors=F)
    end<-read.csv("endPoints.csv", sep=",", header=T, stringsAsFactors=F)
    gdong<-read.csv("gdong_sigungu.csv", header=T,stringsAsFactors = F)
    sdong<-read.csv("서울시_동.csv",header=T,stringsAsFactors=F)
    
    
    #2. edge_list 만들기
    edge_list<-merge(start,end, by="fileID") 
    edge_list<-edge_list[,c(4,7)]                       #시점, 끝점 행만 남김 
    
    
    #3. 경기도,인천의 '동'을 '시군구'단위로 바꾸기 
    for(i in 1:nrow(edge_list)){
        for (j in 1:nrow(gdong)){
            if(gdong[j,3] == edge_list[i,1]){               #출발지 경기도 동 -> 시군구
                edge_list[i,1]<-gdong[j,4]
            }
            if(gdong[j,3]== edge_list[i,2]){                #도착지 경기도 동 -> 시군구 
                edge_list[i,2]<-gdong[j,4]
            }
        }
    }
    #write.csv(edge_list, "modified_edge.csv", row.names=F)
    
    
    #4. 랜덤한 순서의 인접행렬 만들기 
    G<-graph.data.frame(edge_list, directed=T)
    random.A<-as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE,attr=NULL)
    random.A<-as.matrix(random.A)
    
    
    #5. 서울시 동 + 엣지리스트에 포함된 경기, 인천 시군구 개수 세기
    
    #출발점 
    S.uniq<-unique(edge_list[,1])
    nchar1<-nchar(S.uniq)
    endchar<-substr(S.uniq,nchar1,nchar1)           #끝문자만 추출: "동", "구" ...
    
    #끝점
    E.uniq<-unique(edge_list[,2])
    nchar2<-nchar(E.uniq)
    endchar2<-substr(E.uniq,nchar2,nchar2)
    
    #엣지리스트에 포함된 경기, 인천 시군구 개수
    gyunggi.n<-NROW(union(S.uniq[endchar!="동"],E.uniq[endchar2!="동"]))
    
    
    #6. 빈 인접행렬의 dimension 구하기 
    N<- 423+gyunggi.n                  #서울시동개수(423)+경기도 시군구 개수(gyunggi.n)
    N
    
    #7. 빈 인접행렬 만들기
    empty.A<-matrix(0,N,N)             #467*467
    
    colnames(empty.A)<-c(sdong[,3],union(S.uniq[endchar!="동"],E.uniq[endchar2!="동"]))
    rownames(empty.A)<-colnames(empty.A)
    
    #문제의 효자2동 ==> 코드 수정 필요!
    idx<-which(colnames(random.A)=="효자2동")
    random.A<-random.A[-idx,-idx]
    
    #8. 빈 인접행렬에 랜덤한 순서의 인접행렬 값 넣기 
    for (i in colnames(random.A)){
        for (j in rownames(random.A)){
            empty.A[j, i] <- random.A[j, i]
        }
    }
    
    write.csv(empty.A, "modified_adj.csv")
}

edge("test") #edge 저장됨





#구체적인 작업과정:(edge함수 하나씩 해봄)===================================================
#경기도 동, 시군구 파일===================================================================== 
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

#start, end 파일을 edge list로===============================================================
library(igraph)
setwd("C:/Users/user/Desktop/빅캠_자전거프로젝트/Sample/test")

start<-read.csv("startPoints.csv",sep=',', header=T, stringsAsFactors = F)
end <-read.csv("endPoints.csv", sep=',', header =T, stringsAsFactors = F)

edge_list<-merge(start,end, by="fileID") 
edge_list<-edge_list[,c(4,7)]
#head(edge_list,10)

#엣지리스트에 있는 경기도의 동을 '시군구'단위로 바꾸는 과정==================================
#하나 예시-----------------------------------------------------------------------------------
gdong<-read.csv("gdong_sigungu.csv", header=T,stringsAsFactors = F)

edge_list[330,]                                         #별내면: 경기도 위치한 동단위 
gdong[gdong[,3]=="별내면",]                             #경기도 동 파일에 "별내면"있는지 확인 

if(gdong[450,3]==edge_list[330,2]){
edge_list[330,2]<-gdong[450,4]}

edge_list[330,]                                         #별내면 -> 남양주시로 바뀜 

#전체 과정-----------------------------------------------------------------------------------
for(i in 1:nrow(edge_list)){
    for (j in 1:nrow(gdong)){
        if(gdong[j,3] == edge_list[i,1]){               #출발지 경기도 동 -> 시군구
             edge_list[i,1]<-gdong[j,4]
        }
        if(gdong[j,3]== edge_list[i,2]){                #도착지 경기도 동 -> 시군구 
            edge_list[i,2]<-gdong[j,4]
        }
                            }
                           }
write.csv(edge_list, "Modified Edge.csv", row.names=F)
#확인----------------------------------------------------------------------------------------
edge_list[175,]
gdong[gdong[,3]=="서현1동",]

#인접행렬 만들기=============================================================================
library(igraph)
edge_list<-read.csv("Modified Edge.csv", header=T,sep=",", stringsAsFactors = F)
G<-graph.data.frame(edge_list, directed=T)
random.A<-as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE,attr=NULL)
random.A<-as.matrix(random.A)
#dim(random.A)

#엣지리스트의 경기, 인천 시군구 개수 세기
#시점 
S.uniq<-unique(edge_list[,1])
nchar1<-nchar(S.uniq)
endchar<-substr(S.uniq,nchar1,nchar1)           #끝문자만 추출: "동", "구" ...


#끝점
E.uniq<-unique(edge_list[,2])
nchar2<-nchar(E.uniq)
endchar2<-substr(E.uniq,nchar2,nchar2)

gyunggi.n<-NROW(union(S.uniq[endchar!="동"],E.uniq[endchar2!="동"]))

#빈 인접행렬의 dimension: 서울시 423개의 동 + 엣지리스트의 수도권 시군구 개수 
N<- 423+gyunggi.n
N

#빈 인접행렬 만들기
empty.A<-matrix(0,N,N) #467*467
sdong<-read.csv("서울시_동.csv",header=T,stringsAsFactors=F)

colnames(empty.A)<-c(sdong[,3],union(S.uniq[endchar!="동"],E.uniq[endchar2!="동"]))
rownames(empty.A)<-colnames(empty.A)
#View(empty.A)

idx<-which(colnames(random.A)=="효자2동")
random.A<-random.A[-idx,-idx]

for (i in colnames(random.A)){
    for (j in rownames(random.A)){
        empty.A[j, i] <- random.A[j, i]
    }
}

View(empty.A)

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








