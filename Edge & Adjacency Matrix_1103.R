library(igraph)
library(rgdal)
library(ggmap)


#step1. edge list, adjacency--------------------------------------------------------

edge<-function(folder_name){
    setwd(paste0("C:/Users/user/Desktop/빅캠_자전거프로젝트/Sample/", folder_name))
    
    start<-read.csv("startPoints.csv", sep=",", header=T, stringsAsFactors=T)
    end<-read.csv("endPoints.csv", sep=",", header=T, stringsAsFactors=T)
    
    edge_list<-merge(start,end, by="fileID", all=T) #Outer Join (Containing xy coordinates too)
    edge_list$weight<-rep(NA,nrow(edge_list))       #Add "weight column" and fill it as NA
    edge_list<-edge_list[,c(4,7,8,1,3,2,6,5)]       #열의 순서 바꾸기
                                                    #from, to, weight, file_id, from (경도,위도), to(경도,위도) 순 

    for (i in 1:nrow(edge_list)){
        edge_list[i,'Geom'] <-paste0("LINESTRING (",edge_list[i,5]," ",edge_list[i,6],", ",
                                      edge_list[i,7]," ", edge_list[i,8],")")
    }

    write.csv(edge_list,"edge_list.csv", row.names=F)
    
    G <- graph.data.frame(edge_list,directed=T)                                #엣지리스트 
    A <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE,attr=NULL)  #인접행렬
                                     
    write.csv(A, "adjacency_matrix.csv")
}


edge("test") #edge 저장됨


#R로 잘 됐나 확인-----------------------------------------------
library(igraph)
#edge list를 adjacency matrix로 변환
edge_list<-read.csv("edge_list.csv",header=T)
head(edge_list)
G <- graph.data.frame(edge_list,directed=T)
weight=get.adjacency(G,attr='weight') 

sort(degree(G, mode="out"),decreasing=T)[1:20]
sort(degree(G, mode="in"), decreasing=T)[1:20]   #degree 세는 함수 


graph.density(G)        #그래프의 집중도
vcount(G)               #노드의 개수 
V(G)$name               #노드의 이름
E(G)                    #edge 출력

plot(G, vertex.size=20, main = "Networks of Bicycle Routes")

#Adjacency Matrix
A<-read.csv("adjacency_matrix.csv", header=T, row.names =1)
dim(A)
dim(edge_list)
head(edge_list)



#Visualization of Routes-------------------------------------------------
#아직 완성안됐으므로 볼 필요 없음
gc<-geocode("Seoul")
center<-as.numeric(gc)


library(dplyr)
setwd("C:/Users/user/Desktop/빅캠_자전거프로젝트/Sample/test")

df <- data_frame(origins = edge_list$Start_adm, 
                 destinations = edge_list$End_adm)



df2 <- df %>%
    group_by(origins, destinations) %>%
    summarize(counts = n()) %>%
    ungroup() %>%
    arrange(desc(counts))

df2$origins<-as.character(df2$origins)
df2$destinations<-as.character(df2$destinations)
df2

head(edge_list)

start.xy<-subset(edge_list,select = c(Start_adm, Start.lon., Start.lat.))
names(start.xy)[1]<-"origins"
head(start.xy)
    
end.xy<-subset(edge_list, select = c(End_adm, End.lon., End.lat.))
names(end.xy)[1]<-"destinations"
head(end.xy)

df3 <- left_join(x=df2, y=start.xy)



map <- ggmap(get_googlemap(center = center, 
                           zoom=10, maptype="terrain", 
                           fullpage=T))

names(edge_list)
map + geom_path(data = edge_list, 
                aes(x = "Start.lon", y = "Star.lat",group = fileID), 
                   alpha = 0.5, size = 0.8)







