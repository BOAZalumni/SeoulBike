####
# 네트워크 분석.
read.csv("endpoints.csv")

network <- function(foldername)
{
  start <- read.csv(paste("work" ,foldername ,"startPoints.csv", sep="/"))
  end <- read.csv(paste("work" ,foldername ,"endPoints.csv", sep="/"))
  
  start <- start[,3]
  end <- end[,3]
  
  edge_list <- data.frame(matrix(nrow=length(start),ncol=2))
  names(edge_list) <- c("start", "end")
  edge_list[,1] <- start
  edge_list[,2] <- end
  
  G <- graph.data.frame(edge_list,directed=T)
  A <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE,attr="weight")
  adj.mat <- get.adjacency(G, type="both", attr="weignt")
  
  return(adj.mat)
}