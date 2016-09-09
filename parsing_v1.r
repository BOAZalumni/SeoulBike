
### 성훈, 태영 초기 버전

install.packages("XML") 
library(XML)
doc <- xmlTreeParse('mm.kml')
rootNode <- xmlRoot(doc)
mm<-xmlSApply(rootNode[[1]][[1]],xmlValue) 
lin<-gsub(' ',',',x=as.vector(mm$LineString))
lin<-strsplit(lin,",")
lin<-as.numeric(lin[[1]])
lin<-as.data.frame(matrix(lin,ncol=3,byrow=T))
names(lin) <- c("longitude","latitude","elevation")


myline<-Line(lin[,1:2])
linestring<-Lines(list(myline), ID = "a")
sl<-SpatialLines(list(linestring))
sldf<-SpatialLinesDataFrame(sl, data.frame(Z="road", row.names="a"))
writeOGR(sldf, ".", "data", "ESRI Shapefile")
