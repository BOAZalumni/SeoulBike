### R에서 shpae 데이터로 내보내는 코드 
## 폐기 (csv로 QGIS에 load) 
## XML 패키지 참고용


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
