### 일부 파일 속성 개수 맞지 않는 문제



###저 전용 코드
install.packages("XML") 
library(XML)
install.packages("plyr")
library(plyr)



point <- NULL

files <- list.files(pattern = ".kml", recursive = TRUE)
for (j in unlist(files)){
  doc <- xmlTreeParse(j) #파일 불러오는 것
  rootNode <- xmlRoot(doc) #Root node를 정해줌
  
  
  ##태그 훑어보기
  #rootNode[[2]][[1]] #start time, stop time data
  #rootNode[[2]][[2]] #1번째 지점
  #rootNode[[2]][[3]] #2번째 지점
  #rootNode[[2]][[4]] #3번째 지점
  
  ##마지막 지점의 번호를 알고싶은데 위에꺼에서 계산하는 방법을 모르겠어서 기존에 쓰인 코드 활용해서 뽑음
  mm<-xmlSApply(rootNode[[1]][[1]],xmlValue) #xmlValue: tag 안에 있는 값 빼기
  head(mm$LineString, 10) #LineString 안에 좌표값있음
  lin<-gsub(' ',',',x=as.vector(mm$LineString)) #LineString을 벡터화 => gsub 함수를 통해 공백을 ','로 바뀌도록 (csv로 바꾸는 작업)
  lin<-strsplit(lin,",") #split함수로 콤마로 다 분리
  lin<-as.numeric(lin[[1]])#숫자로
  lin<-as.data.frame(matrix(lin,ncol=3,byrow=T))
  names(lin) <- c("longitude","latitude","elevation")
  num <- dim(lin)[1] #마지막 지점의 번호 = 총 지점 개수
  
  #rootNode[[2]][[num+1]] #마지막 지점(현재 데이터에선 475번째 지점)
  
  
  ##각 지점의 정보 가져오기
  #P1<-xmlSApply(rootNode[[2]][[2]],xmlAttrs)
  #P1 #1번째 지점 정보
  #P2<-xmlSApply(rootNode[[2]][[3]],xmlAttrs)
  #P2 #2번째 지점 정보
  
  #tab2 <- rbind(P1, P2)
  #tab2
  
  ##반복문
  assign
  tab1<-xmlSApply(rootNode[[2]][[2]],xmlAttrs)
  for (i in 2:num){ 
    assign(paste0("P",i), xmlSApply(rootNode[[2]][[i+1]],xmlAttrs))
    assign(paste0("tab",i), rbind(get(paste0("tab",i-1)), get(paste0("P",i))))
  }
  
  #head(tab475)
  #head(get(paste0("tab",i))) #위에랑 같은 거
  #tail(tab475)
  
  assign(paste0("df",i), data.frame(get(paste0("tab",i))))
  #head(df475)
  #head(get(paste0("df",i))) #위에랑 같은 거
  
  df_final <- get(paste0("df",i)) #최종 정보 테이블
  colnames(df_final) <- c("latitude","longitude","elevation","elevation_gain","elevation_min","elevation_max","grade","grade_min","grade_max","speed","speed_max","speed_average","speed_average_moving","direction","accuracy","total_distance","total_calorie","total_time","moving_distance","total_time_moving","activity","event_flag","save_date")
  write.table(df_final, file=paste0(j, ".csv"), row.names=F, col.names=T, sep=",") 
  
  
  ##스타트 앤딩 포인트 모으기
  p <- cbind(df_final[1,], df_final[num,]) #1번째 점(start point), 마지막 점(end point) 정보들 수집
  colnames(p) <- c("S_latitude","S_longitude","S_elevation","S_elevation_gain","S_elevation_min","S_elevation_max","S_grade","S_grade_min","S_grade_max","S_speed","S_speed_max","S_speed_average","S_speed_average_moving","S_direction","S_accuracy","S_total_distance","S_total_calorie","S_total_time","S_moving_distance","S_total_time_moving","S_activity","S_event_flag","S_save_date", 'E_latitude','E_longitude','E_elevation','E_elevation_gain','E_elevation_min','E_elevation_max','E_grade','E_grade_min','E_grade_max','E_speed','E_speed_max','E_speed_average','E_speed_average_moving','E_direction','E_accuracy','E_total_distance','E_total_calorie','E_total_time','E_moving_distance','E_total_time_moving','E_activity','E_event_flag','E_save_date')
  point <- rbind.fill(point, p) 
}

point
write.table(point, file='start_end.csv', row.names=F, col.names=T, sep=",") 
