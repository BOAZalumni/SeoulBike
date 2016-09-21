#### XML -> CSV tutorial
### 사전작업
  # 1. "고","저","_|파일" 폴더가 들어있는 폴더명을 "Sample"로 한다.
  # 2. "고"폴더의 이름을 "high"로, "저"폴더의 이름을 "low"로 한다. (혹시 모를 한글 오류를 위해)
  # 3. "Sample"폴더 안에 "work" 폴더를 만들고 그 안에는 또 "high"와 "low"폴더를 만든다.
  # 4. 아래 코드의 3 line setwd()안의 내용을 본인 PC의 "Sample" 폴더 경로로 고친다.
  # 5. 아래의 코드를 통째로 실행시킨다.
  # 6. 만들어진 함수의 인자에 샘플 데이터를 꺼낼 폴더명을 넣는다.
      # ex) boazXML("low") or boazXML("high")
      # 주의 1. "_|파일" 폴더는 고려하지 않고 만들었으므로 실행하지 말 것.
      # 주의 2.  high로 돌리면 매우 오래걸림.


###
library(XML)
boaxXML<-function(foldername){
  setwd("/Users/Martha/Desktop/Sample")   #샘플 폴더 경로 지정
  src_dir<- foldername                    # Sample 폴더 중 지정한 폴더(foldername) 경로 
  src_files<-list.files(src_dir)          # 경로안의 파일명 리스트 
  file_count <-length(src_files)          # 파일의 개수 카운트 
  
  for( i in 1:file_count){                #파일을 하나씩 Loop
    
    doc <- xmlTreeParse(paste(src_dir, "/", src_files[i], sep=""))   #XML 파일을 하나씩 가져옴 
    rootNode <- xmlRoot(doc)  # 루트노드 지정
    nrows<-length(rootNode[[2]])-1 # 루트노드의 2번째 줄에서 마지막줄까지 읽을 것이므로 길이에서 -1 
    ncols<-length(rootNode[[2]][[2]]) # 각 좌표별 변수의 개수
    path.mat<-matrix(data=NA,nrow=nrows,ncol=ncols) # 변수의 개수를 열, 길이-1을 행으로 하는 행렬생성
    colnames(path.mat)<-names(rootNode[[2]][[2]]) # 행렬의 colname은 각 변수명으로
    
        for (j in 2:length(rootNode[[2]])){  # 2번째 노드부터 마지막까지 Loop
           a<-as.numeric(xmlSApply(rootNode[[2]][[j]],xmlAttrs)) # 각 좌표별 값을 읽어서 numeric
           path.mat[j-1,]<-a # 위에서 만든 행렬의 첫번째 행부터 순서대로 읽은 값을 할당
            }
    
    write.csv(path.mat,  
              file=paste("work/",foldername,"/",
                         substr(src_files[i],1,nchar(src_files[i])-4),".csv",sep=""),
              row.names=F) #.kml은 .csv로 바꾸고 경로 앞에 미리 만든 work 폴더의 경로를 붙여서 쓰기
  }
}

