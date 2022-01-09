setwd("C:/Users/st4ndup/Desktop/탐자프로젝트")
library(dplyr)
#####
#지하철 승하차인원 데이터 로드
seosub=read.csv("서울교통공사 2019년 일별 역별 시간대별 승하차인원(1_8호선).csv")
seosub$날짜=as.character(seosub$날짜)
head(seosub)
#역명이 간결하지 못해 역번호 이용
seosub=select(seosub,c(-역명,-합.계))
str(seosub)

loctel=read.csv("역주소및전화번호.csv")
numname=read.csv("서울특별시 노선별 지하철역 정보(신규).csv")
names(loctel)[2]="전철역명"

levels(loctel$전철역명)
levels(numname$전철역명)

loctel=loctel[,c(2,5)]
name_loc_num_subway=unique(merge(loctel,numname,by="전철역명")[,c(1:3,5)])
levels(name_loc_num_subway$전철역명)

#name_loc_num_subway : 역번호-역명-역주소 data

#서울시인 것만 추출 후 구 이름으로 변환
name_loc_num_subway$주소=as.character(name_loc_num_subway$주소)
name_loc_num_subway$전철역코드=as.numeric(as.character(name_loc_num_subway$전철역코드))

ll=length(name_loc_num_subway$주소)
obsseoul=c()
# 서울소재 지하철역 아닌 경우 제외
for(i in 1:ll){
  if (substr(name_loc_num_subway$주소[i],1,5)=="서울특별시"){
    obsseoul=c(obsseoul,i)
  }
}
name_loc_num_subway[obsseoul,]
# 구 명으로 갱신 (서울이 아닌 경우에는 경기도-"00시")
for(i in 1:ll){
  name_loc_num_subway$주소[i]=strsplit(name_loc_num_subway$주소[i], ' ')[[1]][2]
}
names(name_loc_num_subway)[3]="역번호"

#주소 추가
seosub=merge(name_loc_num_subway,seosub,by="역번호")

#####
#####서울시의 도시 클러스터링

###
#녹지면적
seo_green=read.table("서울시구별녹지면적.txt")

#녹지면적은 구당 총 면적 이용, 녹지비율로 전환
seo_green=select(seo_green,c(V2,V4))
names(seo_green)=c("구","녹지면적")
seo_green$구=as.character(seo_green$구)
seo_green$녹지면적=as.numeric(as.character(gsub(',','',seo_green$녹지면적)))

#비율을 구하기 위한 총 면적 
seo_area=matrix(c('서초',	47.03
,'강서',	41.43
,'강남',	39.54
,'노원',	35.42
,'송파',	33.88
,'은평',	29.71
,'관악',	29.57
,'강동',	24.58
,'성북',	24.57
,'영등포',	24.57
,'종로',	23.91
,'마포',	23.88
,'강북',	23.60
,'용산',	21.87
,'도봉',	20.70
,'구로',	20.11
,'중랑',	18.50
,'서대문',	17.61
,'양천',	17.40
,'광진',	17.05
,'성동',	16.85
,'동작',	16.35
,'동대문',	14.21
,'금천',	13.01
,'중구',	9.96), nrow=2)
seo_area=as.data.frame(t(seo_area))
names(seo_area)=c("구","면적")
seo_area$구=as.character(seo_area$구)
seo_area$면적=as.numeric(as.character(seo_area$면적))

#merge하여 결합
for(i in 1:25){seo_area$구[i]=paste0(seo_area$구[i],"구")}
seo_area$구[25]="중구"
seo_green=merge(seo_green,seo_area)
str(seo_green)
seo_green$면적당녹지=seo_green$녹지면적/seo_green$면적

#표준화 시켜주기
seo_green$면적당녹지=scale(seo_green$면적당녹지)
seo_green$면적=scale(seo_green$면적)
seo_green=select(seo_green,c("구","면적","면적당녹지"))

###
#구별 연령 주민수
seo_age=read.table("서울시분기연령별주민수.txt")
seo_age=seo_age[seo_age$V3=='계'&seo_age$V2!='합계',]

#65세 이상 인구의 비율을 통해 고령화를 측정
seo_age=select(seo_age,c('V1','V2','V4','V18','V19','V20','V21'
                 ,'V22','V23','V24','V25'))
rownames(seo_age)=NULL
for(i in 3:11){seo_age[,i]=gsub(',','',seo_age[,i])}
names(seo_age)[1:3]=c("분기","구","인구계")
for(i in 3:11){seo_age[,i]=as.numeric(as.character(seo_age[,i]))}
seo_age$고령화=apply(seo_age[,4:11],1,sum)
seo_age=select(seo_age,c("분기","구","인구계","고령화"))
seo_age$고령화=seo_age$고령화/seo_age$인구계

#2019년 평균으로 요약 및 표준화
seo_age=group_by(seo_age, 구) %>% 
  summarise(인구계=mean(인구계),고령화=mean(고령화))
seo_age=as.data.frame(seo_age)
seo_age$인구계=scale(seo_age$인구계)
seo_age$고령화=scale(seo_age$고령화)

###
#구별 사업체 및 종사자
seo_job=read.table("서울시산업체및종사자수.txt")
seo_job=seo_job[,2:5]
seo_job=seo_job[seo_job$V3=="소계",-2];rownames(seo_job)=NULL
names(seo_job)=c("구","사업체","종사자")
seo_job[,2]=as.numeric(as.character(gsub(',','',seo_job[,2])))
seo_job[,3]=as.numeric(as.character(gsub(',','',seo_job[,3])))
seo_job$사업체=scale(seo_job$사업체)
seo_job$종사자=scale(seo_job$종사자)


###
#구별 쇼핑몰
seo_mall=read.table("서울시유통업체별면적.txt")
seo_mall=seo_mall[-1,-1];rownames(seo_mall)=NULL

#구, 개소, 연면적만 남기고 전처리
seo_mall=select(seo_mall,c("V2","V3","V5"))
seo_mall[,2]=as.numeric(as.character(gsub(',','',seo_mall[,2])))
seo_mall[,3]=as.numeric(as.character(gsub(',','',seo_mall[,3])))
names(seo_mall)=c("구","쇼핑몰수","쇼핑연면적")
seo_mall$쇼핑몰수=scale(seo_mall$쇼핑몰수)
seo_mall$쇼핑연면적=scale(seo_mall$쇼핑연면적)


###
#구별 학교수, 학생수
seo_sch=read.table("서울시학교별구별학생수.txt")
seo_sch=seo_sch[-1,-1];rownames(seo_sch)=NULL
for(i in 2:10){seo_sch[,i]=gsub(',','',seo_sch[,i])}
for(i in 2:10){seo_sch[,i]=as.numeric(as.character(seo_sch[,i]))}
seo_sch$학교수=seo_sch$V3+seo_sch$V6+seo_sch$V9
seo_sch$학생수=seo_sch$V4+seo_sch$V7+seo_sch$V10
seo_sch=select(seo_sch,c('V2','학교수','학생수'))
names(seo_sch)[1]="구"
seo_sch$학교수=scale(seo_sch$학교수)
seo_sch$학생수=scale(seo_sch$학생수)


###
#병원자료에서 구별 병상수(의료기관의 규모)
seo_hosp=read.table("서울시병원별구별병상수.txt")
seo_hosp=seo_hosp[-1,-1];rownames(seo_hosp)=NULL
seo_hosp=seo_hosp[,c(1,3)]
seo_hosp$V4=as.numeric(as.character(gsub(',','',seo_hosp$V4)))
names(seo_hosp)=c("구","의료규모")
seo_hosp$의료규모=scale(seo_hosp$의료규모)


###
#총집: seo_city
seo_city=merge(seo_age,seo_green)%>%
  merge(seo_hosp)%>%
  merge(seo_job)%>%
  merge(seo_mall)%>%
  merge(seo_sch)
seo_city$구=as.character(seo_city$구)

#h clustering
clus=as.dendrogram(hclust(dist(seo_city[,-1])))
plot(clus)

#그룹 분류
#rect.hclust(hclust(dist(seo_city[,-1])), k=3, border="red")
#rect.hclust(hclust(dist(seo_city[,-1])), k=4, border="red")
rect.hclust(hclust(dist(seo_city[,-1])), k=5, border="red")
#rect.hclust(hclust(dist(seo_city[,-1])), k=6, border="red")

#k=5로 분류하는 것이 맞다고 판단함.
#4,9
#3,23,12,10,14,5,17,22
#6,16,8,21,13,19,7,20,2,11,25
#24
#18,1,15

seo_city$구명=seo_city$구
seo_city$구[c(4,9)]='A'
seo_city$구[c(3,23,12,10,14,5,17,22)]='B'
seo_city$구[c(6,16,8,21,13,19,7,20,2,11,25)]='C'
seo_city$구[c(24)]='D'
seo_city$구[c(18,1,15)]='E'
# 강서구 노원구
# 강북구 종로구 동작구 도봉구 서대문구 관악구 성북구 은평구  
# 광진구 성동구 금천구 용산구 마포구 양천구 구로구 영등포구 강동구 동대문구 중랑구
# 중구
# 송파구 강남구 서초구
seo_city$구=as.factor(seo_city$구)
seo_city_hc=seo_city[,c(1,13)]

#이후 주소 변환 편리를 위해서
loc_to_hc=as.data.frame(
cbind(
c('강서구','노원구'
,'강북구','종로구','동작구','도봉구','서대문구','관악구','성북구','은평구'
,'광진구','성동구','금천구','용산구','마포구','양천구','구로구','영등포구','강동구','동대문구'
,'중랑구','중구','송파구','강남구','서초구'),
c(rep('A',2),
  rep('B',8),
  rep('C',11),
  rep('D',1),
  rep('E',3))
))
names(loc_to_hc)=c('loc','loc_hc')

######
######

#날짜, 휴일, 요일
date=cbind(c(
  seq(20190101,20190131),
  seq(20190201,20190228),
  seq(20190301,20190331),
  seq(20190401,20190430),
  seq(20190501,20190531),
  seq(20190601,20190630),
  seq(20190701,20190731),
  seq(20190801,20190831),
  seq(20190901,20190930),
  seq(20191001,20191031),
  seq(20191101,20191130),
  seq(20191201,20191231)
),rep(c(2,3,4,5,6,7,1),53)[1:365]
)
date=as.data.frame(date);names(date)=c('date','dow')

date$holiday=rep(0,365)
for(i in 1:365){if(date$date[i]=='20190101'
   |date$date[i]=='20190204'
   |date$date[i]=='20190205'
   |date$date[i]=='20190206'
   |date$date[i]=='20190301'
   |date$date[i]=='20190505'
   |date$date[i]=='20190506'
   |date$date[i]=='20190512'
   |date$date[i]=='20190606'
   |date$date[i]=='20190815'
   |date$date[i]=='20190912'
   |date$date[i]=='20190913'
   |date$date[i]=='20190914'
   |date$date[i]=='20191003'
   |date$date[i]=='20191009'
   |date$date[i]=='20191225'){date$holiday[i]=1}
}
str(date)
#범주형 처리
date$dow=factor(date$dow)
date$holiday=factor(date$holiday)
date$date=factor(date$date)

#name_loc_num_subway : 지하철 호선, 주소
names(name_loc_num_subway)[2]='loc'
name_loc_num_subway_2=merge(name_loc_num_subway,loc_to_hc, by='loc')

#seosub : 지하철 인원
seosub2=inner_join(seosub, name_loc_num_subway_2, by="역번호")
seosub3=unique(seosub2)
names(seosub3)
seosub3=seosub3[,c(5,2,4,31,7:27)]
names(seosub3)=c("date","station",
                 "line","loc_hc","info",
                 5:24)

seosub3=merge(seosub3,date,by="date")
names(seosub3)


seosub3$line=as.character(seosub3$line)
#호선 숫자로 변환
for(i in 1:8){
  seosub3$line[seosub3$line==paste0('0',i,'호선')]=i
}
seosub3$line=as.factor(seosub3$line)
#분기(season) : 1,2,3,4
for(i in 1:nrow(seosub3)){
  seosub3$season[i]=substr(seosub3$date[i],5,6)
}
seosub3$season=as.numeric(seosub3$season)
seosub3$season[seosub3$season==1|seosub3$season==2|seosub3$season==3]="1"
seosub3$season[seosub3$season==4|seosub3$season==5|seosub3$season==6]="2"
seosub3$season[seosub3$season==7|seosub3$season==8|seosub3$season==9]="3"
seosub3$season[seosub3$season==10|seosub3$season==11|seosub3$season==12]="4"
seosub3$season=as.factor(seosub3$season)
#날짜 범주화 : 1,2,3
for(i in 1:nrow(seosub3)){
  seosub3$date[i]=substr(seosub3$date[i],7,8)
}
seosub3$date=as.numeric(seosub3$date)
seosub3$date[seosub3$date<=10]="1"
seosub3$date[seosub3$date>10&seosub3$date<=20]="2"
seosub3$date[seosub3$date>20]="3"
seosub3$date=as.factor(seosub3$date)

#변수 정리
names(seosub3)
seosub3=seosub3[,c(28,1,26,27,2:25)]

#########201129 : 모델링 후 정확도가 다소 불만족스러워 역 정보 추가#########

subwayinfo=read.csv('4576_역사 현황(층수,길이,형식,면적,준공연도)(건축처).csv',header=T)

names(subwayinfo)=c('line2', 'station', 'type', 'length',
                    'floor', 'area', 'sta_age')

subwayinfo=subwayinfo[,c(1:2,4:7)]
subwayinfo$line2=as.numeric(subwayinfo$line2)
subwayinfo$floor=as.numeric(subwayinfo$floor)
subwayinfo$length=as.numeric(subwayinfo$length)
subwayinfo$area=as.numeric((gsub(',','',subwayinfo$area)))
#몇년 된 역인지 설명 : sta_age
subwayinfo$sta_age=2019-as.numeric(subwayinfo$sta_age)

head(subwayinfo)
#merge
head(seosub3)
seosub3=merge(seosub3,subwayinfo, by='station')
#호선 일치하는 행만 남기기
seosub3=seosub3[seosub3$line==seosub3$line2,]
names(seosub3)
seosub3=seosub3[,c(1:6,30:33,7:28)]
rownames(seosub3)=NULL
#적용 이후 rmse가 눈에 띄게 개선되었음
############################################################################

#xgboost 모델링
library(xgboost)
library(Metrics)

seosub_h=list()
for (i in 5:24){
  seosub_h[[i]]=seosub3[,c(2:12,i+8)]
  names(seosub_h[[i]])[12]='y'
}

#80:20 train/vaildation
set.seed(1111)
samplenum=sample(nrow(seosub3),0.80*nrow(seosub3))
seosub_train=list()
seosub_valid=list()
for (i in 5:24){seosub_train[[i]]=seosub_h[[i]][samplenum,]}
for (i in 5:24){seosub_valid[[i]]=seosub_h[[i]][-samplenum,]}

yhat=NULL

#hyperparameter 조정을 위해 함수 xgbst 작성
xgbst=function(nrd,maxdep,eta,colsam,subsam){
  set.seed(1111)
  for (i in 5:24){
    y=as.matrix(seosub_train[[i]]$y)
    x=select(seosub_train[[i]],-c('y'))
    x=sapply(x,as.numeric)
    fit=xgboost(data=x,label=y, booster='gbtree', nthread=4,
                nrounds=nrd, metrics='rmse', eta=eta,
                colsample_bytree=colsam, subsample=subsam, 
                max_depth=maxdep, early_stopping_round = 3)
    yhat=cbind(yhat,predict(fit,sapply(select(seosub_valid[[i]],-c('y')),as.numeric)))
  };return(yhat)
}

#valid의 y값 matrix : answer 생성
answer=NULL
for(i in 5:24){
  answer=cbind(answer,seosub_valid[[i]]$y)
}

#파라미터 튜닝
#xgbst(nrd,maxdep,eta,colsam,subsam)

#모든 범위에 대하여 하이퍼파라미터를 튜닝하면 좋겠지만
#성능과 시간의 한계가 있으므로 xgboost를 활용한 연구의 파라미터를 참고하여
#범위를 정해서 결정해보도록 한다.
#오재영, 함도현, 이용건, 김기백. (2019). XGBoost 기법을 이용한 단기 전력 수요 예측 및 하이퍼파라미터 변화에 따른 영향 분석. 전기학회논문지, 68(9), 1073-1078.
#최성현, 허진. (2020). 최적화 하이퍼 파라미터의 XGBoost 학습자 기반 배깅 모델을 활용한 태양광 출력 예측. 전기학회논문지, 69(7), 978-984.

#0. nrd는 early stopping option이 존재하므로 과적합 걱정은 없으나
# for 문을 돌리기에 시간이 오래걸리므로 200정도로 주고 상대적인 rmse 값의 높고 낮음만 테스트 해보고자 한다.
# xg_out=xgbst(200,maxdep,eta,colsam,subsam)

#1. max_depth는 3에서 10 사이의 값이 사용되나, 5~10 사이의 값에서 성능이 좋음
#2. eta는 학습률으로 일반적으로 0.01~0.3 사이의 값을 사용한다고 함.
maxdep=5:10
eta=seq(0.01,0.3,length=6)

#3. colsample_bytree는 모형에 사용될 변수 개수로 보통 0.8주변의 값을 사용함
# 참고 논문 등에서 보통 그리드서치 이용
colsam=seq(0.7,0.9,0.1)

#4. subsample은 모형에 사용될 임의 표본 개수로 보통 0.8주변의 값을 사용함
# 참고 논문 등에서 보통 그리드서치 이용
subsam=seq(0.7,0.9,0.1)

#테스팅 하지 못한 다른 파라미터는 기본 값으로 세팅
#1. 과 2. 끼리 그리드서치
#최적화된 1.과 2.를 통해 3. 과 4. 그리드서치

RMSE=matrix(rep(NA,36),6)
rownames(RMSE)=c('dep=5','dep=6','dep=7','dep=8','dep=9','dep=10')
colnames(RMSE)=c('eta1','eta2','eta3','eta4','eta5','eta6')

# max_depth와 eta
#for(i in 1:6){
#  for(j in 1:6){
#    xg_out=xgbst(200,maxdep[i],eta[j],0.8,0.8)
#    RMSE[i,j]=rmse(answer,xg_out)
#  }
#};RMSE

#         eta1     eta2     eta3     eta4     eta5     eta6
#dep=5  766.8984 433.5284 341.9281 288.3442 259.8431 241.4846
#dep=6  681.1741 336.9783 254.9563 218.0428 201.6432 193.2813
#dep=7  601.0812 260.6958 203.1127 182.3551 174.7474 171.7219
#dep=8  527.4042 210.4234 177.0270 168.4458 166.8388 167.0936
#dep=9  468.7369 182.4404 166.1763 165.1841 166.3564 167.7683
#dep=10 426.2839 169.3113 164.9447 166.6242 168.7852 170.8534

#sort(RMSE) : dep=10,eta=0.126 | dep=9,eta=0.126 | dep=9,eta=0.184  
#                       dep=9,eta=0.242 | dep=10,eta=0.184 

md_et=cbind(c(10,9,9,9,10),c(0.126,0.126,0.184,0.242,0.184))
RMSE=matrix(rep(NA,9),3)
colnames(RMSE)=c('col=0.7','col=0.8','col=0.9')
rownames(RMSE)=c('sub=0.7','sub=0.8','sub=0.9')
RMSE2=list(RMSE,RMSE,RMSE,RMSE,RMSE)
names(RMSE2)=c('md_et1','md_et2','md_et3','md_et4','md_et5')

# colsam와 subsam
#for(k in 1:5){
#for(i in 1:3){
#  for(j in 1:3){
#    xg_out=xgbst(200,md_et[k,1],md_et[k,2],colsam[i],subsam[j])
#    RMSE2[[k]][j,i]=rmse(answer,xg_out)
#  }
#}};RMSE2
#$md_et1
#         col=0.7  col=0.8  col=0.9
#sub=0.7 166.6013 165.3673 166.1022
#sub=0.8 166.6816 164.9447 165.5732
#sub=0.9 166.0367 163.9425 164.6150
#$md_et2
#         col=0.7  col=0.8  col=0.9
#sub=0.7 172.7099 166.7824 164.8601
#sub=0.8 172.6810 166.1763 164.2489
#sub=0.9 172.8467 166.2229 163.6454
#$md_et3
#         col=0.7  col=0.8  col=0.9
#sub=0.7 168.1246 165.9444 166.0370
#sub=0.8 167.4539 165.1841 165.2431
#sub=0.9 166.9137 164.1043 164.3657
#$md_et4
#         col=0.7  col=0.8  col=0.9
#sub=0.7 167.3936 167.0604 168.5438
#sub=0.8 166.5023 166.3564 167.0692
#sub=0.9 165.8383 165.3331 166.2184
#$md_et5
#         col=0.7  col=0.8  col=0.9
#sub=0.7 166.4101 167.1056 169.2822
#sub=0.8 165.8599 166.6242 168.6208
#sub=0.9 165.2463 165.7029 167.5583

# max_depth=9 , eta=0.126, colsam=0.9, subsam=0.9 일때 best이다.

# 본 모델링이므로 nrounds는 10000 으로 충분히 준다.
fit_list=list()
yhat=NULL
for (i in 5:24){
  y=as.matrix(seosub_train[[i]]$y)
  x=select(seosub_train[[i]],-c('y'))
  x=sapply(x,as.numeric)
  fit=xgboost(data=x,label=y, booster='gbtree', nthread=4,
              nrounds=10000, metrics='rmse', eta=0.126,
              colsample_bytree=0.9, subsample=0.9, 
              max_depth=9, early_stopping_round = 3)
  yhat=cbind(yhat,predict(fit,sapply(select(seosub_valid[[i]],-c('y')),as.numeric)))
  fit_list[[i-4]]=fit
}
fit_list
rmse(answer,yhat)
# earlystop=10 : rmse 170.471
# earlystop=5 : rmse 168.7949
# earlystop=3 : rmse 167.1102
# earlystop=2 : rmse 166.7716

plot(1,1)
plot(1:100,answer[10001:10100,18],ylim=c(0,1000),type='l',lwd=5,
     xlab="predict", ylab="actual")
points(1:100,yhat[10001:10100,18],col="#ff003990",type='l',lwd=5)



# 전체에 적용
fit_list=list()
yhat=NULL
answer=NULL
for(i in 5:24){
  answer=cbind(answer,seosub_h[[i]]$y)
}
for (i in 5:24){
  y=as.matrix(seosub_h[[i]]$y)
  x=select(seosub_h[[i]],-c('y'))
  x=sapply(x,as.numeric)
  fit=xgboost(data=x,label=y, booster='gbtree', nthread=4,
              nrounds=10000, metrics='rmse', eta=0.126,
              colsample_bytree=0.9, subsample=0.9, 
              max_depth=9, early_stopping_round = 3)
  yhat=cbind(yhat,predict(fit,sapply(select(seosub_h[[i]],-c('y')),as.numeric)))
  fit_list[[i-4]]=fit
}
fit_list
rmse(answer,yhat)
yhat_df=cbind(seosub3[,1:12],yhat)
#write.table(yhat, "yhat.txt")
#write.csv(yhat_df, "yhat_df.csv")


#######################################
setwd("C:/Users/st4ndup/Desktop/탐자프로젝트")
library(dplyr)
yhat_df=read.csv('yhat_df.csv')
yhat_df=yhat_df[,-1]
#######################################
library(ggplot2)
yhat_df_hs=yhat_df[,c(1:6,11:32)]

#평일
yhat_df_hs1=yhat_df_hs[yhat_df_hs$dow<=5,]
평일모든시간=cbind(yhat_df_hs1[,1:8],apply(yhat_df_hs1[,9:28],1,sum))
평일출근시간=cbind(yhat_df_hs1[,1:8],apply(yhat_df_hs1[,10:12],1,sum))
평일퇴근시간=cbind(yhat_df_hs1[,1:8],apply(yhat_df_hs1[,22:24],1,sum))
names(평일모든시간)[9]="pop";names(평일출근시간)[9]="pop";names(평일퇴근시간)[9]="pop"

#평일 빈도 수
mean(평일모든시간$pop)
mean(평일출근시간$pop+평일퇴근시간$pop)
#5시~24시 까지의 시간동안 출퇴근 6시간 동안이 절반 가까운 빈도

#평일모든
평일모든시간smre=summarise(group_by(평일모든시간,station),meanpop=mean(pop))
평일모든시간smre=arrange(평일모든시간smre, -meanpop)
평일모든시간smre$meanpop=평일모든시간smre$meanpop*2
sta_name=as.data.frame(평일모든시간smre)$station[1:50]

#호선별 승하차
ggplot(data=평일모든시간, aes(y=pop, x=as.factor(line), color=info))+
  geom_boxplot()
ggplot(data=평일출근시간, aes(y=pop, x=as.factor(line), color=info))+
  geom_boxplot()
ggplot(data=평일퇴근시간, aes(y=pop, x=as.factor(line), color=info))+
  geom_boxplot()

#상위 50개역 추출
for (i in 1:138890){
  if(평일모든시간$station[i] %in% sta_name){
    평일모든시간$num[i]=1
  }else{평일모든시간$num[i]=0}
}
평일모든시간=평일모든시간[평일모든시간$num==1,-10]

for (i in 1:138890){
  if(평일출근시간$station[i] %in% sta_name){
    평일출근시간$num[i]=1
  }else{평일출근시간$num[i]=0}
}
평일출근시간=평일출근시간[평일출근시간$num==1,-10]

for (i in 1:138890){
  if(평일퇴근시간$station[i] %in% sta_name){
    평일퇴근시간$num[i]=1
  }else{평일퇴근시간$num[i]=0}
}
평일퇴근시간=평일퇴근시간[평일퇴근시간$num==1,-10]

#평일출근, 승차-하차
평일출근시간smre=summarise(group_by(평일출근시간,station,info),meanpop=mean(pop))
평일출근시간smre=data.frame(평일출근시간smre[평일출근시간smre$info=='승차',]$station,
                            평일출근시간smre[평일출근시간smre$info=='승차',]$meanpop-
                              평일출근시간smre[평일출근시간smre$info=='하차',]$meanpop)
names(평일출근시간smre)=c("station","승차하차")

#승차-하차가 낮은것, 높은것 상하위 10역
평일출근시간smre=arrange(평일출근시간smre,-승차하차)
ggplot(data=평일출근시간smre, aes(y=승차하차, x=station))+
  geom_point()

#평일퇴근, 승차-하차
평일퇴근시간smre=summarise(group_by(평일퇴근시간,station,info),meanpop=mean(pop))
평일퇴근시간smre=data.frame(평일퇴근시간smre[평일퇴근시간smre$info=='승차',]$station,
                            평일퇴근시간smre[평일퇴근시간smre$info=='승차',]$meanpop-
                              평일퇴근시간smre[평일퇴근시간smre$info=='하차',]$meanpop)
names(평일퇴근시간smre)=c("station","승차하차")

#승차-하차가 낮은것, 높은것 상하위 10역
평일퇴근시간smre=arrange(평일퇴근시간smre,-승차하차)

ggplot(data=평일퇴근시간smre, aes(y=승차하차, x=station))+
  geom_point()

#종합
names(평일출근시간smre)[2]="출근승차하차"
names(평일퇴근시간smre)[2]="퇴근승차하차"
평일승하차=merge(평일출근시간smre,평일퇴근시간smre)
arrange(평일승하차,출근승차하차-퇴근승차하차)$station




#주말
yhat_df_hs2=yhat_df_hs[yhat_df_hs$dow>5,]
주말모든시간=cbind(yhat_df_hs2[,1:8],apply(yhat_df_hs2[,9:28],1,sum))
주말출근시간=cbind(yhat_df_hs2[,1:8],apply(yhat_df_hs2[,10:12],1,sum))
주말퇴근시간=cbind(yhat_df_hs2[,1:8],apply(yhat_df_hs2[,22:24],1,sum))
names(주말모든시간)[9]="pop";names(주말출근시간)[9]="pop";names(주말퇴근시간)[9]="pop"

#평일 빈도 수
mean(주말모든시간$pop)
mean(주말출근시간$pop+주말퇴근시간$pop)
#5시~24시 까지의 시간동안 출퇴근 6시간 동안이 절반 가까운 빈도

#주말모든
주말모든시간smre=summarise(group_by(주말모든시간,station),meanpop=mean(pop))
주말모든시간smre=arrange(주말모든시간smre, -meanpop)
주말모든시간smre$meanpop=주말모든시간smre$meanpop*2
sta_name=as.data.frame(주말모든시간smre)$station[1:50]

#시간 다시 나누기
주말아침=cbind(yhat_df_hs2[,1:8],apply(yhat_df_hs2[,9:13],1,sum))
주말점심=cbind(yhat_df_hs2[,1:8],apply(yhat_df_hs2[,14:18],1,sum))
주말저녁=cbind(yhat_df_hs2[,1:8],apply(yhat_df_hs2[,19:23],1,sum))
주말밤=cbind(yhat_df_hs2[,1:8],apply(yhat_df_hs2[,24:28],1,sum))
names(주말아침)[9]="pop";names(주말점심)[9]="pop"
names(주말저녁)[9]="pop";names(주말밤)[9]="pop"

for (i in 1:55344){
  if(주말아침$station[i] %in% sta_name){
    주말아침$num[i]=1
  }else{주말아침$num[i]=0}
}
주말아침=주말아침[주말아침$num==1,-10]
for (i in 1:55344){
  if(주말점심$station[i] %in% sta_name){
    주말점심$num[i]=1
  }else{주말점심$num[i]=0}
}
주말점심=주말점심[주말점심$num==1,-10]
for (i in 1:55344){
  if(주말저녁$station[i] %in% sta_name){
    주말저녁$num[i]=1
  }else{주말저녁$num[i]=0}
}
주말저녁=주말저녁[주말저녁$num==1,-10]
for (i in 1:55344){
  if(주말밤$station[i] %in% sta_name){
    주말밤$num[i]=1
  }else{주말밤$num[i]=0}
}
주말밤=주말밤[주말밤$num==1,-10]

주말아침smre=summarise(group_by(주말아침,station,info),meanpop=mean(pop))
주말점심smre=summarise(group_by(주말점심,station,info),meanpop=mean(pop))
주말저녁smre=summarise(group_by(주말저녁,station,info),meanpop=mean(pop))
주말밤smre=summarise(group_by(주말밤,station,info),meanpop=mean(pop))

주말아침smre=data.frame(주말아침smre[주말아침smre$info=='승차',]$station,
                         주말아침smre[주말아침smre$info=='승차',]$meanpop-
                           주말아침smre[주말아침smre$info=='하차',]$meanpop)
names(주말아침smre)=c("station","승차하차")

주말점심smre=data.frame(주말점심smre[주말점심smre$info=='승차',]$station,
                        주말점심smre[주말점심smre$info=='승차',]$meanpop-
                          주말점심smre[주말점심smre$info=='하차',]$meanpop)
names(주말점심smre)=c("station","승차하차")

주말저녁smre=data.frame(주말저녁smre[주말저녁smre$info=='승차',]$station,
                        주말저녁smre[주말저녁smre$info=='승차',]$meanpop-
                          주말저녁smre[주말저녁smre$info=='하차',]$meanpop)
names(주말저녁smre)=c("station","승차하차")

주말밤smre=data.frame(주말밤smre[주말밤smre$info=='승차',]$station,
                        주말밤smre[주말밤smre$info=='승차',]$meanpop-
                          주말밤smre[주말밤smre$info=='하차',]$meanpop)
names(주말밤smre)=c("station","승차하차")

주말아침smre=arrange(주말아침smre, -승차하차)
주말점심smre=arrange(주말점심smre, -승차하차)
주말저녁smre=arrange(주말저녁smre, -승차하차)
주말밤smre=arrange(주말밤smre, -승차하차)

#종합
주말아침smre$num=1;names(주말아침smre)[2]='승하차'
주말점심smre$num=2;names(주말점심smre)[2]='승하차'
주말저녁smre$num=3;names(주말저녁smre)[2]='승하차'
주말밤smre$num=4;names(주말밤smre)[2]='승하차'

주말종합=rbind(주말아침smre,주말점심smre,주말저녁smre,주말밤smre)
주말종합=arrange(주말종합,station)

ggplot(data=주말종합[1:40,], aes(y=승하차, x=num, color=station))+
  geom_line(lwd=2)+
  geom_point(size=3)
ggplot(data=주말종합[41:80,], aes(y=승하차, x=num, color=station))+
  geom_line(lwd=2)+
  geom_point(size=3)
ggplot(data=주말종합[81:120,], aes(y=승하차, x=num, color=station))+
  geom_line(lwd=2)+
  geom_point(size=3)
ggplot(data=주말종합[121:160,], aes(y=승하차, x=num, color=station))+
  geom_line(lwd=2)+
  geom_point(size=3)
ggplot(data=주말종합[161:200,], aes(y=승하차, x=num, color=station))+
  geom_line(lwd=2)+
  geom_point(size=3)

#주말 탐색
typeA=c('강남',
        '경복궁',
        '고속터미널',
        '광화문',
        '삼성',
        '서울',
        '서초',
        '선릉',
        '안국',
        '신사',
        '을지로입구',
        '종각',
        '잠실',
        '압구정',
        '홍대입구')

typeB=c('까치산',
        '낙성대',
        '구로디지털단지',
        '서울대입구',
        '봉천',
        '미아사거리',
        '신림',
        '쌍문',
        '수유',
        '신대방',
        '연신내')

주말종합[주말종합$station%in%typeA,]%>%
ggplot(aes(y=승하차, x=num, color=station))+
  geom_line(lwd=2)+
  geom_point(size=3)

주말종합[주말종합$station%in%typeB,]%>%
  ggplot(aes(y=승하차, x=num, color=station))+
  geom_line(lwd=2)+
  geom_point(size=3)
