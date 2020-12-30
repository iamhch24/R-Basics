library(ggplot2)
library(dplyr)
library(corrplot)
library(readxl)
library(stringr) # 문자열 처리를 위한 라이브러리

#################################
# cj_zone_data.R 설명
# 이 스크립트는 청주 지역 스쿨존과 기타 교통안전 시설과의 상관도 분석, 시각화를 위해
# 3가지 CSV 파일(기초데이터: 어린이보호구역(CCTV포함) + 스쿨존어린이교통사고 + 청주 신호등정보)을
# 불러 들여 하나의 CSV 파일로 데이터를 정제, 전처리하는 프로그램이다.
#################################


#################################
# 1. 데이터 불러오기
#################################
accident <-read.csv("12_19_schoolzone.csv", header=T, stringsAsFactors=F)
zone_info <-read.csv("충청북도_청주시_어린이보호구역_20200717_1594961127925_45579.csv", header=T, stringsAsFactors=F)
lamp_info <-read.csv("충청북도_청주시_신호등정보_20191007.csv", header=T, stringsAsFactors=F)


#################################
# 2. 데이터 정제
#################################

# 전국 스쿨존 사고 중 청주 데이터만 추출 "청주"가 들어있는 데이터만 추출
accident <- accident[str_detect(accident$"시도시군구명",'청주'),]

# 스쿨존 중복 정보 제거 (ex) 초등학교 부설 유치원
# == 소재지지번주소가 같은 것을 중복으로 보고 하나만 남김
zone_info2 = zone_info[-which(duplicated(zone_info$"소재지지번주소")), ] 

# 특수학교 및 초등학교 단어 처리
# == 초등학교, 초교 --> '초'로 변경 / (특수) 등의 문자를 없애줌 
zone_info2$'대상시설명' <- str_replace(zone_info2$'대상시설명',"초등학교","초")
zone_info2$'대상시설명' <- str_replace(zone_info2$'대상시설명',"초교","초")
zone_info2$'대상시설명' <- str_replace(zone_info2$'대상시설명',"(특수)","")
zone_info2$'대상시설명' <- str_replace(zone_info2$'대상시설명',"청주분원","")
zone_info2$'대상시설명' <- str_replace(zone_info2$'대상시설명',"청주","")

# 신호등 정보에 결측치 제거, ""으로 된 공백 데이터도 제거
lamp_info <- lamp_info %>% filter(!is.na('교차로')) # 결측치 제거
lamp_info <- lamp_info %>% filter(lamp_info$'교차로'!="") #공백 데이터도 제거

#사고 데이터를 스쿨존 단위로 집계
data_acc <- accident %>% group_by(지점명) %>% summarise(cnt_a = sum(발생건수),cnt_d = sum(사망자수))
data_acc


#################################
# 3. 스쿨존 정보에 여러가지 컬럼 추가
#    1) 신호등 컬럼 추가 (from 신호등 정보)
#    2) 지역구 컬럼 추가 
#    3) 사고 발생건수, 사망자 컬럼 추가 (from 어린이사고 정보)
#################################


# 1) 스쿨존 정보에 신호등 컬럼 추가
# == for 문 사용하여 각 스쿨존에 해당하는 신호등(교차로) 정보가 있는지 확인하여
# == 신호등(교차로) 정보가 있으면 'Y'을, 없으면 'N'을 값으로 스쿨존 정보에 넣음
zones <- zone_info2$'대상시설명'
lamp_info$'교차로'
lamp <- c() # 신호등 정보 Y, N을 담을 벡터
i <- 1  # 신호등 정보 벡터의 인덱스
for (zone in zones){
  lamp[i] = ifelse(lamp_info %>% filter(str_detect(교차로, zone)) %>% nrow > 0, 'Y','N')
  i <- i+1
}
zone_info2$'신호등' <- lamp # 스쿨존 정보에 '신호등' 컬럼으로 설정
zone_info2


# 2) 스쿨존 정보에 지역구 컬럼 추가
# == 스쿨존의 소재지도로명 주소에 있는 구을 바탕으로 지역구를 설정함
zone_info2$'지역구' <- ifelse(str_detect(zone_info2$'소재지도로명주소','상당구'),'상당구',
                  ifelse(str_detect(zone_info2$'소재지도로명주소','서원구'),'서원구',
                  ifelse(str_detect(zone_info2$'소재지도로명주소','청원구'),'청원구',
                  ifelse(str_detect(zone_info2$'소재지도로명주소','흥덕구'),'흥덕구','기타'))))
zone_info2$'지역구'


# 3) 스쿨존 정보에 사고 발생건수, 사망자 컬럼 추가
# == for문을 돌면서 해당스쿨존에 해당하는 사고의 발생건수, 사망자를 조사하여 컬럼 정보를 만든다
# == for문을 돌면서 index 정보를 활용하여 새로운 컬럼과 기존의 스쿨존 정보의 인덱스를 맞춘다.
areas <- zone_info2$'지역구'
zones <- zone_info2$'대상시설명'
i <- 1  # 벡터의 인덱스
acci = c()
dead = c()
for (zone in zones){
  area = areas[i]
  row <- data_acc[str_detect(data_acc$지점명, zone) & str_detect(data_acc$지점명, area),]
  if(count(row)>0){
    acci[i] <- sum(row$cnt_a)
    dead[i] <- sum(row$cnt_d)
  }
  else{
    acci[i] <- 0
    dead[i] <- 0
  }
  # print(paste(zone,area,acci[i],dead[i]))
  i <- i+1
}
zone_info2$'발생건수' <- acci  # 스쿨존 정보에 '발생건수' 컬럼으로 설정
zone_info2$'사망자수' <- dead  # 스쿨존 정보에 '사망자수' 컬럼으로 설정

str(zone_info2)


#################################
# 4. 데이터 전처리가 끝난 스쿨존 정보를 CSV 파일로 저장한다.
#################################

write.csv(zone_info2, file="cj_schoolzone_Data2.csv")
