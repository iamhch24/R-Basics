library(dplyr)
library(ggplot2)

# 1. data 수정 

data = read.csv("진보보수_기본데이터.csv")
data
table(is.na(data)) # na 값 확인

# 각 채널의 연령대별 구독자수(명)를 구하고 새 칼럼에 할당하는 과정
data2 = data
data2$age_13_17 = data2$total * data2$a_13_17
data2$age_18_24 = data2$total * data2$a_18_24
data2$age_25_34 = data2$total * data2$a_25_34
data2$age_35_44 = data2$total * data2$a_35_44
data2$age_45_54 = data2$total * data2$a_45_54
data2$age_55_64 = data2$total * data2$a_55_64
data2$age_65_up = data2$total * data2$a_65_

data3 = data2[,10:17] # 새 칼럼들만 data3에 할당 (10열에서 17열까지)
data3

# 보수 채널의 연령대별 구독자수를 합산하는 과정
bosu = data3 %>% filter(group == 1) # 보수 채널의 데이터들만(group이 1 값) bosu에 할당
bosu = bosu[,-1] # 1열(group) 삭제
bosu
bosu_t = t(bosu) # 파이 그래프에 적절하게 행과 열을 변환 
bosu_t # 칼럼명이 존재하지 않은 상태
colnames(bosu_t) = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10") # 칼럼명 부여  
bosu_df = data.frame(bosu_t) # class가 행렬이므로 데이터 프레임으로 변환
bosu2 = bosu_df %>%
  mutate(subscriber = V1+V2+V3+V4+V5+V6+V7+V8+V9+V10) # 연령대별 구독자수 합산

# 시각화에 필요한 칼럼들 추가 및 불필요한 칼럼 제거 
bosu2$age_group = rownames(bosu_df) # 파이 차트에 쓸 x 축에 사용할 연령대 칼럼 생성
bosu2$camp = "보수" # 막대그래프 fill(hue) 값에 부여할 camp 칼럼 생성
bosu3 = bosu2[,11:13] # 그래프에 필요한 칼럼들만 할당

bosu_sum = sum(bosu3$subscriber)
bosu3$rate = round(bosu3$subscriber/bosu_sum, 3) # 연령대별 비율 추가(연령대별 구독자/총 구독자)
bosu3

# 진보 데이터의 수정 과정 
jinbo = data3 %>% filter(group == 2)
jinbo = jinbo[,-1]
jinbo_t = t(jinbo)
colnames(jinbo_t) = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")
jinbo_df = data.frame(jinbo_t)
jinbo2 = jinbo_df %>%
  mutate(subscriber = V1+V2+V3+V4+V5+V6+V7+V8+V9+V10)

jinbo2$age_group = rownames(jinbo_df)
jinbo2$camp = "진보"
jinbo3 = jinbo2[,11:13]

jinbo_sum = sum(jinbo3$subscriber)
jinbo3$rate = round(jinbo3$subscriber/jinbo_sum, 3)
jinbo3


# 2. 시각화

# 파이 차트에 사용할 라벨과 색
youtu_label = c('13~17세', '18~24세', '25~34세', '35~44세',
               '45~54세', '55~64세', '65세 이상') 
bosu_col = c('#F0E4D4', '#F9D9CA','#D18063','#917B56','#E4A99B',
              '#838BB2','#CACFE3')
jinbo_col = c('#C0D8F0', '#EDB2BC', '#A2CDE8', '#6F94A6', '#96E3D6',
              '#E6939D', '#F2E0B6')

# 보수 pie
pie(bosu3$subscriber,labels=paste(youtu_label,"\n", bosu3$rate*100,"%"),
    radius = 0.9, col = bosu_col, border = "#FFBFC1",
    main = "보수 채널의 연령대 구독자 비율")

# 진보 pie
pie(jinbo3$subscriber,labels=paste(youtu_label,"\n", jinbo3$rate*100,"%"),
    radius = 0.9, col = jinbo_col, border = "#BFBFFF", 
    main = "진보 채널의 연령대 구독자 비율")

# 막대그래프
youtu= bind_rows(bosu3,jinbo3) # 데이터프레임 세로 결합
youtu

ggplot(youtu, aes(x = age_group, y = rate, fill = camp)) + # fill = 파이썬 hue
  geom_col(position = "dodge", alpha=0.8) + # dodge는 두 막대 분리, alpha 색 투명도 조절
  ggtitle("보수, 진보 채널의 연령대별 구독자 비율 비교") +
  xlab("연령대") + ylab("구독자(%)") +
  theme(legend.title=element_blank())


# 3. 검증

# T 검증으로 비교할 두 개의 변수 생성
gum_df = read.csv("진보보수_검증데이터.csv")
gum_df
gum_df$구독자 = gum_df$구독자 * 1000 #구독자 수, 천명 단위를 명 단위로
gum_df$age_55up = gum_df$구독자 * gum_df$X55세.이상.비율 # 고령(55세 이상)만 할당
gum_bosu = gum_df %>% filter(진영 == "보수") %>% select(age_55up)
gum_jinbo = gum_df %>% filter(진영 == "진보") %>% select(age_55up)
gum_bosu

t.test(gum_bosu, gum_jinbo, var.equal = T)

# p-value = 0.0296으로, 유의수준 0.05보다 작으므로 귀무가설을 기각한다.
# 즉, 진보와 보수 채널에서 55세 이상 구독자의 평균 차이가 
# 통계적으로 유의하다고 할 수 있다.

