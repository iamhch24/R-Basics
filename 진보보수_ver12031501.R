library(dplyr)
library(ggplot2)

data = read.csv("진보보수_기본데이터.csv")
data
# 각 연령대별 구독자수(명)를 구하고 새 칼럼에 할당하는 과정
data2 = data
data2$age_13_17 = data2$total * data2$a_13_17
data2$age_18_24 = data2$total * data2$a_18_24
data2$age_25_34 = data2$total * data2$a_25_34
data2$age_35_44 = data2$total * data2$a_35_44
data2$age_45_54 = data2$total * data2$a_45_54
data2$age_55_64 = data2$total * data2$a_55_64
data2$age_65_up = data2$total * data2$a_65_

# 새 칼럼들만 data3에 할당
data3 = data2[,11:17]

# 보수 채널의 데이터들만 bosu에 할당
bosu = data3[1:10,]
bosu
bosu_t = t(bosu) # 뒤집기
bosu_t
#칼럼을 이용해서 각 연령대별 총 구독자수를 구하고자 칼럼명을 부여(뒤집으니까 칼럼명이 없음), (각 채널들의 연령대별 구독자수 데이터를 수정하는 과정)
colnames(bosu_t) = c("aa","bb","cc","dd","ee","ff","gg","hh","ii","jj") 
bosu_df = data.frame(bosu_t) # 뒤집으니 데이터 프레임이 아니라 데이터 프레임으로 
bosu_df$age_group = rownames(bosu_df) # 파이 차트에 쓸 x 축에 사용할 연령대 칼럼 생성
bosu2 = bosu_df %>%
  mutate(subscriber = aa+bb+cc+dd+ee+ff+gg+hh+ii+jj) # 보수 진영의 연령대별 구독자수(합산)
bosu2$camp = "보수" # 막대그래프를 그릴 때 fill(hue) 값으로 부여할 camp 칼럼 생성
bosu3 = bosu2[,11:13] # 그래프에 필요한 칼럼들만 할당
bosu3
write.csv(bosu3, file = "bosu.csv") 

# 아래는, 위와 같은 이유들로, 같은 방법으로 작성
jinbo = data3[11:20,]
jinbo_t = t(jinbo)
colnames(jinbo_t) = c("aa","bb","cc","dd","ee","ff","gg","hh","ii","jj")
jinbo_df = data.frame(jinbo_t)
jinbo_df$age_group = rownames(jinbo_df)
jinbo2 = jinbo_df %>%
  mutate(subscriber = aa+bb+cc+dd+ee+ff+gg+hh+ii+jj)
jinbo2$camp = "진보"
jinbo3 = jinbo2[,11:13]
write.csv(jinbo3, file = "jinbo.csv")

# 막대 그래프에 쓸 데이터 할당, 두 데이터가 공동의 칼럼을 가지고 있다.
bosujinbo = bind_rows(bosu3, jinbo3)
write.csv(bosujinbo, file = "bosujinbo.csv")



# 아래서 부터는 시각화와 검증

# 보수 데이터에 비율 추가
bosu = read.csv("bosu.csv")
bosu_sum = sum(bosu$subscriber)
bosu$rate = round(bosu$subscriber/bosu_sum, 3)
bosu$rate

# 보수 pie
ggplot(bosu, aes(x = "", y = rate, fill = age_group)) +
  geom_bar(width = 1, stat = "identity", color = "white", alpha = 0.7) +
  coord_polar("y") +
  geom_text(aes(label = paste(rate*100,"%")), # paste는 문자열을 연결시켜 주는 함수
            position = position_stack(vjust = 0.5),) + # 비율 숫자 위치 설정
  ggtitle("보수 채널의 연령대별 구독자 비율") +
  theme_void() +
  theme(legend.title=element_blank()) + # 범례 제목 삭제
  scale_fill_discrete(labels = c("13~17세", "18~24세", "25~34세", "35~44세","45~54세", "55~64세","65세 이상")) # 각 범례 이름 설정

# 진보 pie
jinbo = read.csv("jinbo.csv")
jinbo_sum = sum(jinbo$subscriber)
jinbo$rate = round(jinbo$subscriber/jinbo_sum, 3)

ggplot(jinbo, aes(x = "", y = rate, fill = age_group)) +
  geom_bar(width = 1, stat = "identity", color = "white", alpha = 0.7) +
  coord_polar("y") +
  geom_text(aes(label = paste(rate*100,"%")), # paste는 문자열을 연결시켜 주는 함수
            position = position_stack(vjust = 0.5)) + # 비율 숫자 위치 설정
  ggtitle("진보 채널의 연령대별 구독자 비율") +
  theme_void() +
  theme(legend.title=element_blank()) + # 범례 제목 삭제
  scale_fill_discrete(labels = c("13~17세", "18~24세", "25~34세", "35~44세","45~54세", "55~64세","65세 이상")) # 각 범례 이름 설정


# 보수, 진보 막대그래프
youtu= bind_rows(bosu,jinbo)

ggplot(youtu, aes(x = age_group, y = rate, fill = camp)) + # fill은 파이썬에서 hue
  geom_col(position = "dodge", alpha=0.8) + # dodge는 두 막대 분리, alpha 색 투명도
  ggtitle("보수, 진보 채널의 연령대별 구독자 비율 비교") +
  xlab("연령대") + ylab("구독자(%)") +
  theme(legend.title=element_blank()) 



# 검증
gum_df = read.csv("진보보수_검증데이터.csv")
gum_df$구독자 = gum_df$구독자 * 1000 #구독자 수가 천명 단위로 되어 있어 수정
gum_df$age_55up = gum_df$구독자 * gum_df$X55세.이상.비율 # 고령(55세 이상)만 할당
# T 검증으로 비교할 두 개의 변수 선언?
gum_bosu = gum_df %>% filter(진영 == "보수") %>% select(age_55up)
gum_jinbo = gum_df %>% filter(진영 == "진보") %>% select(age_55up)

t.test(gum_bosu, gum_jinbo, var.equal = T)

# p-value = 0.0296으로, 유의수준 0.05보다 작으므로 귀무가설을 기각한다.
# 즉, 55세 이상 진보와 보수 채널 구독자의 평균 차이가 
# 통계적으로 유의하다고 할 수 있다.

