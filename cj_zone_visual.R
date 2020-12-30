library(ggplot2)
library(dplyr)
library(corrplot)

# 데이터 불러오기
df <-read.csv("cj_schoolzone_Data2.csv", header=T, stringsAsFactors=F)
# 필요한 데이터만 추출
data = df[,c('지역구', '대상시설명', '신호등', 'CCTV설치여부', '발생건수', '사망자수')]
data

# 데이터 변수 이름 변경 -- 한글 너무 길어서
data <- rename(data, gu='지역구', zone='대상시설명', lamp='신호등',cctv='CCTV설치여부', cnt_a='발생건수', cnt_d='사망자수')
head(data)
View(data)

---
data
data$lamp <- ifelse(data$lamp == "Y", 1, 0)
data$cctv <- ifelse(data$cctv == "Y", 1, 0)
data$zone <- ifelse(data$zone == "", 0, 1)
data <- data[,c('gu','zone','lamp','cctv','cnt_a','cnt_d')]

data_sum
data_sum <- data %>% 
  group_by(gu) %>% 
  summarise(zone = sum(zone),
            lamp = sum(lamp),
            cctv = sum(cctv),
            cnt_a = sum(cnt_a),
            cnt_d = sum(cnt_d))

#지역구별 스쿨존 갯수 (bar 그래프)
ggplot(data=data_sum, aes(x=gu, y=zone))+geom_col()

str(data_sum)
dput(data_sum)

#지역구별 데이터 (line 그래프)
ggplot(data=data_sum, aes(x=gu, y=zone))+
  geom_line(color="blue", group = 1)+
  geom_line(aes(x=gu, y=lamp), color="red", group=2)+
  geom_line(aes(x=gu, y=cctv), color="yellow", group=3)+
  geom_line(aes(x=gu, y=cnt_a), color="orange", group=4)+
  geom_line(aes(x=gu, y=cnt_d), color="black", group=5)

# 청주시 어린이 인구 수 추가 (단위 천명)
data_sum$kids = c(21.40,20.14,26.67,32.34)
data_sum

# 어린이 인구 추가된 지역구별 데이터 (line 그래프)
ggplot(data=data_sum, aes(x=gu, y=zone))+
  geom_line(color="blue", group = 1)+
  geom_line(aes(x=gu, y=lamp), color="red", group=2)+
  geom_line(aes(x=gu, y=cctv), color="yellow", group=3)+
  geom_line(aes(x=gu, y=cnt_a), color="orange", group=4)+
  geom_line(aes(x=gu, y=cnt_d), color="black", group=5)+
  geom_line(aes(x=gu, y=kids), color="green", group=6)


data_sum$kids_r = round(data_sum$kids / data_sum$zone,3)
data_sum$lamp_r = round(data_sum$lamp / data_sum$zone,3)
data_sum$cctv_r = round(data_sum$cctv / data_sum$zone,3)
data_sum$cnt_a_r = round(data_sum$cnt_a / data_sum$zone,3)
data_sum$cnt_d_r = round(data_sum$cnt_d / data_sum$zone,3)

data_sum
df <- data_sum[, c("kids_r","lamp_r","cctv_r","cnt_a_r")]

df
#상관분석 함수 적용
df_cor <- cor(df)

#히트맵 그래프
col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
corrplot(df_cor, method="color",col=col(200), type="lower",order="hclust",addCoef.col="black",diag=T)
