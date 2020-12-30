# 놀이동산 - 놀이기구 만족도 상관분석

## 1 데이터 불러오기
df <-read.csv("http://goo.gl/HKnl74")
df
## 2 데이터 확인
str(df)

## 3 결측치 확인
colSums(is.na(df)) # 컬럼별 합계
attach(df) # 데이터를 변수명으로 바로 접근할 수 있게 한다.

## 4 데이터 시각화
plot(overall~rides) # 산점도 : 두 변수간의 관계
plot(overall~rides, main="전체만족도~놀이기구 만족도와의 관계",
     xlab="놀이기구 만족도", ylab="놀이동산 전체 만족도", cex=1, pch=1, col='red') 
# pch=1 --원 형태, cex=셀크기, col -- 색상

## 5 통계 검증
cov(overall, rides)
#공분산 (covariance)과 상관계수(correlation coefficient)
#공분산을 표준화 시킨 상관계수

#상관계수 검증
cor.test(overall,rides)

#상관행렬 생성
df_cor <- cor(df[,4:8])

#히트맵 출력
corrplot(df_cor)
