library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(ggthemes)
# 데이터 불러오기
df <-read.csv("kids_accident.csv", header=T, stringsAsFactors=F)
# 데이터 변수 이름 변경 -- 한글 너무 길어서
df <- rename(df, year='년도', kids_a='어린이_교통사고.건.', kids_d='어린이_사망.명.',zone_a='스쿨존_교통사고.건.', zone_d='스쿨존_사망.명.', cnt_zone='스쿨존_지정.곳.')
df
#어린이 사고vs 스쿨존 갯수 -- 정확히 보이는 부적 관계
ggplot(df) +
  geom_line(aes(x=year, y=kids_a, color="pink"), size=3) + 
  geom_point(aes(x=year, y=kids_a, color="pink"), size=7) + 
  geom_line(aes(x=year, y=cnt_zone, color="skyblue"), size=3) +
  geom_point(aes(x=year, y=cnt_zone, color="skyblue"), size=7) +
  scale_color_discrete(name = "Y series", labels = c("어린이교통사고", "스쿨존갯수"))
  

  # ggtitle("★전국★ 연도별 어린이 교통사고와 스쿨존 관계") +
  # xlab("발생 연도") + ylab("사고(건) & 스쿨존(수)") +
  

ggplot(df)+
    geom_line(aes(y=kids_a,x= year,color="pink"),size=3 )+
    geom_line(aes(y=cnt_zone,x= year,color="skyblue"),size=3) +
    scale_color_discrete(name = "Y series", labels = c("어린이교통사고", "스쿨존갯수"))
    
    
    
ggplot(df)+
  geom_line(aes(y=kids_a,x= year,colour="pink"),size=3 )+
  geom_line(aes(y=cnt_zone,x= year,colour="skyblue"),size=3) +
  scale_color_discrete(name = "Y series", labels = c("어린이교통사고", "스쿨존갯수"))+
ggtitle("★전국★ 연도별 어린이 교통사고와 스쿨존 관계") +
  xlab("발생 연도") + ylab("사고(건) & 스쿨존(수)")+
  theme_bw()  # 테마 변경
    

# 
#   
#   scale_color_manual(values = c("pink", "skyblue"),
#                      labels = c("kids_a", "cnt_zone"))
#   
#   
#   
#   legend(x=0,y=0,legend=c("kids_a", "cnt_zone"), col=c("red","blue"), lty=1, bg = "white", cex = 1)
# 
# 
# labs(x=year, y=kids_a, fill="Data") +
#   labs(x=year, y=cnt_zone, fill="Data") +
#   