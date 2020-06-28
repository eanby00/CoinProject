data = read.csv("코인입시데이터.csv",header = TRUE)
str(data)
data$년도 = factor(data$년도, levels = c(2015,2016,2017,2018,2019,2020))

{
library(dplyr)
library(car)
library(dunn.test)
library(psych)
library(stringr)
library(ggplot2)
library(gridExtra)
library(polycor)
library(tidyr)
}

# 1 ----------------------------------------------------------------------------------------------------
sample1 = data %>% filter(!is.na(지원인원))

par(mfrow = c(1,2))
plot(sample1$지원인원~sample1$년도, xlab = "년도", ylab = "지원인원", col = "gray",main = "이상치제거전")
sample1 = sample1 %>% filter(지원인원<600)
plot(sample1$지원인원~sample1$년도, xlab = "년도", ylab = "지원인원", col = "gray",main = "이상치제거후")
par(mfrow = c(1,1))

shapiro.test(sample1$지원인원[sample1$년도 == 2015])
shapiro.test(sample1$지원인원[sample1$년도 == 2016])
shapiro.test(sample1$지원인원[sample1$년도 == 2017])
shapiro.test(sample1$지원인원[sample1$년도 == 2018])
shapiro.test(sample1$지원인원[sample1$년도 == 2019])
shapiro.test(sample1$지원인원[sample1$년도 == 2020])

kruskal.test(sample1$지원인원~sample1$년도)
dunn.test(sample1$지원인원,sample1$년도,method="bonferroni",altp = T)
# 2015, 2016
# 2015, 2017
# 2015, 2018
# 2015, 2019
# 2016, 2020
# 2017, 2020
# 2018, 2020
# 2019, 2020

describeBy(sample1$지원인원,sample1$년도, mat = T)
# 평균기준: 2019,2018,2017,2016,2020,2015
# 중앙값기준: 2019,2018,2017,2016,2020,2015
plot_sample1 = sample1 %>% group_by(년도) %>% summarise(mean = mean(지원인원),median = median(지원인원))
top1_mean = plot_sample1 %>%arrange(mean) %>% head(6)
top1_median = plot_sample1 %>%arrange(desc(median)) %>% head(6)
s1p1 = ggplot(top1_mean,aes(x = reorder(년도, mean),y = mean)) +geom_col()+coord_flip()+
  labs(y = "지원인원의 평균",
      x = "년도",
      title = "지원인원 순위 - 평균기준")
s1p2 = ggplot(top1_median,aes(x = reorder(년도, median),y = median)) +geom_col()+coord_flip()+
  labs(y = "지원인원의 중앙값",
      x = "년도",
      title = "지원인원 순위 - 중앙값기준")
grid.arrange(s1p1,s1p2, ncol = 2)
par(mfrow = c(1,1))

# -----------------------------------------------------------------------------------------------------------

{
# 교과 정규분포X, 등분산X
#가천교과 = data %>% filter(data$전형 == "학생부우수자") %>% filter(!is.na(지원인원))
#명지성신교과 = data %>% filter(data$전형 == "학생부교과" ) %>% filter(!is.na(지원인원))
#명지교과 = data %>% filter(data$전형 == "학생부교과(교과성적)") %>% filter(!is.na(지원인원))
#과기상명교과 = data %>% filter(data$전형 == "학생부교과우수자전형") %>% filter(!is.na(지원인원))
#서여교과 = data %>% filter(data$전형 == "학생부 교과(일반학생)전형[70%반영]") %>% filter(!is.na(지원인원))
#sample = rbind(가천교과,명지성신교과,명지교과,과기상명교과,서여교과)
#plot(sample$지원인원~sample$년도)
#sample = sample %>% filter(지원인원<400)
#plot(sample$지원인원~sample$년도, xlab = "년도", ylab = "지원인원")

#shapiro.test(sample$지원인원[sample$년도 == 2015])
#shapiro.test(sample$지원인원[sample$년도 == 2016])
#shapiro.test(sample$지원인원[sample$년도 == 2017])
#shapiro.test(sample$지원인원[sample$년도 == 2018])
#shapiro.test(sample$지원인원[sample$년도 == 2019])
#shapiro.test(sample$지원인원[sample$년도 == 2020])

#kruskal.test(sample$지원인원~sample$년도)
#dunn.test(sample$지원인원,sample$년도,method="bonferroni",altp = T)

# 2016, 2017
# 2017, 2018
# 2018, 2020
#describeBy(sample$지원인원,sample$년도, mat = T)
# 평균기준: 2018,2019,2016,2020,2017,2015
# 중앙값기준: 2018,2016,2019,2015,2020,2017

## 종합 정규분포X, 등분산X
#가천종합1 = data %>% filter(data$전형 == "가천프런티어") %>% filter(!is.na(지원인원))
#가천종합2 = data %>% filter(data$전형 == "가천바람개비1") %>% filter(!is.na(지원인원))
#가천종합3 = data %>% filter(data$전형 == "가천프런티어(가천바람개비1)") %>% filter(!is.na(지원인원))
#가천종합4 = data %>% filter(data$전형 == "가천바람개비") %>% filter(!is.na(지원인원))
#명지종합 = data %>% filter(data$전형 == "학생부종합(명지인재)") %>% filter(!is.na(지원인원))
#명지성신종합 = data %>% filter(data$전형 == "학생부종합" ) %>% filter(!is.na(지원인원))
#상명종합 = data %>% filter(data$전형 == "학생부종합(상명인재전형)" ) %>% filter(!is.na(지원인원))
#과기종합 = data %>% filter(data$전형 == "학교생활우수자전형") %>% filter(!is.na(지원인원))
#서여종합1 = data %>% filter(data$전형 == "학생부 종합 전형") %>% filter(!is.na(지원인원))
#서여종합2 = data %>% filter(data$전형 == "학생부 종합 전형 (바롬인재 전형)") %>% filter(!is.na(지원인원))
#sample2 = rbind(가천종합1,가천종합2,가천종합3,가천종합4,명지종합,명지성신종합,상명종합,과기종합,서여종합1,서여종합2)
#shapiro.test(sample2$지원인원[sample2$년도 == 2015])
#shapiro.test(sample2$지원인원[sample2$년도 == 2016])
#shapiro.test(sample2$지원인원[sample2$년도 == 2017])
#shapiro.test(sample2$지원인원[sample2$년도 == 2018])
#shapiro.test(sample2$지원인원[sample2$년도 == 2019])
#shapiro.test(sample2$지원인원[sample2$년도 == 2020])
#plot(sample2$지원인원~sample2$년도)
#sample2 = sample2 %>% filter(지원인원<600)
#plot(sample2$지원인원~sample2$년도, xlab = "년도", ylab = "지원인원")

#kruskal.test(sample2$지원인원~sample2$년도)
#dunn.test(sample2$지원인원,sample2$년도,method="bonferroni",altp = T)
# 2015, 2018
# 2015, 2019
# 2016, 2018
# 2016, 2019
# 2017, 2018
# 2017, 2019
# 2020, 2019
#describeBy(sample2$지원인원,sample2$년도, mat = T)
# 평균기준: 2019,2018,2020,2016,2017,2015
# 중앙값기준: 2019,2018,2020,2016,2017,2015

# 교과 + 종합 정규분포X, 등분산X
#sample3 =rbind(sample,sample2)
#shapiro.test(sample3$지원인원[sample3$년도 == 2015])
#shapiro.test(sample3$지원인원[sample3$년도 == 2016])
#shapiro.test(sample3$지원인원[sample3$년도 == 2017])
#shapiro.test(sample3$지원인원[sample3$년도 == 2018])
#shapiro.test(sample3$지원인원[sample3$년도 == 2019])
#shapiro.test(sample3$지원인원[sample3$년도 == 2020])

#kruskal.test(sample3$지원인원~sample3$년도)
#dunn.test(sample3$지원인원,sample3$년도,method="bonferroni",altp = T)
}

# 2--------------------------------------------------------------------------------------------------------------------

sample2 = data %>% filter(!is.na(평균점수))
shapiro.test(sample2$평균점수[sample2$년도 == 2015])
shapiro.test(sample2$평균점수[sample2$년도 == 2016])
shapiro.test(sample2$평균점수[sample2$년도 == 2017])
shapiro.test(sample2$평균점수[sample2$년도 == 2018])
shapiro.test(sample2$평균점수[sample2$년도 == 2019])
shapiro.test(sample2$평균점수[sample2$년도 == 2020])

par(mfrow = c(1,2))
plot(sample2$평균점수~sample2$년도, xlab = "년도", ylab = "평균 등급", col = "gray",main = "이상치제거전")
sample2 = sample2 %>% filter(평균점수<10)
plot(sample2$평균점수~sample2$년도, xlab = "년도", ylab = "평균 등급", col = "gray",main = "이상치제거후")
par(mfrow = c(1,1))

kruskal.test(sample2$평균점수~sample2$년도)
dunn.test(sample2$평균점수,sample2$년도,method="bonferroni",altp = T)
# 2016, 2020

describeBy(sample2$평균점수,sample2$년도, mat = T)
# 평균기준: 2016,2018,2019,2015,2017,2020
# 중앙값기준: 2015,2016,2017,2020,2018,2019

plot_sample2 = sample2 %>% group_by(년도) %>% summarise(mean = mean(평균점수),median = median(평균점수))
top2_mean = plot_sample2 %>%arrange(mean) %>% head(6)
top2_median = plot_sample2 %>%arrange(desc(median)) %>% head(6)
s2p1 = ggplot(top2_mean,aes(x = reorder(년도, -mean),y = mean)) +geom_col()+coord_flip()+
  labs(y = "평균 등급의 평균",
       x = "년도",
       title = "평균 등급 순위 - 평균기준")
s2p2 = ggplot(top2_median,aes(x = reorder(년도, -median),y = median)) +geom_col()+coord_flip()+
  labs(y = "평균 등급의 중앙값",
       x = "년도",
       title = "평균 등급 순위 - 중앙값기준")
grid.arrange(s2p1,s2p2, ncol = 2)
par(mfrow = c(1,1))

# 3--------------------------------------------------------------------------------------------------------------------

sample3 = data
sample3$전형 = as.character(sample3$전형)
{
# 가천대
sample3$전형 = ifelse(sample3$전형 == "학생부우수자", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천프런티어", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천바람개비1", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천프런티어(가천바람개비1)", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천바람개비", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천의예전형", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "사회기여자", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학석사통합(5년제)", "학석사통합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "교육기회균형", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "단원고특별전형", "단원고", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "실기우수자", "실기", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천SW", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천바람개비(가천바람개비2)", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "특성화고교(종합)", "특성화", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "농어촌(종합)", "농어촌", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "농어촌(학생부우수자)", "농어촌", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "가천바람개비2", "교과", sample3$전형 )

# 명지대
sample3$전형 = ifelse(sample3$전형 == "학생부교과", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과면접", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "국가보훈대상자", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "기회균형", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "사회적배려대상자", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "크리스천리더", "종교", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "특성화고교", "특성화", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "특수교육대상자", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "특성화고졸재직자", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "농어촌학생", "농어촌", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "실기우수자", "실기", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "특기자", "실기", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "재외국민", "외국민", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "특수교육", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "국가보훈", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "사회적배려", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "세계화인재", "외국민", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "적성고사", "적성", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과(교과성적)", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과(교과면접)", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(명지인재)", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과(국가보훈대상자)", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(크리스천리더)", "종교", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(기회균형)", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과(특성화고교)", "특성화", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(사회적배려대상자)", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(농어촌학생)", "농어촌", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과(특수교육대상자)", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "실기/실적(특기자)", "실기", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과(평생학습자)", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과(특성화고졸재직자)", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "재외국민", "외국민", sample3$전형 )

# 상명대
sample3$전형 = ifelse(sample3$전형 == "학생부교과우수자전형", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "상명인재전형", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "지역균형전형", "농어촌", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "사회적배려대상자전형", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "안보학전형", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "국가보훈대상자전형", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "실기우수자전형", "실기", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "농어촌학생전형", "농어촌", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(상명인재전형)", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(국가보훈대상전형)", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "실기(실기우수자전형)", "실기", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부종합(농어촌학생전형)", "농어촌", sample3$전형 )

# 서울과기대
sample3$전형 = ifelse(sample3$전형 == "학교생활우수자전형", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "전공우수자전형", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "논술전형", "논술", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과우수자전형", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "SW(소프트웨어)인재전형", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "국가보훈대상자전형", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "기회균형전형", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "농어촌학생전형", "농어촌", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "실기전형", "실기", sample3$전형 )

# 성신여대
sample3$전형 = ifelse(sample3$전형 == "학생부종합", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부교과", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "논술전형", "논술", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "정시", "정시", sample3$전형 )

# 서울여대
sample3$전형 = ifelse(sample3$전형 == "학생부 교과(일반학생)전형[70%반영]", "교과", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "논술우수자 전형 [30%반영]", "논술", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "정시(가군)", "정시", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "정시(나군)", "정시", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "정시(다군)", "정시", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (기독교지도자 전형)", "종교", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (바롬인재 전형)", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (플러스인재 전형)", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (융합인재 전형)", "종합", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (고른기회 I) [기초생활수급자  및  차상위계층,  국가보훈대상자,  서해5도주민]", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (고른기회 I) [농어촌 학생]", "기회균형", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (고른기회 II) [군인・경찰・소방공무원의  자녀,  다자녀(3자녀  이상)  가정의  자녀]", "유공자", sample3$전형 )


# 기타
sample3$전형 = ifelse(sample3$전형 == "군사학전형", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "단원고", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "적성", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "외국민", "특수", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "실기/실적(실기우수자)", "실기", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학생부 종합 전형 (고른기회 II) [군인?경찰?소방공무원의  자녀,  다자녀(3자녀  이상)  가정의  자녀]", "유공자", sample3$전형 )
sample3$전형 = ifelse(sample3$전형 == "학석사통합", "특수", sample3$전형 )
}
str(data)
str(sample3)
sample3$전형 = factor(sample3$전형)
str(sample3)
describeBy(sample3$평균점수,sample3$전형,mat = T)

# 교과,기회균형(사회적 취약층),논술,농어촌,실기,유공자(국가유공자),종교,종합,특성화
sample4 = sample3 %>% filter(전형 != "특수") %>% filter(!is.na(평균점수))
sample4 = sample4 %>% filter(전형 != "정시")
sample4 = sample4 %>% filter(평균점수>1)
sample4$전형 = as.character(sample4$전형)
sample4$전형 = factor(sample4$전형, levels = c("교과","기회균형","논술","농어촌","실기","유공자","종교","종합","특성화"),
                    labels = c("교과","기회균형","논술","농어촌","실기","유공자","종교","종합","특성화"))
describeBy(sample4$평균점수,sample4$전형,mat = T)
plot(sample4$평균점수~sample4$전형, xlab = "전형", ylab = "평균 등급", col = "gray", main = "전형별 평균 등급")


shapiro.test(sample4$평균점수[sample4$전형 == "교과"])
shapiro.test(sample4$평균점수[sample4$전형 == "기회균형"])
shapiro.test(sample4$평균점수[sample4$전형 == "논술"])
shapiro.test(sample4$평균점수[sample4$전형 == "농어촌"])
shapiro.test(sample4$평균점수[sample4$전형 == "실기"])
shapiro.test(sample4$평균점수[sample4$전형 == "유공자"])
shapiro.test(sample4$평균점수[sample4$전형 == "종교"])
shapiro.test(sample4$평균점수[sample4$전형 == "종합"])
shapiro.test(sample4$평균점수[sample4$전형 == "특성화"])

kruskal.test(sample4$평균점수~sample4$전형)
dunn.test(sample4$평균점수,sample4$전형,method="bonferroni",altp = T)
{
# 기회균형,교과
# 논술,교과
# 농어촌,교과
# 실기,교과
# 유공자,교과
# 종교,교과
# 종합,교과

# 농어촌, 기회균형
# 실기, 기회균형
# 종교, 기회균형 
# 종합, 기회균형
# 특성화, 기회균형

# 농어촌, 논술 
# 실기, 논술
# 종교, 논술
# 종합, 논술
# 특성화, 논술

# 실기, 농어촌
# 유공자,농어촌
# 종교, 농어촌
# 특성화, 농어촌

# 유공자,실기
# 종교, 실기
# 종합, 실기
# 특성화, 실기

# 종합, 유공자
# 특성화, 유공자

# 종합, 종교
# 특성화, 종교

# 특성화, 종합 
}
# 평균기준: 특성화, 교과, 종합, 농어촌, 종교, 유공자, 논술, 기회균형, 실기 
# 중앙값기준: 특성화, 교과, 종합, 농어촌, 종교, 유굥자,  논술 = 기회균형, 실기

plot_sample3 = sample4 %>% group_by(전형) %>% summarise(mean = mean(평균점수),median = median(평균점수))
top3_mean = plot_sample3 %>%arrange(mean) %>% head(9)
top3_median = plot_sample3 %>%arrange(desc(median)) %>% head(9)
s3p1 = ggplot(top3_mean,aes(x = reorder(전형, -mean),y = mean)) +geom_col()+coord_flip()+
  labs(y = "평균 등급의 평균",
       x = "전형",
       title = "평균 등급 순위 - 평균기준")
s3p2 = ggplot(top3_median,aes(x = reorder(전형, -median),y = median)) +geom_col()+coord_flip()+
  labs(y = "평균 등급의 중앙값",
       x = "전형",
       title = "평균 등급 순위 - 중앙값기준")
grid.arrange(s3p1,s3p2, ncol = 2)
par(mfrow = c(1,1))

# 4--------------------------------------------------------------------------------------------------------------------

# 의미있는 검정을 위해 교과로 한정 
sample5 = sample4 %>% filter(전형 == "교과")
sample5 = sample5 %>% filter(!is.na(평균점수))
sample5 = sample5 %>% filter(!is.na(경쟁률))
sample5 = sample5 %>% filter(평균점수 < 10) %>% filter(평균점수 >= 1)
sample5 = sample5 %>% filter(경쟁률< 50)

shapiro.test(sample5$평균점수)
shapiro.test(sample5$경쟁률)

cor.test(sample5$평균점수,sample5$경쟁률, method = "kendall")

par(mfrow = c(1,2))
plot(sample5$경쟁률~sample5$평균점수, xlab = "평균 등급", ylab = "경쟁률",main = "평균 등급과 경쟁률")
plot(sample5$경쟁률~sample5$평균점수, xlab = "평균 등급", ylab = "경쟁률", main = "평균 등급과 경쟁률, 관계식")
lm.sample5 = lm(sample5$경쟁률~sample5$평균점수)
summary(lm.sample5)
abline(lm.sample5, col = "red")
par(mfrow = c(1,1))

score_sample = sample5 %>% group_by(과) %>% summarise(mean_score = mean(평균점수))
bottom10_fir = score_sample %>% arrange(desc(mean_score)) %>% head(10)
top10_fir = score_sample %>% arrange(mean_score) %>% head(10)
topbot10_fir = bind_rows(bottom10_fir,top10_fir)

compet_sample = sample5 %>% group_by(과) %>% summarise(mean_compet = mean(경쟁률))
bottom10_sec = compet_sample %>% arrange(desc(mean_compet)) %>% head(10)
top10_sec = compet_sample %>% arrange(mean_compet) %>% head(10)
topbot10_sec = bind_rows(bottom10_sec,top10_sec)


p1 = ggplot(topbot10_fir,aes(x = reorder(과, -mean_score),y = mean_score)) +geom_col()+coord_flip()+
  labs(y = "평균 등급",
       x = "전형",
       title = "평균 등급 순위")
p2 = ggplot(topbot10_sec,aes(x = reorder(과, mean_compet),y = mean_compet)) +geom_col()+coord_flip()+
  labs(y = "경쟁률",
       x = "전형",
       title = "경쟁률 순위")
grid.arrange(p1,p2, ncol = 2)
par(mfrow = c(1,1))
# 10개의 데이터 중 3개만 겹침

# 5--------------------------------------------------------------------------------------------------------------------

sample6 = sample4
sample6 = sample6 %>% filter(학교명 == "명지대" )
sample6 = sample6 %>% select(년도, 전형,평균점수)
new = sample6 %>% group_by(년도) %>% summarise(mean = mean(평균점수), last = max(평균점수), first = min(평균점수))
test = data.frame(년도 = c(2015,2016,2017,2018,2019,2020), 전형 = c("전체","전체","전체","전체","전체","전체"))
test$년도 = factor(test$년도)
new = left_join(new, test)
sample6 = sample6 %>% group_by(년도, 전형) %>% summarise(mean = mean(평균점수), last = max(평균점수), first = min(평균점수))
str(new)
str(sample6)
save = new[,5]
new[,5] = new[,4]
new[,4] = new[,3]
new[,3] = new[,2]
new[,2] = save
colnames(new) = c("년도", "전형", "mean", "last", "first")
qplot(년도, mean, data = sample6, geom = c("point","line"), colour = 전형)
ggplot(sample6, aes(x = 년도, y = mean, group = 전형)) + geom_line(aes(color = 전형)) + geom_point(aes(color = 전형))

sample6$년도 = as.character(sample6$년도)
sample6$년도 = as.integer(sample6$년도)
str(sample6)

lmm = lm(sample6$mean~sample6$년도+sample6$전형)
summary(lmm)
# 년도보다는 전형이 등급의 차이를 가져온다는 것을 확인할 수 있음 

# 교과전형 
score1= sample6 %>% filter(전형 == "교과")
new_score1 = gather(score1, "mean","last","first", key = "method", value = "score")
ggplot(new_score1, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score1$mean,score1$년도, method = "pearson")
cor.test(score1$mean,score1$년도, method = "spearman")
cor.test(score1$mean,score1$년도, method = "kendall")
# 상관 관계 없음 

# first
cor.test(score1$first,score1$년도, method = "pearson")
cor.test(score1$first,score1$년도, method = "spearman")
cor.test(score1$first,score1$년도, method = "kendall")
# 상관 관계 없음 

# last
cor.test(score1$last,score1$년도, method = "pearson")
cor.test(score1$last,score1$년도, method = "spearman")
cor.test(score1$last,score1$년도, method = "kendall")
# 상관 관계 없음 

# 기회균형전형
score2 = sample6 %>% filter(전형 == "기회균형")
new_score2 = gather(score2, "mean","last","first", key = "method", value = "score")
ggplot(new_score2, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score2$mean,score1$년도, method = "pearson")
cor.test(score2$mean,score1$년도, method = "spearman")
cor.test(score2$mean,score1$년도, method = "kendall")
# 상관 관계 있음 => kendall의 방식으로 상관계수 0.8666667 

# first
cor.test(score2$first,score1$년도, method = "pearson")
cor.test(score2$first,score1$년도, method = "spearman")
cor.test(score2$first,score1$년도, method = "kendall")
# 상관 관계 없음 

# last
cor.test(score2$last,score1$년도, method = "pearson")
cor.test(score2$last,score1$년도, method = "spearman")
cor.test(score2$last,score1$년도, method = "kendall")
# 상관 관계 없음 

# 농어촌전형 # 여기서부터 하기
score3= sample6 %>% filter(전형 == "농어촌")
new_score3 = gather(score3, "mean","last","first", key = "method", value = "score")
ggplot(new_score3, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score3$mean,score3$년도, method = "pearson")
cor.test(score3$mean,score3$년도, method = "spearman")
cor.test(score3$mean,score3$년도, method = "kendall")
# 상관 관계 없음 

# first
cor.test(score3$first,score3$년도, method = "pearson")
cor.test(score3$first,score3$년도, method = "spearman")
cor.test(score3$first,score3$년도, method = "kendall")
# 상관 관계 없음 

# last
cor.test(score3$last,score3$년도, method = "pearson")
cor.test(score3$last,score3$년도, method = "spearman")
cor.test(score3$last,score3$년도, method = "kendall")
# 상관 관계 없음 

# 실기전형 
score4= sample6 %>% filter(전형 == "실기")
new_score4 = gather(score4, "mean","last","first", key = "method", value = "score")
ggplot(new_score4, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score4$mean,score4$년도, method = "pearson")
cor.test(score4$mean,score4$년도, method = "spearman")
cor.test(score4$mean,score4$년도, method = "kendall")
# 상관 관계 없음 

# first
cor.test(score4$first,score4$년도, method = "pearson")
cor.test(score4$first,score4$년도, method = "spearman")
cor.test(score4$first,score4$년도, method = "kendall")
# 상관 관계 없음 

# last
cor.test(score4$last,score4$년도, method = "pearson")
cor.test(score4$last,score4$년도, method = "spearman")
cor.test(score4$last,score4$년도, method = "kendall")
# 상관 관계 없음 

# 유공자 전형 
score5= sample6 %>% filter(전형 == "유공자")
new_score5 = gather(score5, "mean","last","first", key = "method", value = "score")
ggplot(new_score5, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score5$mean,score5$년도, method = "pearson")
cor.test(score5$mean,score5$년도, method = "spearman")
cor.test(score5$mean,score5$년도, method = "kendall")
# 상관 관계 없음 

# first
cor.test(score5$first,score5$년도, method = "pearson")
cor.test(score5$first,score5$년도, method = "spearman")
cor.test(score5$first,score5$년도, method = "kendall")
# 상관 관계 없음 

# last
cor.test(score5$last,score5$년도, method = "pearson")
cor.test(score5$last,score5$년도, method = "spearman")
cor.test(score5$last,score5$년도, method = "kendall")
# 상관 관계 없음 

# 종교 전형 
score6= sample6 %>% filter(전형 == "종교")
new_score6 = gather(score6, "mean","last","first", key = "method", value = "score")
ggplot(new_score6, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score6$mean,score6$년도, method = "pearson")
cor.test(score6$mean,score6$년도, method = "spearman")
cor.test(score6$mean,score6$년도, method = "kendall")
# 상관 관계 있음, kendall방식이 0.8666667의 상관계수를 도출 

# first
cor.test(score6$first,score6$년도, method = "pearson")
cor.test(score6$first,score6$년도, method = "spearman")
cor.test(score6$first,score6$년도, method = "kendall")
# 상관 관계 없음 

# last
cor.test(score6$last,score6$년도, method = "pearson")
cor.test(score6$last,score6$년도, method = "spearman")
cor.test(score6$last,score6$년도, method = "kendall")
# 상관 관계 없음 

# 종합 전형
score7= sample6 %>% filter(전형 == "종합")
new_score7 = gather(score7, "mean","last","first", key = "method", value = "score")
ggplot(new_score7, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score7$mean,score7$년도, method = "pearson")
cor.test(score7$mean,score7$년도, method = "spearman")
cor.test(score7$mean,score7$년도, method = "kendall")
# 상관 관계 없음 

# first
cor.test(score7$first,score7$년도, method = "pearson")
cor.test(score7$first,score7$년도, method = "spearman")
cor.test(score7$first,score7$년도, method = "kendall")
# 상관 관계 없음 

# last
cor.test(score7$last,score7$년도, method = "pearson")
cor.test(score7$last,score7$년도, method = "spearman")
cor.test(score7$last,score7$년도, method = "kendall")
# 상관 관계 있음 => pearson 방식이 0.8913516의 상관 계수를 가짐 

# 특성화 전형 
score8 = sample6 %>% filter(전형 == "특성화")
new_score8 = gather(score8, "mean","last","first", key = "method", value = "score")
ggplot(new_score8, aes(x = 년도, y = score, group = method)) + geom_line(aes(color = method)) + geom_point(aes(color = method))

# mean
cor.test(score8$mean,score8$년도, method = "pearson")
cor.test(score8$mean,score8$년도, method = "spearman")
cor.test(score8$mean,score8$년도, method = "kendall")
# 상관 관계 있음 pearson 방식이 0.82526의 상관계수를 가짐 

# first
cor.test(score8$first,score8$년도, method = "pearson")
cor.test(score8$first,score8$년도, method = "spearman")
cor.test(score8$first,score8$년도, method = "kendall")
# 상관 관계 있음 kendall 방식이 0.8280787의 상관계수를 가짐  

# last
cor.test(score8$last,score8$년도, method = "pearson")
cor.test(score8$last,score8$년도, method = "spearman")
cor.test(score8$last,score8$년도, method = "kendall")
# 상관 관계 있음 kendall 방식이 0.8666667의 상관계수를 가짐  



