# 일원배치 분산 분석

# 전처리
data_first = read.csv("일원분산분석.csv")
str(data_first)
score = c()
for (fir in data_first[,-1]){
  for (sec in fir){
    score = c(score,sec)
  }
}
data_make_first = data.frame(unv = c(rep("명지대",4),rep("상명대",4),rep("가천대",4)),
                             year = c(rep(c(2016,2017,2018,2019),3)),
                             score = score)
data_make_first
library(psych)
# 가설 1

# H0: 대학 별 학생부 우수자 전형 평균 점수는 차이가 없다.
# H1: 대학 별 학생부 우수자 전형 평균 점수는 적어도 한 쌍은 차이가 있다. 
describeBy(data_make_first$score, data_make_first$unv, mat = T)

library(car)
leveneTest(score~unv, data = data_make_first) # p값: 0.2495 => 등분산 
unv_score = aov(score~unv,data = data_make_first)
summary(unv_score) # p값: 0.143 => 귀무가설 채택, 평균 점수는 차이가 없다.
TukeyHSD(unv_score) # p adj 확인하기
#                diff       lwr      upr     p adj
#명지대-가천대 0.0325 -0.282882 0.347882 0.9556156
#상명대-가천대 0.2300 -0.085382 0.545382 0.1591220
#상명대-명지대 0.1975 -0.117882 0.512882 0.2405713

plot(TukeyHSD(unv_score), col ="blue")

# 가설 2
# H0: 년도 별 학생부 우수자 전형 평균 점수는 차이가 없다.
# H1: 년도 별 학생부 우수자 전형 평균 점수는 적어도 한 쌍은 차이가 있다.
data_make_first$year = factor(data_make_first$year, levels = c(2016,2017,2018,2019), labels = c(2016,2017,2018,2019))
describeBy(data_make_first$score, data_make_first$year, mat = T)

library(car)
bartlett.test(score ~ year, data=data_make_first) #p-value = 0.7692 = > 등분산 
year_score = aov(score~year,data = data_make_first)
summary(year_score) # p값: 0.306 => 귀무가설 채택, 평균 점수는 차이가 없다.
TukeyHSD(year_score)

plot(TukeyHSD(year_score), col ="red")

# ---------------------------------------------------------------------------------------------------------------
# 반복측정분산분석 
# H0: 시점별 혈중농도의 차이는 없다
# H1: 시점별 혈중농도의 차이는 있다.

data_sec = read.csv("반복측정분산분석.csv")
str(data_sec)

library(psych)
data_make_sec = data.frame(id = data_sec$id,
                           time = c(rep("pre",6),rep("after3M",6),rep("after6M",6)),
                           result = c(data_sec$pre,data_sec$after3M,data_sec$after6M))
describeBy(data_make_sec$result,data_make_sec$time, mat =T) 
str(data_make_sec)

library(car)
sec.matrix <- cbind(data_make_sec$result[data_make_sec$time == "pre"], 
                    data_make_sec$result[data_make_sec$time == "after3M"], 
                    data_make_sec$result[data_make_sec$time == "after6M"])
head(sec.matrix)

sec.model.lm = lm(sec.matrix ~ 1)
time.f = factor(c("pre", "after3M", "after6M")) 
options(contrasts=c("contr.sum", "contr.poly"))
sec.result.mt <- Anova(sec.model.lm,
                       idata=data.frame(time.f),
                       idesign=~time.f, 
                       type="III")

summary(sec.result.mt, multivariate=F)
# 구형성 검정: p-value: 0.18795 => 일변량의 p-value 확인
# 구형성을 만족하지 못할 경우 다변량을 이용하기 
sec.result = aov(result ~ time+Error(id/time), 
                  data=data_make_sec)
summary(sec.result)
# p값 = 0.882 => 귀무가설 채택, 시점별 차이는 없다. 

library(multcomp)
result.lm <- lm(result ~ time, data=data_make_sec)
tukey.result <- glht(result.lm, linfct=mcp(time='Tukey'))
summary(tukey.result)
plot(tukey.result)

TukeyHSD(aov(result~time, data = data_make_sec))
plot(TukeyHSD(aov(result~time, data = data_make_sec)),col = "red")


# -----------------------------------------------------------------------------------------------------------------

#이원분산분석
cars = data.frame(mtcars)
cars$am = factor(cars$am, levels = c(0,1), labels = c("자동","수동"))
cars$cyl = factor(cars$cyl, levels = c(4,6,8), labels = c(4, 6, 8))
str(cars)
describeBy(cars$mpg, cars$am:cars$cyl, mat = T)
cars_result = aov(mpg ~ am + cyl + am:cyl, data = cars)
summary(cars_result)
# am => p값: 4.85e-07 => am과 mpg는 관련이 있다.
# cyl => p값: 9.35e-07 => cyl과 mpg는 관련이 있다.
# am:cyl => p값: 0.269 => am과 cyl은 관련이 없다.
interaction.plot(cars$am, cars$cyl, cars$mpg)
cars_result = aov(mpg ~ am + cyl, data = cars)
summary(cars_result)

t.test(cars$mpg~cars$am) #자동이 수동보다 평균적으로 주행거리가 짧다
t.test(cars$mpg~cars$am, alternative = c("less"))

mpg_cyl=aov(cars$mpg~cars$cyl)
summary(mpg_cyl) # 4,6,8 중 한 쌍 이상은 차이가 난다. 
TukeyHSD(mpg_cyl) # 세 집단 모두 차이가 난다. 
plot(TukeyHSD(mpg_cyl), col = "blue")

# plot(TukeyHSD(cars_result))
