# 상관분석

data_fir = read.csv("건강검진정보(상관분석).csv")
describe(data_fir)
pairs.panels(data_fir)

cor(data_fir, use="complete.obs", method = c("pearson")) # 상관계수 0.6675074
cor.test(data_fir$신장.5Cm단위.,
         data_fir$체중.5Kg단위.,
         method = c("pearson"))
# p-value < 2.2e-16  => p-value < 0.05 => 키와 몸무게는 상관관계이다.

# --------------------------------------------------------------

# 단순 선형회귀분석


sample = read.csv("sample.csv")
str(sample)
plot(happiness~money, data= sample)
abline(lm(happiness~money, data = sample), col = "red", lty = 4)

#sample = sample[c(-38,-39,-62),]
#sample = sample[c(-27,-12),]
sample.model = lm(happiness~money, data = sample)
library(car)
anova(sample.model) # p-value< 2.2e-16 => 대립가설 채택, 관계가 있다.
summary(sample.model) # Estimate: 1.03942

layout(matrix(c(1,2,3,4), nrow = 2))
plot(sample.model)
layout(1)
car::ncvTest(sample.model) # p = 0.73535 => 등분산
shapiro.test(sample.model$residuals) #p-value = 0.4816 => 정규분포
influencePlot(sample.model, id.method = "identify")

sample.new = data.frame(money=c(130,150))
predict(sample.model, newdata = sample.new)
# ---------------------------------------------------------------

# 다중 회귀분석

library(MASS)
birthwt_my = as.data.frame(MASS::birthwt)
str(birthwt_my)

library(psych)
describe(birthwt_my)
pairs.panels(birthwt_my)

#birthwt_my = birthwt_my[c(-11,-16,-226),]
library(car)
birthwt.model = lm(bwt~age+lwt+smoke+ptl+ht+ui, data = birthwt_my)
anova(birthwt.model) #p값이 0.05보다 큰 age, ptl 제거
summary(birthwt.model)   

birthwt.model_new = lm(bwt~lwt+smoke+ht+ui, data = birthwt_my)
anova(birthwt.model_new)
summary(birthwt.model_new)
vif(birthwt.model_new) # 10보다 모두 작음, 다중공선성문제 없음 

#표준화 작업
library(lm.beta)
lm.beta = lm.beta(birthwt.model_new)
summary(lm.beta)

# forword 변수제거
birthwt.model_new.f <- lm(bwt ~1, birthwt_my) 
summary(birthwt.model_new.f) 
birthwt.model_new.f <- step(birthwt.model_new.f, direction = "forward", 
                     scope =(bwt ~ lwt+smoke+ht+ui), 
                     trace = T)
library(car)
car::ncvTest(birthwt.model_new) # p = 0.24835 => 등분산 
shapiro.test(birthwt.model_new$residuals) # p-value = 0.8452 => 정규분포
car::durbinWatsonTest(birthwt.model_new) # p-value = 0 => 무언가 문제가 있음 (독립성 검정) 

library(car)
influencePlot(birthwt.model_new, id.method="identify")

# ----------------------------------------------------------------

# 로지스틱 회귀분석

#install.packages("survival")
library(survival)
colon = as.data.frame(survival::colon)
str(colon)
colon$status = factor(colon$status,
                      levels = c(0,1),
                      labels = c("생존", "재발 or 사망"))
library(psych)
describe(colon)
#pairs.panels(colon)

colon.model = glm(status ~ obstruct+perfor+adhere+nodes,
                  family = binomial,
                  data = colon)
summary(colon.model)

colon.model.new = glm(status ~ obstruct+adhere+nodes,
                  family = binomial,
                  data = colon)
summary(colon.model.new)

odds = data.frame(summary(colon.model.new)$coefficients,
                   odds = exp(coef(colon.model.new)))
round(odds, 5)






