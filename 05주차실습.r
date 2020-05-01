# p-value < 0.05 => 대립가설 채택 // 다르다
# p-value > 0.05 => 귀무가설 채택 // 같다 

#one sample test

#A회사의 건전지의 수명시간이 1000시간 일 때, 무작위로 뽑은 10개의 건전지에 대한 수명은 다음과 같다.

sample_bat = c(980,1008,968,1032,1012,996,1021,1002,996,1017)
library(psych)
describe(sample_bat)
t.test(sample_bat,
       alternative = c("two.sided"),
       mu = 1000,
       conf.level = 0.95)
# p-value = 0.611
# 0.05보다 큼 => 귀무 가설 채택 , 샘플은 모집단과 같다고 할 수 있다.
mu = 1000
se = 6.07
inter = qt(p = 0.025, df = 9)
data = rnorm(1000,mu,se)
data = sort(data)
plot(data,dnorm(data,mu,se),
     type = "l")
abline(v = mu, lty = 5, col = "red")
abline(v = mu + inter*se, lty = 5, col = "blue")
abline(v = mu - inter*se, lty = 5, col = "blue")
abline(v = mean(sample_bat), lty = 5, col = "red")

#3-1반의 학생들의 수학 평균성적은 55점이었다. 보충 수업을 시행하고나서 학생들의 시험 성적은 다음과 같았다.
#<58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39>
#보충 수업을 시행한 후, 학생들의 성적은 올랐다고 할 수 있는가?
sample_score = c(58,49,39,99,32,88,62,30,55,65,44,55,57,53,88,42,39)
describe(sample_score)  
t.test(sample_score,
       alternative = c("greater"),
       mu = 55,
       conf.level = 0.95)
# p-value = 0.4046
# 0.05보다 큼 => 귀무가설 채택, 학생들의 성적은 올랐다고 볼 수 없다.
mu = 55
se = 4.79
inter = qt(p = 0.05, df = 16,lower.tail = F)
data = rnorm(1000,mu,se)
data = sort(data)
plot(data,dnorm(data,mu,se),
     type = "l")
abline(v = mu, lty = 5, col = "red")
abline(v = mu + inter*se, lty = 5, col = "blue")
abline(v = mean(sample_score), lty = 5, col = "red")

# 가설을 세워보기
# 소비습관에 관한 책을 읽기 전에 대상 집단의 평균 소비 금액은 달에 50만원이었다.
# 책을 읽은 후에는 집단의 소비 금액은 다음과 같았다.
# <25,20,30,14,16,40,10,9,16,12,13,15,20>
# 책을 읽은 후 집단의 평균 소비 금액은 감소했다고 할 수 있는가?

sample_price = c(25,20,30,14,16,40,10,9,16,12,13,15,20)
describe(sample_price)
t.test(sample_price,
       alternative = c("less"),
       mu = 50,
       conf.level = 0.95)
# p-value = 1.005e-08
# 0.05보다 작음 => 대립가설 채택, 평균 소비 금액은 감소했다.
mu = 50
se = 2.43
inter = qt(p = 0.05, df = 12,lower.tail = T)
data = rnorm(1000,mu,se)
data = sort(data)
plot(data,dnorm(data,mu,se),
     type = "l",
     xlim = c(10,90))
abline(v = mu, lty = 5, col = "red")
abline(v = mu + inter*se, lty = 5, col = "blue")
abline(v = mean(sample_price), lty = 5, col = "red")

# -------------------------------------------------------------------

#independent t test
#두 반의 영어 점수를 모아둔 자료 class를 읽고 가설을 세워서 독립표본 t검정으로 풀어보기
# 가설: 두 반의 영어 점수는 같다고 할 수 있을까?
# 귀무가설: 두 반의 영어 점수는 같다.
# 대립가설: 두 반의 영어 점수는 다르다.
class = read.csv("class.csv")
str(class)
describe(class$class1)
describe(class$class2)

layout(matrix(c(1,2), nrow = T))
boxplot(class$class1)
boxplot(class$class2)
layout(1)

var.test(class$class1,class$class2)
# p-value = 0.6183
# 0.05보다 큼 => 등분산 
t.test(class$class1, class$class2,
       alternative = c("two.sided"),
       var.equal = T,
       conf.level = 0.95)

# p-value = 0.6041
# 0.05보다 큼 => 귀무가설 채택 두 반의 영어 점수는 같다.
mu_class1 = 81.5
se_class1 = 2.73
data = rnorm(1000,mu_class1,se_class1)
data = sort(data)
plot(data, dnorm(data, mu_class1, se_class1),
     col = "blue", type ="l",
     xlim = c(70,90), ylim = c(0.00, 0.15))
abline(v = mu_class1,col = "blue", lty = 3)

par(new = T)
mu_class2 = 79.35
se_class2 = 3.07
data = rnorm(1000,mu_class2,se_class2)
data = sort(data)
plot(data, dnorm(data, mu_class2, se_class2),
     col = "red", type ="l",
     xlim = c(70,90), ylim = c(0.00, 0.15))
abline(v = mu_class2,col = "red", lty = 3)

#비모수통계분석: 데이터가 적을 경우, 정규분포를 쓸 수 없을 경우 => 불가능 예상
shapiro.test(class$class1)
shapiro.test(class$class2)
wilcox.test(class$class1,class$class2)
# p-value = 0.4895 단 경고메시지 출력, tie가 있어 정확한 p값을 계산할 수 없습니다.

#ggplot 그래프
# 전처리 과정 
class1 = data.frame(class$class1)
class1 = cbind(class1, c("class1"))
names(class1) = c("score","class")
class2 = data.frame(class$class2)
class2 = cbind(class2, c("class2"))
names(class2) = c("score","class")
newclass = rbind(class1, class2)

library(ggplot2)
ggplot(newclass, aes(score, color = class)) +
  geom_histogram()

# -----------------------------------------------------------------------------------

# paired t test
# 탈모약 복용 전 후의 발모량을 모아둔 자료 TakingMedicine을 읽고 대응표본 t검정으로 풀어보기
# 가설 : 탈모약 복용은 탈모에 아무런 효과도 없을 것이다.
# 귀무가설 : 전과 후의 차이 없음
# 대립가설 : 전과 후의 차이 있음 
medicine = read.csv("TakingMedicine.csv")

library(psych)
describe(medicine)
dip = c(medicine$after-medicine$before)
describe(dip)
t.test(medicine$after, medicine$before,
       alternative = c("two.sided"),
       paired = T,
       conf.level = 0.95)
# p-value = 0.02933
# 0.05보다 작음 => 대립 가설 채택, 전과 후의 차이 있음

mu = 0
se = 1120.01
inter = qt(p =0.025, df = 49)
data = rnorm(1000,mu,se)
data = sort(data)
plot(data, dnorm(data,mu,se),
     type = "l")
abline(v = mu, col = "red", lty = 5)
abline(v = mu+inter*se, col = "blue", lty = 5)
abline(v = mu-inter*se, col = "blue", lty = 5)
abline(v = 2514.2, col = "red", lty = 5)

