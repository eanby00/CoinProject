#1 40%의 확률로 10번 중 4번 성공할 확률을 구하세요.
dbinom(4,10,0.4)

#2 시행횟수: 0~10, 성공확률 60%의 이항분포 그래프를 그리시오.
#type = h, lwd = 10, color = blue, xlab = "성공 확률X", ylab = "확률", main = "coin 이항분포"
plot(0:10, dbinom(0:10, 10,0.6), 
     type='h', 
     lwd=10, 
     col="blue", 
     xlab="성공 확률 X ", 
     ylab="확률",
     main = "coin 이항분포")

#3 2번 그래프에서 P(X >= 3)의 확률을 구하시오.
pbinom(2, 10, 0.6, lower.tail = F)


#4 2번과 똑같은 조건으로 누적 이항분포표를 그리시오.
plot(pbinom(0:10, 10, 0.4),
     type='h', 
     lwd=10, 
     col="blue", 
     main ="누적이항분포")

#5 발생건수 0~10, 단위시간당 평균발생 건수 = 2.5의 포아송분포 확률밀도 함수를 그리시오.
#type = h, lwd = 10, color = blue, xlab = "성공 확률X",main = "포아송분포"
plot(dpois(0:10,2.5),
     type='h', 
     lwd=10, 
     col="blue", 
     xlab="성공 확률 X ", 
     main = "포아송분포")

#6 콜센터에서 10분에 평균 4회 전화, 3분 이내일 확률 P(X<=3/10)이내로 받을 확률을 구하시오 
pexp(3/10, 4, lower.tail = T)

#7 정규분포 그리기
layout(matrix(c(1,2)))
x = rnorm(10000, 500, 500)
plot(x,dnorm(x,500,500))
abline(v = 500,col = "red",lty = 1)
abline(v = 500+1.96*500, col = "blue", lty = 1)
abline(v = 500-1.96*500, col = "blue", lty = 1)

mean = mean(x)
sd = sd(x)
plot(x,dnorm(x,mean,sd))
abline(v = mean,col = "red",lty = 1)
abline(v = mean+1.96*sd, col = "blue", lty = 1)
abline(v = mean-1.96*sd, col = "blue", lty = 1)

layout(1)
plot(x,dnorm(x,mean,sd))
abline(v = mean,col = "red",lty = 1)
abline(v = mean+1.96*sd, col = "blue", lty = 1)
abline(v = mean-1.96*sd, col = "blue", lty = 1)

plot(x,dnorm(x,500,300))
abline(v = 500,col = "red",lty = 1)
abline(v = 500+1.96*300, col = "blue", lty = 1)
abline(v = 500-1.96*300, col = "blue", lty = 1)

y = rnorm(100000, 500, 500)
plot(y,dnorm(y,500,500))
mean_y = mean(y)
sd_y = sd(y)
