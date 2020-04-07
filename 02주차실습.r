#산술평균
x = read.csv("average.csv")
str(x$x)
boxplot(x$x)

mean(x$x,na.rm = T) #513.069
median(x$x,na.rm = T) #508
summary(x$x)
mean(x$x,trim= 0.1) #513.7825

#가중평균 
y = read.csv("weighted average.csv")
weighted.mean(y$y2,y$y1) #48.47722
weighted.mean(y$y1,y$y2) #45.2847

#기하평균
corona = read.csv("corona.csv")
str(corona)
#install.packages("psych")
library(psych)

cagr1_2 = c(1:24)
for (i in 1:24) {
  cagr1_2[i] = (corona$confirmed_case[i+1]/corona$confirmed_case[i])
}
cagr1_2
geometric.mean(cagr1_2)-1 #0.4036559

#조화평균
library(psych)
harmonic.mean(c(7,5)) #5.833333
harmonic.mean(c(900,200)) #327.2727

#분포
DMB = read.csv("DMB재난경보방송발령현황(2020년).csv")
library(dplyr)
DMB = DMB %>% group_by(지역) %>% summarise(n = n())
DMB
min(DMB$n) #1
max(DMB$n) #78
diff(range(DMB$n)) #77
quantile(DMB$n, c(0.25,0.5,0.75,1.0)) # 2,6,13,78
var(DMB$n) #152.5369
sd(DMB$n) #12.35058
skew(DMB$n) #2.477965
kurtosi(DMB$n) #7.984548
DMB$n.z = scale(DMB$n)
tail(DMB)

#다변량
health = read.csv("건강검진정보.csv")
health$성별코드 = factor(health$성별코드, levels = c("1","2"),labels = c("남자","여자"))
attach(health)
library(psych)
summary(총콜레스테롤)
describe(총콜레스테롤)
boxplot(총콜레스테롤)
health = health[!(총콜레스테롤 > 300),]
detach(health)
attach(health)
boxplot(총콜레스테롤)
describe(총콜레스테롤)
tapply(총콜레스테롤,성별코드,summary)
library(psych)
describeBy(health[c("총콜레스테롤")],성별코드, mat=T)
detach(health)

