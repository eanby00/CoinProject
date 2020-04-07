#1 도수분포표
grade = read.table("grade.txt", header = T)
t1 = table(grade)
t2 = prop.table(t1)
t3 = cbind(t1,t2)
graderesult = addmargins(t3,1)
graderesult

#2 분할표 

Titanicdata = read.csv("타이타닉.csv")
Titanicdata$Survived = factor(Titanicdata$Survived,levels = c(1,0), labels = c("생존", "사망"))
Titanicdata$Pclass = factor(Titanicdata$Pclass, levels = c(1,2,3), labels = c("1st","2st","3st"))

titanic1 = table(Survived = Titanicdata$Survived,Sex = Titanicdata$Sex)
t1data = matrix(titanic1)

titanic2 = table(Survived = Titanicdata$Survived,Pclass = Titanicdata$Pclass)
t2data = matrix(titanic2)




attach(Titanicdata)
library(gmodels)

titanicd1 = xtabs(t1data ~ Survived+Sex,data = titanic1)
CrossTable(titanicd1)

titanicd2 = xtabs(t2data ~ Survived+Pclass,data = titanic2)
CrossTable(titanicd2)

detach(Titanicdata)

