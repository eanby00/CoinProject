csvdata = read.csv("학력과 취업의 관계(사후설계 분할표).csv")
str(csvdata)
csvdata$academic_background =  factor(csvdata$academic_background, levels = c(1,2,3), labels = c("univ","college","hisc"))
csvdata$employ = factor(csvdata$employ, levels = c(0,1), labels = c("Failure", "Success"))

attach(csvdata)
dummy_csv = table(employ, academic_background)
dummy_csv2 = table(employ, academic_background)[1,]
dummy_csv1 = table(employ, academic_background)[2,]

layout(matrix(c(1,2),1,2,byrow = F))
barplot(dummy_csv1, main = "취업 성공", xlab = "학력", ylab = "명", col = rainbow(3))
legend(1.5,20,colnames(dummy_csv),cex = 0.8,fill = rainbow(3) )
barplot(dummy_csv2, main = "취업 실패", xlab = "학력", ylab = "명", col = rainbow(3))
legend(1.5,20,colnames(dummy_csv),cex = 0.8,fill = rainbow(3) )
layout(1)

barplot(t(dummy_csv), main = "학력에 따른 취업률", xlab = "취업 현황", ylab = "명", col = rainbow(3))
legend(2,170,colnames(dummy_csv),cex = 0.8,fill = rainbow(3) )

barplot(t(dummy_csv), main = "학력에 따른 취업률", xlab = "취업 현황", ylab = "명", col = rainbow(3), beside = T)
legend(5,60,colnames(dummy_csv),cex = 0.8,fill = rainbow(3) )

barplot(t(dummy_csv), main = "학력에 따른 취업률", xlab = "취업 현황", ylab = "명", col = rainbow(3), beside = T, horiz = T)
legend(50,6,colnames(dummy_csv),cex = 0.8,fill = rainbow(3) )

layout(matrix(c(1,2,3,4),2,2,byrow = T))
pie(dummy_csv1,main = "취업 성공",init.angle = 90,col = rainbow(3))
legend(0.5,1.05,colnames(dummy_csv),cex = 0.5, fill = rainbow(3))
pie(dummy_csv1,main = "취업 실패",init.angle = 90,col = rainbow(3))
legend(0.5,1.05,colnames(dummy_csv),cex = 0.5, fill = rainbow(3))
library(plotrix)
pie3D(dummy_csv1,col = rainbow(3),labels = dummy_csv1,explode = 0.1)
legend(0.5,1.05,colnames(dummy_csv),cex = 0.5, fill = rainbow(3))
pie3D(dummy_csv2,col = rainbow(3),labels = dummy_csv2,explode = 0.1)
legend(0.5,1.05,colnames(dummy_csv),cex = 0.5, fill = rainbow(3))
layout(1)

mosaicplot(t(dummy_csv), shade = T, main = "학력에 따른 취업률", xlab = "학력", ylab = "취업 현황")

detach(csvdata)


seconddata = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Diamond.csv")
stem(seconddata$price)

layout(matrix(c(1,2),2,1))
hist(seconddata$price)
boxplot(seconddata$price)
layout(1)

price = transform(seconddata$price,
                  price_cut = cut(seconddata$price,
                              breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000),
                              right = F,
                              labels = c("0초과~1000이하","1000초과~2000이하","2000초과~3000이하","3000초과~4000이하","4000초과~5000이하",
                                         "5000초과~6000이하","6000초과~7000이하","7000초과~8000이하","8000초과~9000이하","9000초과~10000이하",
                                         "10000초과~11000이하","11000초과~12000이하","12000초과~13000이하","13000초과~14000이하","14000초과~15000이하",
                                         "15000초과~16000이하")))
barplot(table(price$price_cut))

hist(seconddata$price,
     breaks = 17,
     col = "cyan",
     freq = F)

boxplot(seconddata$price~seconddata$colour, xlab = "색 등급", ylab = "$", main = "색깔 등급에 따른 가격 분포")

library(dplyr)
new_dummy = seconddata %>% group_by(clarity) %>% summarise(pricemean = mean(price))
barplot(new_dummy$pricemean~new_dummy$clarity, main = "투명도 등급별 가격평균", ylim = c(0,10000),col = "darkblue")

library(ggplot2)
ggplot(seconddata, aes(x = certification))+geom_bar() +ggtitle("인증기관별 다이아몬드수")
ggplot(seconddata, aes(x = certification,y = carat))+geom_boxplot() +ggtitle("인증기관별 다이아몬드 무게도표")

