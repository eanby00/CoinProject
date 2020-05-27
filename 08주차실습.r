# 동질성 검정 

income = read.csv("income_depression.csv")
str(income)
income

library(gmodels)
income.table = table(income$income,income$depression)
income.result = CrossTable(income.table,
                           expected = T,
                           chisq = T,
                           asresid = F)

income.result$chisq$stdres

0.923/1.551+0.214
#고소득이 우울증이 없을 확률은 고소득이 아닌 사람에 비해 0.80배로 나타났음

# ------------------------------------------------------------------------
# 독립성 검정 

tv = read.csv("tv_program.csv")
str(tv)
tv

library(gmodels)
tv.table = xtabs(count~성별+프로그램명,
                 data = tv)
tv.result = CrossTable(tv.table,
                       expected = T,
                       chisq = T,
                       asresid = F)

tv.result$chisq$stdres

#로봇카 폴리를 본 남자와 뽀로로를 본 남자의 비율 비교 
odds.ro = 20/70
odds.bo = 50/50
odds.ro/odds.bo
