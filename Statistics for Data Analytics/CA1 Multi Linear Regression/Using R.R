#install.packages("haven")
#install.packages("leaps")
#install.packages("car")
library(haven)
library(leaps)
library(car)
setwd("D:\\NCI\\Sem 1\\Statistics\\CA1 Multi Linear Regression")
dataset = read_sav("StatsCA1SPSSFile.sav")

head(dataset)
str(dataset)
attach(dataset)
lmfit1 <- lm(Lifeexpectancy~Adultmortalityrate+Infantmortalityrate+Crudesuiciderate+NCDmortalityrate) 
summary(lmfit1)
par(mfrow=c(2,2))
plot(lmfit1)

#Interaction effect
lmfit2 <- lm(Lifeexpectancy~Adultmortalityrate*Infantmortalityrate*Crudesuiciderate*NCDmortalityrate) 
summary(lmfit2)
par(mfrow=c(1,1))
plot(lmfit2)



library(car)
ncvTest(lm(Lifeexpectancy~Adultmortalityrate+Infantmortalityrate+Crudesuiciderate+NCDmortalityrate))
bptest(lmfit1)

durbinWatsonTest(lmfit1)

vif(lmfit1)

cooks.distance(lmfit1)
influencePlot(lmfit1)

Bestfit <- regsubsets(Lifeexpectancy~Adultmortalityrate+Infantmortalityrate+Crudesuiciderate+NCDmortalityrate, data =dataset, nbest =2)
par(mfrow=c(1,1))
subsets(Bestfit,statistic = 'adjr2')
plot(Bestfit,scale="adjr2")

?subsets
