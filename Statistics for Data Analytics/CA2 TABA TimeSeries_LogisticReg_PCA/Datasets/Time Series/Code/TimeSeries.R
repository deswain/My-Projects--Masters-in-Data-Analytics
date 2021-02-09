library(fpp2)
plot(wmurders)

head(wmurders)
wmurders

setwd("D:\\NCI\\Sem 1\\Statistics\\CA2 TABA TimeSeries_LogisticReg_PCA\\Datasets\\Time Series\\Code")
Values <- read.csv('IndustryTotMonthlySales.csv')
Values

plot(Values)

ts1 <- ts(Values, start= 1985, frequency =12)
ts1
plot(ts1)
autoplot(ts1)


ggseasonplot(ts1,year.labels = TRUE, year.labels.left = TRUE)+
  ylab("$ Sale") +
  ggtitle("Seasonal plot: Industry sale")

ggsubseriesplot(ts1)+
  ylab("$ Sale") +
  ggtitle("Seasonal plot: Industry sale")



# Smoothing the Time series using simple moving average
plot(ma(ts1,5)) # Always odd numbers are taken

plot(ma(ts1,7))

plot(ma(ts1,15))


autoplot(ts1)+
  autolayer(ma(ts1,3))+
  autolayer(ma(ts1,7))


ggtsdisplay(ts1)



#Seasonal Decomposition using Decompose() - Additive
fit.decadd <- decompose(ts1, type = "additive")
fit.decadd
plot(fit.decadd)


#Seasonal Decomposition using Decompose() - Multiplicative [not appropriate for my Time Series]
fit.decmul <- decompose(ts1, type = "multiplicative")
fit.decmul
plot(fit.decmul)

monthplot(ts1)



# Simple Exponential Smoothing
sesFit <- ses(ts1, h=2)                                        
sesFit

summary(sesFit)                                               # RMSE : 3.669027, AIC : 3741.029 

round(accuracy(sesFit),2)

autoplot(sesFit)

autoplot(sesFit)+autolayer(fitted(sesFit),series = "Fitted")



# Holt's Linear Trend Method
airFit <- holt(ts1)

summary(airFit)                                                # RMSE = 3.6698 , AIC : 3745.234


airFit <- holt(ts1, h= 5)
airFit


round(accuracy(airFit),2)

# Holt- Winters Model

hwFit1 <- hw(ts1, seasonal = "additive")
hwFit2 <- hw(ts1, seasonal = "multiplicative") # Throws error as it is inappropriate


summary(hwFit1)                                                # RMSE = 3.667565 , AIC: 3768.686

autoplot(ts1)+
  autolayer(hwFit1, series = "HW additive forecast", PI=FALSE)

round(accuracy(hwFit1),2)





# ets model 
# Model = "ZZZ" -> first letter error , 2nd for trend, 3rd for seasonality

etsFit <- ets(ts1, model="ZZZ")
summary(etsFit)                  # RMSE = 3.63823


etsFit_1 <- ets(ts1, model = "ANN")
summary(etsFit_1) # Same result as ZZZ. But it is taking trend as A which doesn't show right.


etsFit_2 <- ets(ts1, model = "ANA")
summary(etsFit_2)


# Mean model
meanFit <-meanf(ts1) 
summary(meanFit)                                             # RMSE = 11.09989 , AIC: NA
AIC(meanFit)

# Naive Model
naiveFit <- naive(ts1)
summary(naiveFit)                                            # RMSE = 3.673285 , AIC: NA
AIC(naiveFit)

# Seasonal Naive Model
sNaiveFit <- snaive(ts1)
summary(sNaiveFit)                                           # RMSE = 14.783 ,  AIC: NA
AIC(sNaiveFit)






#####################    ARIMA    ##############
library(tseries)
# ADF Test
adf.test(ts1)  # Showing stationary which is required.

ndiffs(ts1)

# The adf test on original time series is showing that it is stationary.
# So, no need to perform the adf test on differenced time series
dts1 <- diff(ts1)
plot(dts1)
adf.test(dts1)

ndiffs(ts1)

# same as ts1. Also ndiff(ts1) gave 0 which mean not difference i needed for the model.
# This also means d value in ARIMA(p,d,q) is 0 here.



# Auto-correlation plot (ACF) and Partial Auto-correlation plot (PCF)
# Choosing the value of p and q for  ARIMA model

Acf(ts1)
Pacf(ts1)

# In my case, huge number of spikes are seen in ACF and PCF

# Fitting an ARIMA model

arimaFit <- arima(ts1, order = c(2,0,3)) 
summary(arimaFit)                           ############### RMSE = 3.4927
checkresiduals(arimaFit)
auto.arima(ts1)  

# ARIMA(2,0,3) gives better result with minimum RMSE.

arimaFit1 <- arima(ts1, order = c(3,0,2))   ############ RMSE = 3.567713
summary(arimaFit1)





###### Evaluating Model fit ######
qqnorm(arimaFit$residuals)
qqline(arimaFit$residuals)

Box.test(arimaFit$residuals, type= "Ljung-Box")  # p-value = 0.7447
# The above test gave p-value as 0.7447 which is insignificant.
# Thi means, autocorrelations don't differ from zero and ARIMA model appears to fit the data well

checkresiduals(arimaFit) 
# Visually it is approximately looking like normal distribution of residuals.
# Also the ACF plot doesn't show spikes in the initial lags. But there are few spikes.

  ############## Need to read in details from below link ########
  #   http://people.duke.edu/~rnau/Notes_on_nonseasonal_ARIMA_models--Robert_Nau.pdf

accuracy(arimaFit)



# Forecasting with the fitted model
forecast(arimaFit, 3)
plot(forecast(arimaFit, 3), xlab = "Year", ylab = "Annual Flow")

autoplot(forecast(arimaFit, 3), xlab = "Year", ylab = "Annual Flow")


######## Auto ARIMA #######
# Automatically finds the best fit model
auto.arima(ts1)

'''
O/p of auto ARIMA:  ARIMA(2,0,3)(0,0,2)[12] with zero mean
                     This shows the effect of Seasonality order (0,0,2)
                    It will be exciting to apply the SARIMA model to see the effectiveness

'''

################### Seasonal ARIMA Model ########
SarimaFit <- arima(ts1, order = c(3,0,3), seasonal = c(0,1,1))
summary(SarimaFit)                                             # RMSE = 2.762312 
checkresiduals(SarimaFit)

SarimaFit2 <- arima(ts1, order = c(2,0,3), seasonal = c(0,0,2))  
summary(SarimaFit2)                                             


#############################################################################################
######### Final SARIMA Model #########33
SarimaFit2 <- arima(ts1, order = c(3,0,2), seasonal = c(0,0,1))  
summary(SarimaFit2)
checkresiduals(SarimaFit2)

##############################################################################################


#############################################################################################
                     ###     Forcasting Using SARIMA Model ########


fcast.Sarima <- forecast(SarimaFit2, h=4, find.frequency= TRUE)  
autoplot(fcast.Sarima)

library(dplyr)
fcast.Sarima<- SarimaFit2 %>% forecast(h=5,find.frequency=TRUE)


SarimaFit2 %>% forecast(find.frequency=TRUE) %>% autoplot()


ts1
#install.packages("sarima")
#library(sarima)
#smodel <- sarima(ts1,3,0,2,0,0,1,12)
#?sarima
#sarima.for(ts1, 24, 3,0,2,0,0,1,12)

























?forecast()




Box.test(SarimaFit2$residuals, type= "Ljung-Box")



accuracy(SarimaFit2)


# Try with few more seasonal values and compare.

checkresiduals(ts1)
