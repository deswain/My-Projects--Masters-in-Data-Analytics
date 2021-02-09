######## Multilinear Regression on King County Datset for House price Prediction#######


setwd("D:\\NCI\\Sem 1\\DMML\\Datasets\\King Country House Dataset")

#Reading the dataset
kcdataset <- read.csv("kc_house_data.csv")
#Seeing the type of kcdataset
class(kcdataset)
#Seeing the top 6 rows
head(kcdataset)
#Seeing the structure of the dataset
str(kcdataset) # 21597 obs. of  21 variables
#Seeing the summary of the dataset
summary(kcdataset)

# Correlation matrix. This will throw error as one column is not numeric. 
library(corrplot)
cor(kcdataset)


#Dropping the columns 'id'(index 1), 'date' (Index 2)
kcdataset_1 <- kcdataset[-c(1,2)] 
head(kcdataset_1)
str(kcdataset_1) # 21597 obs. of  19 variables



#Converting factor datatype to integer for the column kcdataset_1. Just to see the correlation
table(kcdataset_1$view)
kcdataset_1$view <- as.integer(kcdataset_1$view)


#Correlation matrix along with the significance level(p-value)
#install.packages("Hmisc")
library("Hmisc")
rcorr(as.matrix(kcdataset_1))

# Correlation matrix along with plot
cor(kcdataset_1)
par(mfrow =c(1,1))
newdatacor = cor(kcdataset_1[1:19])
corrplot(newdatacor, method = "number")

#################
# From Correlation matrix, all the 18 Columns are showing significant values(p-value < 0.05) w.r.t Price Column. 
# So These columns cannot be dropped for now.
#################


# Checking if any column has any missing values
sapply(kcdataset_1, function(x) sum(is.na (x))) # No columns have any missing values




# Converting categorical column data types to factors
table(kcdataset_1$bedrooms)
kcdataset_1$bedrooms <- as.factor(kcdataset_1$bedrooms)

table(kcdataset_1$bathrooms)
kcdataset_1$bathrooms <- as.factor((kcdataset_1$bathrooms))

table(kcdataset_1$floors)
kcdataset_1$floors <- as.factor(kcdataset_1$floors)

table(kcdataset_1$waterfront)
kcdataset_1$waterfront <- as.factor(kcdataset_1$waterfront)

table(kcdataset_1$view)
kcdataset_1$view <- as.factor(kcdataset_1$view)

table(kcdataset_1$condition)
kcdataset_1$condition <- as.factor(kcdataset_1$condition)

table(kcdataset_1$zipcode)
kcdataset_1$zipcode <- as.factor(kcdataset_1$zipcode)

table(kcdataset_1$yr_built)
kcdataset_1$yr_built <- as.factor(kcdataset_1$yr_built)

table(kcdataset_1$yr_renovated)
kcdataset_1$yr_renovated <- as.factor(kcdataset_1$yr_renovated)

table(kcdataset_1$grade)
kcdataset_1$grade <- as.factor(kcdataset_1$grade)

#Checking the structure of the datasets after changing the datatypes of few columns to 
str(kcdataset_1) # 21597 obs. of  19 variables




################################# DUMMY COLUMNS   ###############################################
# Since there are multipe features with factor type, dummy columns need to be created for those
# Features: bedrooms, bathrooms, floors, Waterfront, View, Condition 

#####DUMMY Columns for Categorical Columns #####
table(kcdataset_1$bedrooms)  # 12 Categories # Dummy columns needed: 12-1 = 11
table(kcdataset_1$bathrooms) # 29 Categories # Dummy Columns needed: 29-1 = 28
table(kcdataset_1$floors)    # 6 Categories  # Dummy columns required 6-1 = 5 
table(kcdataset_1$waterfront)# 2 categories  # No Dummies required as it has binary type 
table(kcdataset_1$view)      # 5 categories  # Dummy Columns required  5-1 = 4
table(kcdataset_1$condition) # 5 categories  # Dummy Columns required  5-1 = 4
table(kcdataset_1$zipcode)   # 70 categories # Dummy columns required 70-1 =69
table(kcdataset_1$yr_built)
table(kcdataset_1$yr_renovated)
table(kcdataset_1$grade)
#Dummy Variable creation
#install.packages("fastDummies")
#kcdataset_2 <- fastDummies::dummy_cols(kcdataset_1, remove_first_dummy = TRUE)
# Tried the above command to create  dummies but it also created dummies for binary data column 'waterfront'.
#So, I will be changing the dtatype of waterfront to integer before creating dummies.
kcdataset_1$waterfront <- as.integer(kcdataset_1$waterfront)
#Previously, the datatype of view column was changed to int to see correlation. Changing it back to int.


#Creating Dummies
kcdataset_2 <- fastDummies::dummy_cols(kcdataset_1, remove_first_dummy = TRUE)
kcdataset_2
str(kcdataset_2) # 21597 obs. of  334 variables:

# Dropping the original columns since dummy columns are created
kcdataset_3 <- subset(kcdataset_2, select = -c(bedrooms,bathrooms,floors,view,condition,zipcode,
                                               yr_built,yr_renovated,grade))
str(kcdataset_3)  # 21597 obs. of  325 variables


# Correlation matrix and p- values
rcorr(as.matrix(kcdataset_3))

# From the matrix, the p-values of some columns are not showing significant values w.r.t price, i.e,  P>0.05
# Hence, these columns will be dropped in the next step

kcdataset_4 <- subset(kcdataset_3, select = -c(bedrooms_10,bedrooms_11,bedrooms_33,bathrooms_1.25,
                                               bathrooms_2.25,bathrooms_7.5,condition_3,grade_8,yr_built_1901,
                                               yr_built_1902,yr_built_1903,yr_built_1904,yr_built_1908,
                                               yr_built_1912,yr_built_1913,yr_built_1914,yr_built_1915,
                                               yr_built_1916,yr_built_1917,yr_built_1918,yr_built_1919,
                                               yr_built_1920,yr_built_1921,yr_built_1922,yr_built_1923,
                                               yr_built_1924,yr_built_1929,yr_built_1930,yr_built_1932,
                                               yr_built_1934,yr_built_1935,yr_built_1936,yr_built_1938,
                                               yr_built_1939,yr_built_1940,yr_built_1941,yr_built_1946,
                                               yr_built_1951,yr_built_1952,yr_built_1964,yr_built_1965,
                                               yr_built_1972,yr_built_1973,yr_built_1974,yr_built_1975,
                                               yr_built_1976,yr_built_1982,yr_built_1983,yr_built_1984,
                                               yr_built_1985,yr_built_1987,yr_built_1990,yr_built_1992,
                                               yr_built_1993,yr_built_1995,yr_built_2002,yr_built_2003,
                                               yr_built_2009,yr_built_2010,yr_built_2011,yr_built_2012,
                                               yr_renovated_1934,yr_renovated_1940,yr_renovated_1944,
                                               yr_renovated_1945,yr_renovated_1946,yr_renovated_1948,
                                               yr_renovated_1950,yr_renovated_1951,yr_renovated_1953,
                                               yr_renovated_1954,yr_renovated_1955,yr_renovated_1956,
                                               yr_renovated_1957,yr_renovated_1958,yr_renovated_1959,
                                               yr_renovated_1960,yr_renovated_1962,yr_renovated_1963,
                                               yr_renovated_1964,yr_renovated_1965,
                                               yr_renovated_1968,yr_renovated_1969,yr_renovated_1970,
                                               yr_renovated_1971,yr_renovated_1972,yr_renovated_1973,
                                               yr_renovated_1974,yr_renovated_1975,yr_renovated_1976,
                                               yr_renovated_1977,yr_renovated_1978,yr_renovated_1979,
                                               yr_renovated_1980,yr_renovated_1981,yr_renovated_1983,
                                               yr_renovated_1984,yr_renovated_1985,yr_renovated_1986,
                                               yr_renovated_1988,yr_renovated_1989,yr_renovated_1992,
                                               yr_renovated_1997,yr_renovated_2011,yr_renovated_2012,
                                               yr_renovated_2015,zipcode_98011,zipcode_98024,zipcode_98034,
                                               zipcode_98065,zipcode_98070,zipcode_98072,zipcode_98107,
                                               zipcode_98136))
str(kcdataset_4) # 21597 obs. of  211 variables


# Rechecking if there are any more insignificant features
# Correlation matrix and p- values
rcorr(as.matrix(kcdataset_4)) 


# No more insignificant columns are seen in the dataset. 
#Also, we have already found out that there are no missing values available in the dataset


############################ Splitting the dataset "kcdataset_3" into two parts for training and testing###################
#install.packages("caTools")
library(caTools)
?sample
set.seed(111)   #  seed is used so that we will get the same random numbers generated all the time
sample = sample.split(kcdataset_4,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. 
#After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
kctrain =subset(kcdataset_4,sample ==TRUE) # creates a training dataset named kctrain1 with rows which are marked as TRUE
kctest=subset(kcdataset_4, sample==FALSE)  ## creates a training dataset named kctest1 with rows which are marked as False

################################################################3
str(kctrain) # 16179 obs. of  211 variables
str(kctest)  # 5418 obs. of  211 variables

#Datset is now split into two parts. kctrain(75% of dataset) and kctest1(25% of dataset)
model1 <- lm(price~., data = kctrain)
summary(model1)  # Multiple R-squared:  0.8518,	Adjusted R-squared:  0.8498
par(mfrow=c(2,2))
plot(model1)


############# Strange Observation ##################
'''
For some columns (eg: sqft_basement, bathrooms_6.25, bathrooms_6.5), NA values are received for coefficients,
p-value, std. Error and t value.

References: 
https://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient
https://discuss.analyticsvidhya.com/t/linear-regression-in-r-coefficients-having-na-in-summary-model/64624

As per the above url, this kind of observation indicates that these features are linearly related
to one or more features. So, these features are not adding any values to the model, i.e.,
the information which can be received from these features(showing NA values), are already being 
received through other variables.

Also, there are insignificant columns which need to be dropped.


Hence, these features will be removed in the next step. Then, the model will be aplied.

'''

str(kctrain) # 16157 obs. of  119 variables
kctrain_1 <- subset(kctrain, select = -c(sqft_basement,bedrooms_2,bedrooms_3,bathrooms_0.75,
                                         bathrooms_1.75,bathrooms_2,bathrooms_8,floors_3.5,condition_2,grade_4,
                                         yr_built_1906,yr_built_1907,yr_built_1909,yr_built_1910,yr_built_1925,
                                         yr_built_1927,yr_built_1928,yr_built_1931,
                                         yr_built_1937,yr_built_1942,yr_built_1943,yr_built_1944,
                                         yr_built_1945,yr_built_1947,yr_built_1948,yr_built_1949,
                                         yr_built_1950,yr_built_1956,yr_built_1958,
                                         yr_built_1960,yr_built_1961,yr_built_1962,yr_built_1963,
                                         yr_built_1968,yr_built_1969,yr_built_1970,
                                         yr_built_1971,yr_built_1980,yr_built_1981,yr_built_1986,
                                         yr_built_1988,yr_built_1989,yr_built_1994,
                                         yr_built_1996,yr_built_1997,yr_built_1998,yr_built_2000,
                                         yr_built_2001,yr_built_2004,yr_built_2007,yr_built_2008,yr_built_2015,
                                         yr_renovated_1967,yr_renovated_1982,yr_renovated_1987,
                                         yr_renovated_1990,yr_renovated_1991,yr_renovated_1995,
                                         yr_renovated_1996,yr_renovated_1998,
                                         yr_renovated_2000,yr_renovated_2001,
                                         zipcode_98002,zipcode_98014,
                                         zipcode_98038,zipcode_98059,zipcode_98118,zipcode_98126))

str(kctrain_1) # 16179 obs. of  143 variables

#Running the lm for kctrain_1
model2 <- lm(price~., data = kctrain_1)
summary(model2) # Multiple R-squared:  0.8296,	Adjusted R-squared:  0.8282
                   

library(car)                   
ncvTest(model2) # p value < 0.05. not good

durbinWatsonTest(model2) # D-w Stat value close to 2 which is good.         

vif(model2)

# Some features are having VIF values more than 5. Hence, removing those.
kctrain_2 <- subset(kctrain_1, select = -c(sqft_living, sqft_above)) 
model3 <- lm(price~., data= kctrain_2)
summary(model3) # Multiple R-squared:  0.8219,	Adjusted R-squared:  0.8204


# Few more columns are showing insignificant values. Dropping those.
kctrain_3 <- subset(kctrain_2, select = -c(sqft_lot15,bedrooms_7,bedrooms_9,floors_2,
                                           yr_built_1926,yr_built_1933,yr_built_1955,
                                           yr_built_1957,yr_built_1959,yr_built_1967,yr_built_1991,yr_built_1999,
                                           yr_built_2013,yr_renovated_2006,zipcode_98148))

model4 <- lm(price~., data = kctrain_3)
summary(model4) # Multiple R-squared:  0.8215,	Adjusted R-squared:  0.8201
par(mfrow=c(2,2))
plot(model4)



ncvTest(model4) # p value < 0.05. not good

durbinWatsonTest(model4)  # D-W value 1..99 which is close to 2. Good       

vif(model4) # No columns with VIF value >=5

############ Regression Assumption Violation and steps to fix these ############
'''
Reference: Statistics module Slides

Diagnostic plots for the above model are showing multilinear regression assumption violations.

1. Gauss-Markov Assumptions
  i. Correct Functional form: 
      - The linear regression model assumes that there is a straight-line relationship between the
        predictors and the response.
      - If the dependent variable is linearly related to the independent variables, there should be no
        systematic relationship between the residuals and the predicted (that is, fitted) values. In other
        words, the model should capture all the systematic variance present in the data, leaving nothing but
        random noise.
      - To satisfy this assumption, there should not be a clear pattern in the Residual Vs Fitted plot.
    As we can see in the diagnostic plot for model3, there is a clear exponential curve pattern which is
    viotaing the assumption.
  ii. Homoscadasticity: Errors have constant variance
     - If we have met the constant variance assumption, the points in the Scale-Location graph of the R
       Diagnostic plots should be a random band around a horizontal line. There should not be any funnel
       kind of pattern (Fan in or Fan out) in the graph.
    As we can see in the plot for model3, there is a clear sign of heteroscadasticity. Intitially the points
    are compact, then they are spreading out. Hence, this is a violation to the assumption.
    Also, the ncvTest gave the following o/p which shows the p value as significant. This indicates
    Heteroscadasticity.
               Chisquare = 46877.41, Df = 1, p = < 2.22e-1
  iii.No auto-correlation between errors
     -If the dependent variable is normally distributed for a fixed set of predictor values, then the 
       residual values should be normally distributed with a mean of 0.
     - Also, this can be tested using Durbin-Watson test. As per the test, if the D-W Statistics value is 
       close to 2, the assumption is satisfied. Value less than 1 or greater than 3 will be a huge violation.
    For model3, Normal Q-Q plot was looking reasonably fair. Also, D-W stats value is 2.0075 which is close
    to 2. So, we are good with the assumption.
     DurbinWatsonTest for model3:
     lag  Autocorrelation   D-W Statistic    p-value
      1    -0.003754238      2.007508        0.632
2. No Influential Data point:
    cooks distance should not be more than 1 for any data points. If any data points show cooks distance
    more than 1, we have influential data points.
    For model3, cooks distance is not exceeding 1. So, no violation here.
3. Multi-Collinearity (Variance Inflation Factor)
   Variance Inflation factor is used to check if there is any multicollinearity.
   VIF < 5, low multicollinearity
   VIF > 5, moderate multicollinearity
   VIF > 10, High multicollinearity
   For model3, All but one features are showing VIF less than 5. O
 
One of the possible solution to fix the violations is to take the log of the Response.
We will be doing that in the next step.

'''

#To improve the diagnostic plots                   
model5 <- lm(log(price)~., data = kctrain_3)                      
summary(model5)    # Multiple R-squared:  0.8496,	Adjusted R-squared:  0.8484                 
par(mfrow=c(2,2))
plot(model5)                         

# Much Better now. Diagnostics plot also looks much better. No violation of assumptions.

#Few more columns are showing insignificant
kctrain_4 <- subset(kctrain_3, select = -c(bedrooms_8,bathrooms_6.75,bathrooms_7.75,
                                           yr_built_1953,zipcode_98056,yr_built_2006))

######################################################################################################
####################### Final Model ################

model6 <- lm(log(price)~., data = kctrain_4)
summary(model6) # Multiple R-squared:  0.8495,	Adjusted R-squared:  0.8484  
par(mfrow=c(2,2))
plot(model6)

vif(model6)

ncvTest(model6) # p- value is 0.33 which i s good. No homoscadasticity

durbinWatsonTest(model6)
######################################################################################################

####################### Prediction ###################################################################

pred1 <- predict(model6, newdata = kctest)
rmse <- sqrt(sum((pred1 - log(kctest$price))^2)/length(kctest$price)) # taking log since I took log(price) in the model.
c(RMSE = rmse, R2=summary(model6)$r.squared)

'''
                   RMSE          R2 
                0.2047548     0.8468920 
'''

plot(log(kctest$price), pred1)    # # Linear relationship between actual and predicted values which is good.


####### Alternative way to calculate rmse ###########
#install.packages("Metrics")
library(Metrics)
rmse(pred1,log(kctest$price))  # 0.2048595
mean(log(kctest$price))        # 13.04085                   

# rmse is very low which is good.
# The prediction error RMSE is 0.2048595, representing an error rate of 
#                        0.2048595/13.04085 = 0.0157      . The less, the good.



