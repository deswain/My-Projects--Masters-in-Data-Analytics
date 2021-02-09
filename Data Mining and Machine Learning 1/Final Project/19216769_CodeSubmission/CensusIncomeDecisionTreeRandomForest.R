

setwd("D:\\NCI\\Sem 1\\DMML\\Datasets\\Census Income Dataset")
censusData <- read.csv("CensusIncomeDataset.csv")
head(censusData)
str(censusData) # 48842 obs. of  15 variables 


sapply(censusData, function(x) sum(is.na (x))) 
# Seems it doesn't have missing values But need to check if there is any strange values.

lapply(censusData, table)


'''
   Strange values (question marks with a space before it) in the following columns. These ar nothing but missing/NA/unknown values.
   
   workclass -      count: 585
   occupation -     count: 586
   native.country - count: 181
  ******************************
     Total Missing Values: 1352           

'''

# Replacing the ? values with na for better clarity.
# Reference: DMML lab 3 : Assigning ImputedAgeMean value in place of missing values in titanicData$Age column
library(dplyr)
str(censusData$workclass)

'''
censusData[censusData$workclass == "?",c("workclass")] <- NA
sapply(censusData, function(x) sum(is.na (x))) 

The above code did not replace ? with na values. After further analysis, it is found that all the 
question marks had a space before them. Hence, these values were not being replaced by na values. 

'''
######################## Handling Missing Values ##################################
'''
Two options can be followed here

1. Replacing the missing categorical column values as unknown(new category value). Hence, dummy columns
   will be created accordingly.
2. Replacing the missing categorical columns with Modes of respective columns
  

'''
sapply(censusData, function(x) sum(x== ' ?'))
################################################################################


################### Step 1:  Replacing the missing values with a new category as unknown ##################
censusData[censusData$workclass == " ?",c("workclass")] <- 'Unknown'
censusData[censusData$occupation == " ?",c("occupation")] <- 'Unknown'
censusData[censusData$native.country == " ?",c("native.country")] <- 'Unknown'

# Checking the missing values now.
sapply(censusData, function(x) sum(is.na (x))) # Reflecting now.

lapply(censusData, table)

# Now all the categorical features are ready for dummy column creation.
# But before creating the dummies, the resonse (Salary Column), need to be converted to numeric.
# Else, the dummies will be created for this column which we don't want.


table(censusData$Salary) # <=50K  <=50K.    >50K   >50K.
# Strange observation. It is showing 4 categories but it clearly implies 2 categories, i.e,
# <=50k is same as <=50k. --- extra dot(.) is there.
# >50k is same as >50k.   --- extra dot(.) is there.
# So, these values will be handled in the next steps
# The same is mentioned in UCI from where the dataset is collected.
# http://archive.ics.uci.edu/ml/datasets/Adult (Attribute Information)

censusData[censusData$Salary == "<=50K.",c("Salary")] <- '<=50K'
censusData[censusData$Salary == ">50K.",c("Salary")] <- '>50K' 
table(censusData$Salary)

# The above codes didn't work. On further analysis, it is found that all the categories have
# space before them.

censusData[censusData$Salary == " <=50K.",c("Salary")] <- ' <=50K'
censusData[censusData$Salary == " >50K.",c("Salary")] <- ' >50K' 
table(censusData$Salary) # Now we have 2 classes: <= 50K , >50K

# Lets convert these categories to numerical before creating the dummies for features.

censusData$Salary <- ifelse(censusData$Salary==" >50K", 1, 0)
table(censusData$Salary) # 0, 1


lapply(censusData, table)
str(censusData)    # 48842 obs. of  15 variables:
table(censusData$age)
'''
Categorical columns: age, workclass, education, education.num,marital.status, occupation,
                      Relationship, race, sex, capital.gain, capital.loss, hours.per.week,
                      native.country

Age(more than 70 value types),  capital.gain (more than 100 types of values), 
capital.loss(more than 90 value types) and hours.per.week (more than 90 value types) 
have huge number of values. But, whe I saw is using table function, these values have 
been repeated several time. Hence, I am considering these columns as categorical columns

All the above mentioned columns need to be factor/char type before dummy creation.

'''
censusData$age <- as.factor(censusData$age)
censusData$education.num <- as.factor(censusData$education.num)
censusData$capital.gain <- as.factor(censusData$capital.gain)
censusData$capital.loss <- as.factor(censusData$capital.loss)
censusData$hours.per.week <- as.factor(censusData$hours.per.week)

str(censusData)


#### Creating Dummies for all the categorical features ######

censusData_1 <- fastDummies::dummy_cols(censusData, remove_first_dummy = TRUE)
str(censusData_1) # 48842 obs. of  512 variables

# Dropping the original columns
censusData_2 <- subset(censusData_1, select = -c(age, workclass,education,education.num,
                                                 marital.status, occupation, relationship, race,
                                                 sex, capital.gain, capital.loss, hours.per.week,
                                                 native.country))
str(censusData_2) 
##################### Splitting the data into train and test ##########
library(caTools)
set.seed(222) 
censusSample <- sample.split(censusData_2,SplitRatio = 0.75)
censusTrain <- subset(censusData_2,censusSample ==TRUE)
censusTest <- subset(censusData_2, censusSample==FALSE)
str(censusTrain) # 7501 obs. of  435 variables:
str(censusTest)  # 2508 obs. of  435 variables:



########################### Applying Model 1: Decision Tree ###################

library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
censusTrain$Salary <- as.factor(censusTrain$Salary)
fit <- rpart(Salary~., data = censusTrain, method = 'class')
rpart.plot(fit, extra = 106)
summary(fit)

########### Model Evaluation #############

str(fit)
str(censusTest)
################################### Accuracy and Confusion Matrix #####

census_pred = predict(fit,censusTest,type="class")
t = censusTest$Salary
accuracy = sum(census_pred == t)/length(t) 
print(accuracy) # Decesion Tree


confMat <- table(censusTest$Salary,census_pred) #Decision tree

confMat
'''
           census_pred
             0    1
        0 1762  120
        1  335  291

'''
sensitivity(confMat)                             
specificity(confMat)
F_meas(confMat)



'''
x <- subset(censusTest , select = -c(Salary))
x1 <- as.vector(x)
x2 <- as.vector(census_pred)
b <- prediction(as.numeric(fit), censusTest$Salary)
c <- performance(b, measure = "tpr", x.measure = "fpr")
'''

plot(census_pred)

str(census_pred)
str(censusTest$Salary)





############### ROC Decision Tree ####
#install.packages("gplots")
library(gplots)
library(ROCR)
library(pROC)
par(pty="s")
Pred.cart = predict(fit, newdata = censusTest, type = "class")
test_roc = roc(censusTest$Salary ~ as.numeric(Pred.cart), plot = TRUE, print.auc = TRUE, smooth= FALSE,
               auc.polygon=TRUE, auc.polygon.col= "lightyellow" ,col= "#377eb8", lwd=4, legacy.axes=TRUE,
                percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate")

################################################################################################
#################################Decision Tree: Overfitting/Underfitting Test with K-Fold #############
# Let us predict with new sample data and check the performance
library(caret)
folds <- createFolds(censusTrain$Salary, k = 10)

# Fold 1
census_train_01 <- censusTrain[folds$Fold01, ]
Censu_test_01 <- censusTrain[-folds$Fold01, ]

census_train_01$Salary <- as.factor(census_train_01$Salary)
KFoldFit01 <- rpart(Salary~., data = census_train_01, method = 'class')
rpart.plot(KFoldFit01, extra = 106)
summary(KFoldFit01)

Kfold01_census_pred = predict(KFoldFit01,Censu_test_01,type="class")
t = Censu_test_01$Salary
accuracy01 = sum(Kfold01_census_pred == t)/length(t) 
print(accuracy01) # 0.7948452


# Fold 2
census_train_02 <- censusTrain[folds$Fold02, ]
Censu_test_02 <- censusTrain[-folds$Fold02, ]

census_train_02$Salary <- as.factor(census_train_02$Salary)
KFoldFit02 <- rpart(Salary~., data = census_train_02, method = 'class')
rpart.plot(KFoldFit02, extra = 106)
summary(KFoldFit02)

Kfold02_census_pred = predict(KFoldFit02,Censu_test_02,type="class")
t = Censu_test_02$Salary
accuracy02 = sum(Kfold02_census_pred == t)/length(t) 
print(accuracy02) # 0.7977778


# Fold 3
census_train_03 <- censusTrain[folds$Fold03, ]
Censu_test_03 <- censusTrain[-folds$Fold03, ]

census_train_03$Salary <- as.factor(census_train_03$Salary)
KFoldFit03 <- rpart(Salary~., data = census_train_03, method = 'class')
rpart.plot(KFoldFit03, extra = 106)
summary(KFoldFit03)

Kfold03_census_pred = predict(KFoldFit03,Censu_test_03,type="class")
t = Censu_test_03$Salary
accuracy03 = sum(Kfold03_census_pred == t)/length(t) 
print(accuracy03) # 0.8170912

# Fold 4
census_train_04 <- censusTrain[folds$Fold04, ]
Censu_test_04 <- censusTrain[-folds$Fold04, ]

census_train_04$Salary <- as.factor(census_train_04$Salary)
KFoldFit04 <- rpart(Salary~., data = census_train_04, method = 'class')
rpart.plot(KFoldFit04, extra = 106)
summary(KFoldFit04)

Kfold04_census_pred = predict(KFoldFit04,Censu_test_04,type="class")
t = Censu_test_04$Salary
accuracy04 = sum(Kfold04_census_pred == t)/length(t) 
print(accuracy04) # 0.8130647

# Fold 5
census_train_05 <- censusTrain[folds$Fold05, ]
Censu_test_05 <- censusTrain[-folds$Fold05, ]

census_train_05$Salary <- as.factor(census_train_05$Salary)
KFoldFit05 <- rpart(Salary~., data = census_train_05, method = 'class')
rpart.plot(KFoldFit05, extra = 106)
summary(KFoldFit05)

Kfold05_census_pred = predict(KFoldFit05,Censu_test_05,type="class")
t = Censu_test_05$Salary
accuracy05 = sum(Kfold05_census_pred == t)/length(t) 
print(accuracy05) # 0.8163235


# fold 6
census_train_06 <- censusTrain[folds$Fold06, ]
Censu_test_06 <- censusTrain[-folds$Fold06, ]

census_train_06$Salary <- as.factor(census_train_06$Salary)
KFoldFit06 <- rpart(Salary~., data = census_train_06, method = 'class')
rpart.plot(KFoldFit06, extra = 106)
summary(KFoldFit06)

Kfold06_census_pred = predict(KFoldFit06,Censu_test_06,type="class")
t = Censu_test_06$Salary
accuracy06 = sum(Kfold06_census_pred == t)/length(t) 
print(accuracy06)  # 0.7990225

# fold 7
census_train_07 <- censusTrain[folds$Fold07, ]
Censu_test_07 <- censusTrain[-folds$Fold07, ]

census_train_07$Salary <- as.factor(census_train_07$Salary)
KFoldFit07 <- rpart(Salary~., data = census_train_07, method = 'class')
rpart.plot(KFoldFit07, extra = 106)
summary(KFoldFit07)

Kfold07_census_pred = predict(KFoldFit07,Censu_test_07,type="class")
t = Censu_test_07$Salary
accuracy07 = sum(Kfold07_census_pred == t)/length(t) 
print(accuracy07) # 0.8103704

# fold 8
census_train_08 <- censusTrain[folds$Fold08, ]
Censu_test_08 <- censusTrain[-folds$Fold08, ]

census_train_08$Salary <- as.factor(census_train_08$Salary)
KFoldFit08 <- rpart(Salary~., data = census_train_08, method = 'class')
rpart.plot(KFoldFit08, extra = 106)
summary(KFoldFit08)

Kfold08_census_pred = predict(KFoldFit08,Censu_test_08,type="class")
t = Censu_test_08$Salary
accuracy08 = sum(Kfold08_census_pred == t)/length(t) 
print(accuracy08) #0.794963

# fold 9
census_train_09 <- censusTrain[folds$Fold09, ]
Censu_test_09 <- censusTrain[-folds$Fold09, ]

census_train_09$Salary <- as.factor(census_train_09$Salary)
KFoldFit09 <- rpart(Salary~., data = census_train_09, method = 'class')
rpart.plot(KFoldFit09, extra = 106)
summary(KFoldFit09)

Kfold09_census_pred = predict(KFoldFit09,Censu_test_09,type="class")
t = Censu_test_09$Salary
accuracy09 = sum(Kfold09_census_pred == t)/length(t) 
print(accuracy09) # 0.8129166

# fold 10
census_train_10 <- censusTrain[folds$Fold10, ]
Censu_test_10 <- censusTrain[-folds$Fold10, ]

census_train_10$Salary <- as.factor(census_train_10$Salary)
KFoldFit10 <- rpart(Salary~., data = census_train_10, method = 'class')
rpart.plot(KFoldFit10, extra = 106)
summary(KFoldFit10)

Kfold10_census_pred = predict(KFoldFit10,Censu_test_10,type="class")
t = Censu_test_10$Salary
accuracy10 = sum(Kfold10_census_pred == t)/length(t) 
print(accuracy10)  # 0.813361

CombinedAcc <- c(accuracy01,accuracy02,accuracy03,accuracy04,accuracy05,accuracy06,
                 accuracy07,accuracy08,accuracy09,accuracy10)
DtKfolFinalAccuracy = mean(CombinedAcc) # 0.8069736

# N.B : The accuracy values may vary when the folding will be done again.
#       But, overall accuracy will be almost same.
##############################################################################################

################ Model 2 : Random Forest ####################
#install.packages("randomForest")
library(randomForest)
#?randomForest


rForestCensus <- randomForest(Salary~., data = censusTrain, importance = TRUE)
# The above code gave folloing error:
# Error in eval(predvars, data, env) : object 'workclass_ Local-gov' not found

# On further analysis, the issue was with the column names like 'workclass_ Local-gov'
# Solution reference: https://stackoverflow.com/questions/47910439/error-in-evalpredvars-data-env-object-trafd110906-when-using-randomfore
censusTrain$Salary <- as.factor(censusTrain$Salary)
names(censusTrain) <- make.names(names(censusTrain))
rForestCensus <- randomForest(Salary~., data = censusTrain, importance = TRUE, method= 'class')

#?randomForest
rForestCensus


y <- censusTest
y$Salary <- as.factor(y$Salary)
names(y) <- make.names(names(y))
rpred <- predict(rForestCensus, y)
confMat <- table(y$Salary,rpred) 
confMat

sensitivity(confMat)
specificity(confMat)
F_meas(confMat)

'''
               rpred
               0    1
           0 1769  113
           1  279  347

'''





#### Since the dataset is large, random forest is taking too much time to execute(between 40 and 60 minutes) 
# Hence, to make the testing a bit simpler, a subset of the dataset is used.
'''
x <- censusTrain[1:5000, ]
names(x) <- make.names(names(x))
x$Salary <- as.factor(x$Salary) 
rForestCensus <- randomForest(Salary~., data = x, importance = TRUE, method= 'class')
rForestCensus # From Confusion Matrix: Accuracy = .84714

y <- censusTest[5001:6000, ]
y$Salary <- as.factor(y$Salary)
names(y) <- make.names(names(y))
rpred <- predict(rForestCensus, y)
confMat <- table(y$Salary,rpred)

censusTest$Salary <- as.factor(censusTest$Salary)
names(censusTest) <- make.names(names(censusTest))
rpred <- predict(rForestCensus, censusTest)
'''



library(Metrics)
accuracy(y$Salary, rpred) # Random Forest
#####################################

################### ROC, AUC for Random Forest ####
par(pty="s")
Pred.cart = predict(rForestCensus, newdata = y, type = "class")
test_roc = roc(y$Salary ~ as.numeric(Pred.cart), plot = TRUE, print.auc = TRUE, smooth= FALSE,
               auc.polygon=TRUE, auc.polygon.col= "lightblue" ,col= "#377eb8", lwd=4, legacy.axes=TRUE,
               percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate")

######################################################################
###################Random Forest: Overfit/ Underfit Test using K-Fold Validation #########

library(caret)
rfolds <- createFolds(censusTrain$Salary, k = 10)

# Fold 1
rcensus_train_01 <- censusTrain[rfolds$Fold01, ]
rCensus_test_01 <- censusTrain[-rfolds$Fold01, ]
names(rcensus_train_01) <- make.names(names(rcensus_train_01))
names(rCensus_test_01) <- make.names(names(rCensus_test_01))

rcensus_train_01$Salary <- as.factor(rcensus_train_01$Salary)
rKFoldFit01 <- randomForest(Salary~., data = rcensus_train_01, method = 'class')
#rpart.plot(rKFoldFit01, extra = 106)
summary(rKFoldFit01)

rKfold01_census_pred = predict(rKFoldFit01,rCensus_test_01,type="class")
t = rCensus_test_01$Salary
raccuracy01 = sum(rKfold01_census_pred == t)/length(t) 
print(raccuracy01) # 0.8280255

# Fold 2
rcensus_train_02 <- censusTrain[rfolds$Fold02, ]
rCensus_test_02 <- censusTrain[-rfolds$Fold02, ]
names(rcensus_train_02) <- make.names(names(rcensus_train_02))
names(rCensus_test_02) <- make.names(names(rcensus_train_02))

rcensus_train_02$Salary <- as.factor(rcensus_train_02$Salary)
rKFoldFit02 <- randomForest(Salary~., data = rcensus_train_02, method = 'class')
#rpart.plot(rKFoldFit02, extra = 106)
#summary(rKFoldFit02)

rKfold02_census_pred = predict(rKFoldFit02,rCensus_test_02,type="class")
t = rCensus_test_02$Salary
raccuracy02 = sum(rKfold02_census_pred == t)/length(t) 
print(raccuracy02) # 0.8308399

# Fold 3

rcensus_train_03 <- censusTrain[rfolds$Fold03, ]
rCensus_test_03 <- censusTrain[-rfolds$Fold03, ]
names(rcensus_train_03) <- make.names(names(rcensus_train_03))
names(rCensus_test_03) <- make.names(names(rcensus_train_03))

rcensus_train_03$Salary <- as.factor(rcensus_train_03$Salary)
rKFoldFit03 <- randomForest(Salary~., data = rcensus_train_03, method = 'class')
#rpart.plot(rKFoldFit03, extra = 106)
#summary(rKFoldFit03)

rKfold03_census_pred = predict(rKFoldFit03,rCensus_test_03,type="class")
t = rCensus_test_03$Salary
raccuracy03 = sum(rKfold03_census_pred == t)/length(t) 
print(raccuracy03) # 0.8289142

# Fold 4
rcensus_train_04 <- censusTrain[rfolds$Fold04, ]
rCensus_test_04 <- censusTrain[-rfolds$Fold04, ]
names(rcensus_train_04) <- make.names(names(rcensus_train_04))
names(rCensus_test_04) <- make.names(names(rcensus_train_04))

rcensus_train_04$Salary <- as.factor(rcensus_train_04$Salary)
rKFoldFit04 <- randomForest(Salary~., data = rcensus_train_04, method = 'class')
#rpart.plot(rKFoldFit04, extra = 106)
#summary(rKFoldFit04)

rKfold04_census_pred = predict(rKFoldFit04,rCensus_test_04,type="class")
t = rCensus_test_04$Salary
raccuracy04 = sum(rKfold04_census_pred == t)/length(t) 
print(raccuracy04) # 0.8329384

# Fold 5
rcensus_train_05 <- censusTrain[rfolds$Fold05, ]
rCensus_test_05 <- censusTrain[-rfolds$Fold05, ]
names(rcensus_train_05) <- make.names(names(rcensus_train_05))
names(rCensus_test_05) <- make.names(names(rcensus_train_05))

rcensus_train_05$Salary <- as.factor(rcensus_train_05$Salary)
rKFoldFit05 <- randomForest(Salary~., data = rcensus_train_05, method = 'class')
#rpart.plot(rKFoldFit05, extra = 106)
#summary(rKFoldFit05)

rKfold05_census_pred = predict(rKFoldFit05,rCensus_test_05,type="class")
t = rCensus_test_05$Salary
raccuracy05 = sum(rKfold05_census_pred == t)/length(t) 
print(raccuracy05) # 0.832

# Fold 6
rcensus_train_06 <- censusTrain[rfolds$Fold06, ]
rCensus_test_06 <- censusTrain[-rfolds$Fold06, ]
names(rcensus_train_06) <- make.names(names(rcensus_train_06))
names(rCensus_test_06) <- make.names(names(rcensus_train_06))

rcensus_train_06$Salary <- as.factor(rcensus_train_06$Salary)
rKFoldFit06 <- randomForest(Salary~., data = rcensus_train_06, method = 'class')
#rpart.plot(rKFoldFit06, extra = 106)
#summary(rKFoldFit06)

rKfold06_census_pred = predict(rKFoldFit06,rCensus_test_06,type="class")
t = rCensus_test_06$Salary
raccuracy06 = sum(rKfold06_census_pred == t)/length(t) 
print(raccuracy06) # 0.8231373

# Fold 7
rcensus_train_07 <- censusTrain[rfolds$Fold07, ]
rCensus_test_07 <- censusTrain[-rfolds$Fold07, ]
names(rcensus_train_07) <- make.names(names(rcensus_train_07))
names(rCensus_test_07) <- make.names(names(rcensus_train_07))

rcensus_train_07$Salary <- as.factor(rcensus_train_07$Salary)
rKFoldFit07 <- randomForest(Salary~., data = rcensus_train_07, method = 'class')
#rpart.plot(rKFoldFit07, extra = 106)
#summary(rKFoldFit07)

rKfold07_census_pred = predict(rKFoldFit07,rCensus_test_07,type="class")
t = rCensus_test_07$Salary
raccuracy07 = sum(rKfold07_census_pred == t)/length(t) 
print(raccuracy07) # 0.8379252

# Fold 8
rcensus_train_08 <- censusTrain[rfolds$Fold08, ]
rCensus_test_08 <- censusTrain[-rfolds$Fold08, ]
names(rcensus_train_08) <- make.names(names(rcensus_train_08))
names(rCensus_test_08) <- make.names(names(rcensus_train_08))

rcensus_train_08$Salary <- as.factor(rcensus_train_08$Salary)
rKFoldFit08 <- randomForest(Salary~., data = rcensus_train_08, method = 'class')
#rpart.plot(rKFoldFit08, extra = 106)
#summary(rKFoldFit08)

rKfold08_census_pred = predict(rKFoldFit08,rCensus_test_08,type="class")
t = rCensus_test_08$Salary
raccuracy08 = sum(rKfold08_census_pred == t)/length(t) 
print(raccuracy08) # 0.8252111

# Fold 9
rcensus_train_09 <- censusTrain[rfolds$Fold09, ]
rCensus_test_09 <- censusTrain[-rfolds$Fold09, ]
names(rcensus_train_09) <- make.names(names(rcensus_train_09))
names(rCensus_test_09) <- make.names(names(rcensus_train_09))

rcensus_train_09$Salary <- as.factor(rcensus_train_09$Salary)
rKFoldFit09 <- randomForest(Salary~., data = rcensus_train_09, method = 'class')
#rpart.plot(rKFoldFit08, extra = 106)
#summary(rKFoldFit08)

rKfold09_census_pred = predict(rKFoldFit09,rCensus_test_09,type="class")
t = rCensus_test_09$Salary
raccuracy09 = sum(rKfold09_census_pred == t)/length(t) 
print(raccuracy09) # 0.82461

# Fold 10

rcensus_train_10 <- censusTrain[rfolds$Fold10, ]
rCensus_test_10 <- censusTrain[-rfolds$Fold10, ]
names(rcensus_train_10) <- make.names(names(rcensus_train_10))
names(rCensus_test_10) <- make.names(names(rcensus_train_10))

rcensus_train_10$Salary <- as.factor(rcensus_train_10$Salary)
rKFoldFit10 <- randomForest(Salary~., data = rcensus_train_10, method = 'class')
#rpart.plot(rKFoldFit08, extra = 106)
#summary(rKFoldFit08)

rKfold10_census_pred = predict(rKFoldFit10,rCensus_test_10,type="class")
t = rCensus_test_10$Salary
raccuracy10 = sum(rKfold10_census_pred == t)/length(t) 
print(raccuracy10) # 0.83084

### Final Accuracy for Random forest
rfAccCombined <- c(raccuracy01,raccuracy02,raccuracy03,raccuracy04,raccuracy05,raccuracy06,raccuracy07,
                   raccuracy08,raccuracy09,raccuracy10)

rfKfoldFinalAccuracy <- mean(rfAccCombined) # 0.8294894

# Accuracy decreased a little bit but still the model is performing well.
  # Hence, no overfitting or underfitting

###################################################################

























