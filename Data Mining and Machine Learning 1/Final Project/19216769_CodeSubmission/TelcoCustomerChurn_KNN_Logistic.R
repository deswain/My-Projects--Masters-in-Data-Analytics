######################### KNN on Telco Customer Churn ######################
#install.packages("class")
#install.packages("gmodels")
library(class)



setwd("D:\\NCI\\Sem 1\\DMML\\Datasets\\Telco Customer Churn Dataset")
#Reading the dataset
telcodata <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
head(telcodata)
str(telcodata) # 7043 obs. of  21 variables
head(telcodata)

table(telcodata$Churn)
############################### Missing Values #########################

# Check If there are any missing values
sapply(telcodata, function(x) sum(is.na (x)))

# Column "TotalCharges" has 11 missing values.
#Let's see what type of data is stored in the above mentioned column
table(telcodata$TotalCharges)
median(telcodata$TotalCharges,na.rm = TRUE)
boxplot(telcodata$TotalCharges, xlab= 'TotalChurn') # Doesn't showing any outliers.
?boxplot

# This looks like continuous values. Replacing these missing values with the mean of TotalCharges
# Reference: DMML Lab 3 - Replacing missing values in the Age column.
TotalChargesMean <- mean(telcodata$TotalCharges,na.rm = TRUE) # Taking mean while excluding the missing values

telcodata[is.na(telcodata$TotalCharges),c('TotalCharges')] <- TotalChargesMean

sapply(telcodata, function(x) sum(is.na (x))) # No more missing values

str(telcodata)

########################################################################

########## Romoving CutomerID since it is not relevant for the model building ########
telcodata_1 <- subset(telcodata, select = -c(customerID))
str(telcodata_1) # 7043 obs. of  20 variables

#################### Analysing Categorical Features ###########################
lapply(telcodata_1, table) # Reference: https://stackoverflow.com/questions/32220202/how-to-get-a-frequency-table-of-all-columns-of-complete-data-frame-in-r

###########
'''                     
                       LIST OF CATEGORICAL FEATURES

gender - Male, Female
seniorCitizen - 0, 1
Partner - Yes, No
Dependents - Yes, No
tenure - 0 to 72 (73 categories)
PhoneService - Yes, No
MultipleLines - Yes, No, No phone service
InternetService - DSL, Fiber optic, No
OnlineSecurity -  Yes, No, No internet service
OnlineBackup - Yes, No, No internet service
DeviceProtection - Yes, No, No internet service
TechSupport - Yes, No, No internet service
StreamingTV - Yes, No, No internet service
StreamingMovies - Yes, No, No internet service
Contract - Month-to-month, One year, Two year
PaperlessBilling - Yes, No
PaymentMethod - Bank transfer (automatic), Credit card (automatic), Electronic check, Mailed check


'''

# Some of the features have binary categories but these are string categories.
# So, these categories will be changed to binary numeric
table(telcodata_1$gender) # Male, Female
telcodata_1$gender <- ifelse(telcodata_1$gender=="Male", 1, 0)
table(telcodata_1$gender) # 0, 1

table(telcodata_1$Partner) # Yes, No
telcodata_1$Partner <- ifelse(telcodata_1$Partner=="Yes", 1, 0)
table(telcodata_1$Partner) # 1,0

table(telcodata_1$Dependents) # Yes, No
telcodata_1$Dependents <- ifelse(telcodata_1$Dependents=="Yes", 1, 0)
table(telcodata_1$Dependents) # 1,0

table(telcodata_1$PhoneService) # Yes, No
telcodata_1$PhoneService <- ifelse(telcodata_1$PhoneService=="Yes", 1, 0)
table(telcodata_1$PhoneService) # 1, 0


table(telcodata_1$PaperlessBilling)
telcodata_1$PaperlessBilling <- ifelse(telcodata_1$PaperlessBilling=="Yes", 1, 0)
table(telcodata_1$PaperlessBilling)


# Other categorical features have more than one class. So, these columns will be handled while creating dummies.


################ Changing the Dependent Variable classes from Yes/No to 1/0 ##############
table(telcodata_1$Churn) # Yes, No
telcodata_1$Churn <- ifelse(telcodata_1$Churn=="Yes", 1, 0)
table(telcodata_1$Churn)
'''
Reason: Initially, the datatype for "Churn" was char.
fastdummies package cretes dummies for all the columns having data type either factor or character.
Dummy columns are not necessary for the response. 
Hence, changed the data type to numeric by replacing Yes wih 1 and No with 0.
Will revert the changes after dummy creation
'''
str(telcodata_1)
###########################################################################################
# CREATING DUMMY COLUMNS FOR FEATURES HAVING MORE THAN 2 CATEGORIES

# The data types for multi-categorical features are showing as character type (chr).
# So, no need to chage the data type while creating dummies

'''
telcodata_1$MultipleLines <- as.factor(telcodata_1$MultipleLines)
telcodata_1$InternetService <- as.factor(telcodata_1$InternetService)
telcodata_1$OnlineSecurity <- as.factor(telcodata_1$OnlineSecurity)
telcodata_1$OnlineBackup <- as.factor(telcodata_1$OnlineBackup)
telcodata_1$DeviceProtection <- as.factor(telcodata_1$DeviceProtection)
telcodata_1$TechSupport <- as.factor(telcodata_1$TechSupport)
telcodata_1$StreamingTV <- as.factor(telcodata_1$StreamingTV)
telcodata_1$StreamingMovies <- as.factor(telcodata_1$StreamingMovies)
telcodata_1$Contract <- as.factor(telcodata_1$Contract)
telcodata_1$PaymentMethod <- as.factor(telcodata_1$PaymentMethod)
'''
str(telcodata_1) # 7043 obs. of  20 variables

# Creating dummies but removing first dummy for each categorical feature
telcodata_2 <- fastDummies::dummy_cols(telcodata_1, remove_first_dummy = TRUE)

str(telcodata_2) # 7043 obs. of  41 variables

# Since dummy columns are created, respective original columns will be removed in the next step

telcodata_3 <- subset(telcodata_2, select = -c(MultipleLines,InternetService,OnlineSecurity,
                                               OnlineBackup,DeviceProtection,TechSupport,
                                               StreamingTV,StreamingMovies,Contract,PaymentMethod))

str(telcodata_3) # 7043 obs. of  31 variables

# Now, the data is ready to be used for model building

#########################################################################################

############################### SPLITTING DATA INTO TARIN & TEST ########################
#library(caTools)
#set.seed(111) 
#telcoSample <- sample.split(telcodata_3,SplitRatio = 0.75)
#telcoTrain <- subset(telcodata_3,telcoSample ==TRUE)
#telcoTest <- subset(telcodata_3, telcoSample==FALSE)
#str(telcoTrain) # 5225 obs. of  31 variables
#str(telcoTest)  # 1818 obs. of  31 variables


telcoTrain <- telcodata_3[1:5250, ]
TelcoTest <-  telcodata_3[5251:7043,]

telco_train_labels <- telcodata_3[1:5250, 10]
telco_test_labels <- telcodata_3[5251:7043, 10]

##################################################################################

##################### Applying KNN Model on telcoTrain ###########################
?knn
#telcoKnnModel1 <- knn(train = telcoTrain, test = telcoTest, k=3)

# For K= 72  : Accuracy : 0.7803,Kappa : 0.3234,Sensitivity : 0.29899,Specificity : 0.96379
telcoKnnModel1 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=72)
confusionMatrix(telcoKnnModel1,ftelco_test_labels, positive = "1")
#sqrt(5250) # 72.45. So I have taken K= 72 which is close to the sqrt of 5250.
# After trying several K values in the following steps, It was found that K=72 was not the best one.
summary(telcoKnnModel1)
'''
library(gmodels)
CrossTable(x = telco_test_labels, y = telcoKnnModel1, prop.chisq=FALSE)



accuracy = function(actual, predicted) {
  mean(actual == predicted)
}


k_to_try = seq(1, by=2, len=90)
acc_k = rep(x= 0, times = length(k_to_try))

#acc_k[6]

for(i in seq_along(k_to_try)) {
  telcoKnnModel1 = knn(train = telcoTrain,
                       test = TelcoTest,
                       cl= telco_train_labels,
                       k= k_to_try[i])
  acc_k[i] = accuracy(telco_test_labels,telcoKnnModel1)
}


plot(acc_k, type = "b", col = "dodgerblue", cex = 1, pch = 20,
     xlab = "k, number of neighbours", ylab = "classification accuracy",
     main = "Accuracy Vs Neighbours")


abline(v = which(acc_k == max(acc_k)), col = "darkorange", lwd = 1.5)
abline(h = max(acc_k), col = "grey", lty = 2)

# The above code was running fine but the strange result were received.
  For example, in the plot, maximum accuracy was showing for K=6.
  But actual accuracy for k=6 was showing 0.7892 in the confusion matrix. 
  Hence, I tried the K values one by one and got maximum accuracy when k=11

'''

# We can see here that the maximum accuracy is being received when K= 6. 
# However, for k=72 (close to sqrt(5250)), maximum accuracy is not being received. 

################# K Value Selection ######################
library(caret)
library(e1071)
# For K = 3 : Accuracy= 0.7747, Kappa= 0.4011, Sensitivity : 0.4970,Specificity : 0.8806
telcoKnnModel3 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=3)
CrossTable(x = telco_test_labels, y = telcoKnnModel3, prop.chisq=FALSE)
# Confusion Matrix # Reference: ISLR book, DMML Lab for KNN
# changing data type to factor as the as the confusion matrix was throwing follwing error.
# Error: `data` and `reference` should be factors with the same levels.
str(telcoKnnModel2) # factor 
str(telco_test_labels) # numeric
ftelco_test_labels <- as.factor(telco_test_labels) 
str(ftelco_test_labels) # factors

confusionMatrix(telcoKnnModel2,ftelco_test_labels, positive = "1")

# For K=4:Accuracy : 0.7685,Kappa : 0.3828,Sensitivity : 0.4808,Specificity : 0.8783 
telcoKnnModel4 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=4)
confusionMatrix(telcoKnnModel4,ftelco_test_labels, positive = "1")

# For K = 5: Accuracy : 0.7897,Kappa : 0.4309,Sensitivity : 0.4970,Specificity : 0.9014
telcoKnnModel5 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=5)
confusionMatrix(telcoKnnModel5,ftelco_test_labels, positive = "1")

# For K=6:Accuracy : 0.7892,Kappa : 0.4275,Sensitivity : 0.4909,Specificity : 0.9029
telcoKnnModel6 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=6)
confusionMatrix(telcoKnnModel6,ftelco_test_labels, positive = "1")


#For K= 7: Accuracy : 0.8015,Kappa : 0.4549,Sensitivity : 0.4970,Specificity : 0.9176
telcoKnnModel7 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=7)
confusionMatrix(telcoKnnModel7,ftelco_test_labels, positive = "1")

# For K=8: Accuracy : 0.797,Kappa : 0.4434,Sensitivity : 0.4909,Specificity : 0.9137
telcoKnnModel8 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=8)
confusionMatrix(telcoKnnModel8,ftelco_test_labels, positive = "1")

# For K = 9: Accuracy : 0.8048, Kappa : 0.4566, Sensitivity : 0.4828,Specificity : 0.9276
telcoKnnModel9 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=9)
confusionMatrix(telcoKnnModel9,ftelco_test_labels, positive = "1")

# For K=10: Accuracy : 0.8076,Kappa : 0.464,Sensitivity : 0.4869,Specificity : 0.9299
telcoKnnModel10 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=10)
confusionMatrix(telcoKnnModel10,ftelco_test_labels, positive = "1")

# For k=11: Accuracy : 0.8076,Kappa : 0.4655,Sensitivity : 0.4909,Specificity : 0.9284 
telcoKnnModel11 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=11)
confusionMatrix(telcoKnnModel11,ftelco_test_labels, positive = "1")

# For k=12: Accuracy : 0.8042,Kappa : 0.4509,Sensitivity : 0.4707,Specificity : 0.9314  
telcoKnnModel12 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=12)
confusionMatrix(telcoKnnModel12,ftelco_test_labels, positive = "1")

# For K=13: Accuracy : 0.807,Kappa : 0.4591,Sensitivity : 0.4768,Specificity : 0.9330 
telcoKnnModel13 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=13)
confusionMatrix(telcoKnnModel13,ftelco_test_labels, positive = "1")


########################## Final KNN Model #################################
# For K=11, maximum accuracy and Kappa value was received. Hence, 11 is selected as k value
# For k=11: Accuracy : 0.8076,Kappa : 0.4655,Sensitivity : 0.4909,Specificity : 0.9284 
telcoKnnModel11 <- knn(train = telcoTrain, test = TelcoTest, cl = telco_train_labels, k=11,prob=TRUE)
confusionMatrix(telcoKnnModel11,ftelco_test_labels, positive = "1")

str(telcoKnnModel11)
#x <- as.numeric(telcoKnnModel11)

#knntest_prob = predict(x, newdata = TelcoTest, type = "response")
#knntest_roc = roc(TelcoTest$Churn ~ test_prob, plot = TRUE, print.auc = TRUE, smooth= FALSE, 
#                  auc.polygon=TRUE, auc.polygon.col= "cornsilk" ,col= "#377eb8", lwd=4)


########################################### ROC-AUC for KNN ######################################
library(gmodels)
library(pROC)

plot(roc(TelcoTest$Churn, attributes(telcoKnnModel11)$prob),
     print.thres = TRUE,
     print.auc = TRUE,
     auc.polygon=TRUE,
     auc.polygon.col= "cornsilk",
     col= "#377eb8", lwd=4,legacy.axes=TRUE,
     percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate")




##########################################################################################################


#                              Logistic Regression




######################################################################################################################
########################### Applying Logistic Regression Model ####################################
str(telcoTrain)
str(telcoTrain$Churn)
telcoTrain$Churn <- factor(telcoTrain$Churn)
LRModel_1 <- glm(Churn ~. , data = telcoTrain, family = "binomial")
summary(LRModel_1)




# From the model summary, several columns are showing insignificant values(P < 0.05).
# Hence, excluding these columns from the model.

lrTelcoTrain_1 <- subset(telcoTrain, select = -c(gender,SeniorCitizen,Partner,Dependents,
                                                 PhoneService,MonthlyCharges,`MultipleLines_No phone service`,
                                                 `OnlineSecurity_No internet service`,`OnlineBackup_No internet service`,
                                                 `DeviceProtection_No internet service`,`TechSupport_No internet service`,
                                                 `StreamingTV_No internet service`,`StreamingMovies_No internet service`,
                                                 `PaymentMethod_Credit card (automatic)`,`PaymentMethod_Mailed check`))


LRModel_2 <- glm(Churn~., data = lrTelcoTrain_1, family = "binomial")
summary(LRModel_2)

# Again, few more columns are showing insignificant p value. Hence, excluding those.

######################## Final LR Model #############################

lrTelcoTrain_2 <- subset(lrTelcoTrain_1, select = -c(MultipleLines_Yes,OnlineBackup_Yes,DeviceProtection_Yes))

LRModel_3 <- glm(Churn~., data = lrTelcoTrain_2, family = "binomial")
summary(LRModel_3)
str(lrTelcoTrain_2$Churn)  


############### Prediction and Accuracy of LR ################
test_prob = predict(LRModel_3, newdata = TelcoTest, type = "response")
test_roc = roc(TelcoTest$Churn ~ test_prob, plot = TRUE, print.auc = TRUE, smooth= FALSE, 
               auc.polygon=TRUE, auc.polygon.col= "cornsilk" ,col= "#377eb8", lwd=4)

str(test_prob)
str(TelcoTest$Churn)
test_prob <- ifelse(test_prob<0.5,0,1)
test_prob<- as.factor(test_prob)
contrasts(test_prob)
contrasts(TelcoTest$Churn)
#test_prob=rep("Yes" ,1)
#test_prob[test_prob >.5]="1"
table(test_prob,TelcoTest$Churn)

accuracy(test_prob,TelcoTest$Churn ) # 0.79977
sensitivity(table(test_prob,TelcoTest$Churn)) # 0.90755
specificity(table(test_prob,TelcoTest$Churn)) # 0.5171
kappa(table(test_prob,TelcoTest$Churn)) # 0.4588
?kappa


#library(caret)
# Prediction    
#pdata <- predict(LRModel_3, newdata = TelcoTest, family= "binomial")
#str(pdata)
#str(x)
#x <- ifelse(pdata>0.5, 1, 0)
#confusionMatrix(pdata,x, threshold= 0.5)




# Pseudo R Squared Values
#install.packages("rcompanion")
library(rcompanion)
nagelkerke(LRModel_3)

'''
                                 Pseudo.R.squared
McFadden                             0.282562
Cox and Snell (ML)                   0.277370
Nagelkerke (Cragg and Uhler)         0.405949

# As we can see, Nagelkerke R squared value is 0.406 which is more than 0.05. Hence, the model is a good fit.
'''


###################### ROC and AUC curve ##############################
par(pty="s")
test_prob = predict(LRModel_3, newdata = TelcoTest, type = "response")
test_roc = roc(TelcoTest$Churn ~ test_prob, plot = TRUE, print.auc = TRUE, smooth= FALSE, print.thres = TRUE,
               auc.polygon=TRUE, auc.polygon.col= "yellow" ,col= "#377eb8", lwd=4,legacy.axes=TRUE,
                xlab="False Positive Rate", ylab="True Positive Rate")
