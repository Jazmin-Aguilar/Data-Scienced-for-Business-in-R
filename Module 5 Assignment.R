#load libraries
library(tidyverse)
library(caret)
library(ROCR)
library(ROSE)



#set working directory (adjust this for your own computer)
setwd("C:/Users/jazmi/OneDrive/1. DTSC-560-80 Data Science for Business/DTSC-560-80 Data Science for Business/Module 5")


#read dataset into R
insurancedf <- read.csv("insurance.csv")
View(insurancedf)

#Convert categorical variables to factors with levels and labels
insurancedf$CLAIM<-factor(insurancedf$CLAIM,levels = c(0,1),labels = c("No","Yes"))
insurancedf$KIDSDRIV<-factor(insurancedf$KIDSDRIV,levels = c(0,1),labels = c("No","Yes"))
insurancedf$HOMEKIDS<-factor(insurancedf$HOMEKIDS,levels = c(0,1),labels = c("No","Yes"))
insurancedf$HOMEOWN<-factor(insurancedf$HOMEOWN,levels = c(0,1),labels = c("No","Yes"))
insurancedf$MSTATUS<-factor(insurancedf$MSTATUS,levels = c(0,1),labels = c("No","Yes"))
insurancedf$GENDER<-factor(insurancedf$GENDER,levels = c(0,1),labels = c("No","Yes"))
insurancedf$EDUCATION<-factor(insurancedf$EDUCATION,levels = c(0,1),labels = c("No","Yes"))
insurancedf$CAR_USE<-factor(insurancedf$CAR_USE,levels = c(0,1),labels = c("No","Yes"))
insurancedf$RED_CAR<-factor(insurancedf$RED_CAR,levels = c(0,1),labels = c("No","Yes"))
insurancedf$CLM_BEF<-factor(insurancedf$CLM_BEF,levels = c(0,1),labels = c("No","Yes"))
insurancedf$REVOKED<-factor(insurancedf$REVOKED,levels = c(0,1),labels = c("No","Yes"))
insurancedf$MVR_PTS<-factor(insurancedf$MVR_PTS,levels = c(0,1),labels = c("No","Yes"))
insurancedf$URBANICITY<-factor(insurancedf$URBANICITY,levels = c(0,1),labels = c("No","Yes"))


#check for missing data
sum(is.na(insurancedf))


#1.generate summary statistics for all variables in dataframe
summary(insurancedf)

#QQ1: what percentage of customers have submitted a recent claim?

#QQ1 answer:  26.38274
(1908/(1908+5324))*10



#2. Partition the dataset into a training, validation, and test set,
#   using a 60%, 20%, 20% split

#set seed so the random sample is reproducible
set.seed(42)

Samples<-sample(seq(1,3),size=nrow(insurancedf),replace=TRUE,prob=c(0.6,0.2,0.2))
Train<-insurancedf[Samples==1,]
Validate<-insurancedf[Samples==2,]
Test<-insurancedf[Samples==3,]

#QQ2. How many observations are in a test set?

#QQ2 answer: 1440
summary(Test)

# there isn't a severe class imbalance, conduct a logistic regression analysis
# using the training set frame with CLAIM as outcome, and ALL
# other variables in dataset as predictors
#Logistic regression is part of the general linear model family, so the R 
#function is glm.
options(scipen=999)
Ltrain <- glm(CLAIM ~. , data = Train, 
               family = binomial(link = "logit"))
summary(Ltrain)
##QQ3. coefficient for the KIDSDRIV
#QQ3 answer:
#QQ4: odds ratio of the URBANICITY variable?
#QQ5: how would you interpret the odds ratio of the URBANICITY variable?
#QQ3 answer: 0.460689845
#QQ4 answer:10.73086
exp(2.373123349)


#Using the model you fitted in Step (3) and the validation data frame you created in
#Step (2), create a confusion matrix to assess the accuracy of the logistic
#regression model.
Ltrain
Validate

#Steps to create a confusion matrix

# obtain probability of defaulting for each observation in validation set

lprob <- predict(Ltrain, newdata = Validate, type ="response")


#Attach probability scores to Validate dataframe
Validated <- cbind(Validate, Probabilities=lprob)


# obtain predicted class for each observation in validation set using threshold of 0.5
lclass <- as.factor(ifelse(lprob > 0.5, "Yes","No"))

#Attach predicted class to Validate dataframe
Validated <- cbind(Validate, PredClass=lclass)

#Create a confusion matrix using "Yes" as the positive class 

confusionMatrix(lclass, Validate$CLAIM, positive = "Yes" )
#QQ6. How many insurance claims (positives) did the model predict
#correctly?

#Question 6 answer: 148
#QQ7. accuracy rate?
#Question 7 answer:78.15%
#QQ8. sensitivity?
#Question 8 answer: sensitivity:40.55
#QQ9 answer: not the best

#5.  again using the model you fitted in step(3), Ltrain, and the validation
#frame , create a ROC Curve and calculate the AUC.
#QQ10: what is the AUC?

##0.786447!!!!!!!!!!!!!!!!!!!!!!
#Plot ROC Curve for model from Validate training set

#create a prediction object to use for the ROC Curve
predROC <- prediction(lprob, Validate$CLAIM)

#create a performance object to use for the ROC Curve
perfROC <- performance(predROC,"tpr", "fpr")

#plot the ROC Curve
plot(perfROC)
abline(a=0, b= 1)

# compute AUC 
performance(predROC, measure="auc")@y.values[[1]]
#0.786447


# Even though we do not have a severe class imbalance in our data, let’s try
#addressing our moderate class imbalance to see if it improves our model
#accuracy. Using the training set you generated in Step (2), create a new training
#subset using the oversampling method


#Create a data frame with only the predictor variables by removing 
#column 2 (CLAIM)
xsdf<-Train[c(-1)]
View(xsdf)


#Create an oversampled training subset
set.seed(42)
oversample<-upSample(x=xsdf, y=Train$CLAIM, yname = "CLAIM")

table(oversample$CLAIM)


oversample
#Quiz question #11: In this new training subset generated from oversampling, how many
#observations are in the class that has made a recent auto claim (“Yes”)?

#QQ 11:3163


#7. Conduct a logistic regression analysis using the new oversampled training subset
#with CLAIM as the outcome variable and all the other variables in the dataset as
#predictor variables

Lover <- glm(CLAIM ~. , data = oversample, 
                family = binomial(link = "logit"))

#Using the model you fitted in Step (7) and the validation data frame you created in
#Step (2), create a confusion matrix to assess the accuracy of the logistic
#regression model

#Lover, Validate

# obtain probability of defaulting for each observation in validation set

lproblov <- predict(Lover, newdata = Validate, type ="response")


#Attach probability scores to Validate dataframe
Validatelov <- cbind(Validate, Probabilities=lproblov)


# obtain predicted class for each observation in validation set using threshold of 0.5
lclasslov <- as.factor(ifelse(lproblov > 0.5, "Yes","No"))

#Attach predicted class to Validate dataframe
Validatelov <- cbind(Validate, PredClass=lclasslov)

#Create a confusion matrix using "Yes" as the positive class 

confusionMatrix(lclasslov, Validatelov$CLAIM, positive = "Yes" )
##QQ12: accuracy rate: 0.7151
##QQ13: sensitivity: 0.7479


#Again using the model you fitted in Step (7) and the validation data frame, create
#an ROC curve plot and calculate the AUC.
#Lover, Validate

#Plot ROC Curve for model from Validate training set

#create a prediction object to use for the ROC Curve
predROClov <- prediction(lproblov, Validatelov$CLAIM)

#create a performance object to use for the ROC Curve
perfROClov <- performance(predROClov,"tpr", "fpr")

#plot the ROC Curve
plot(perfROClov)
abline(a=0, b= 1)

# compute AUC 
performance(predROClov, measure="auc")@y.values[[1]]
#0.7868078!!!!!!!!!!!!!!



#Quiz question #14: What is the AUC? ##QQ14 answer: 0.7868078

#Quiz question #15: What do you notice about this AUC value as compared to the AUC
#value for the previous model? (MC)
# a lil higher but basically same


#Let’s say that for this insurance company, sensitivity is more important than
#overall accuracy and the cost of false positives is lower than the cost of false
#negatives, so we will use the logistic regression model fitted to the oversampled
#training subset.
#Using the model generated in Step (7) and the test set you created in Step (2),
#create a confusion matrix to assess the accuracy of the logistic regression model
#on the test data frame

#Lover, Test

ltest <- predict(Lover, newdata = Test, type ="response")


#Attach probability scores to Test dataframe
Validatetrain <- cbind(Test, Probabilities=ltest)


# obtain predicted class for each observation in validation set using threshold of 0.5
lclasstrain <- as.factor(ifelse(ltest > 0.5, "Yes","No"))

#Attach predicted class to Test dataframe
Validatetrain <- cbind(Test, PredClass=lclasstrain)

#Create a confusion matrix using "Yes" as the positive class 

confusionMatrix(lclasstrain, Validatetrain$CLAIM, positive = "Yes" )

#Quiz question #16: How many insurance claims (positives) did the model predict
#correctly using the test set?
#QQ answer 16: 282
#Quiz question #17: What is the accuracy rate? :0.7208
#Quiz question #18: What is the sensitivity? : 0.7268


#Again using the model you fitted in Step (7) and the test data frame, create an ROC
#curve plot and calculate the AUC.

#create a prediction object to use for the ROC Curve
predROCtest <- prediction(ltest, Validatetrain$CLAIM)

#create a performance object to use for the ROC Curve
perfROCtest <- performance(predROCtest,"tpr", "fpr")

#plot the ROC Curve
plot(perfROCtest)
abline(a=0, b= 1)

# compute AUC 
performance(predROCtest, measure="auc")@y.values[[1]]

#Quiz question #19: What is the AUC? answer: 0.7976094

#Now we’ll use the model fitted to the oversampled training subset to make
#predictions about whether new customers will make auto insurance claims. Using
#the data contained in the csv file “insurance_predictions.csv”, predict the
#probability scores for insurance claims for ten new customers.
#Quiz question #20: What is the predicted probability of making an insurance claim for
#new customer #1

 #Lover, 
#predict probability of CLAIM for new customers

#read new dataset into R
new_customers <- read.csv("insurance_predictions.csv")
View(new_customers)



#Convert categorical variables to factors with levels and labels
new_customers$KIDSDRIV<-factor(new_customers$KIDSDRIV,levels = c(0,1),labels = c("No","Yes"))
new_customers$HOMEKIDS<-factor(new_customers$HOMEKIDS,levels = c(0,1),labels = c("No","Yes"))
new_customers$HOMEOWN<-factor(new_customers$HOMEOWN,levels = c(0,1),labels = c("No","Yes"))
new_customers$MSTATUS<-factor(new_customers$MSTATUS,levels = c(0,1),labels = c("No","Yes"))
new_customers$GENDER<-factor(new_customers$GENDER,levels = c(0,1),labels = c("No","Yes"))
new_customers$EDUCATION<-factor(new_customers$EDUCATION,levels = c(0,1),labels = c("No","Yes"))
new_customers$CAR_USE<-factor(new_customers$CAR_USE,levels = c(0,1),labels = c("No","Yes"))
new_customers$RED_CAR<-factor(new_customers$RED_CAR,levels = c(0,1),labels = c("No","Yes"))
new_customers$CLM_BEF<-factor(new_customers$CLM_BEF,levels = c(0,1),labels = c("No","Yes"))
new_customers$REVOKED<-factor(new_customers$REVOKED,levels = c(0,1),labels = c("No","Yes"))
new_customers$MVR_PTS<-factor(new_customers$MVR_PTS,levels = c(0,1),labels = c("No","Yes"))
new_customers$URBANICITY<-factor(new_customers$URBANICITY,levels = c(0,1),labels = c("No","Yes"))

# make predictions for new data (for which CLAIM is unknown)
lrprobsnew <- predict(Lover, newdata = new_customers , type = "response")

#Attach probability scores to new_customers dataframe 
new_customers <- cbind(new_customers, Probabilities=lrprobsnew)
View(new_customers) #61.93025.68