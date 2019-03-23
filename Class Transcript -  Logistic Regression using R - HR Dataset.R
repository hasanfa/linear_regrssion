#-------Importing the data---------
setwd("C:/Users/Aspire R11/Documents/Rdatasets/linear")
hr<-read.csv("HR dataset for Employee Attrition.csv")


# Data Exploration

dim(hr)
str(hr)
View(hr)

# Convert Attrition to numeric variable

hr$AttritionTarget <- as.numeric(hr$Attrition)-1
View(hr)

# What is the ratio of Attrited vs. not Attritted?

# Frequency Distribution of the binary dependent variable 

table(hr$AttritionTarget)
table(hr$AttritionTarget)/nrow(hr)

#Checking for missing values in all the columns

colSums(is.na(hr))

# No missing values

# Partition the dataset into training and validation dataset

sampling<-sort(sample(nrow(hr), nrow(hr)*.7))

length(sampling)

#Row subset and create training and validation samples using the index numbers

train<-hr[sampling,]
test<-hr[-sampling,]
nrow(train)
nrow(test)

# Checking the frequency Distribution of the target variable 

table(train$AttritionTarget)
table(train$AttritionTarget)/1029
table(test$AttritionTarget)/441

#Renaming Age column

colnames(train)
names(train)[1] <- "Age"
colnames(train)
names(test)[1] <- "Age"

#Are any of the independent variables correlated?

#install.packages("corrplot", dependencies = T)
library(corrplot)

#Finding correlation between numeric variables 

str(train)
traincor<-cor(train[,c(1,4,6,7,11,13,14,15,17,19,20,21,24,25,26,28:35)])

library(corrgram)
?corrgram
cormat<-corrgram(traincor)

corrplot(traincor)

write.csv(cormat,"Correlation.csv")

# After Conditional formatting, we find :
# High correlation between:
# Job Level and Monthly Income
# Job Level and Total Working Years
# Monthly Income and Total Working Years
# Percent Salary Hike and Performance Rating

str(train)
colnames(train)

?glm()
# Family of dependent variable is binary or binomial 
myresult<-glm(data=train,AttritionTarget ~ Age+BusinessTravel+
+DailyRate+Department+DistanceFromHome+Education+
EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+
JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+
NumCompaniesWorked+OverTime+PercentSalaryHike+
RelationshipSatisfaction+StandardHours+StockOptionLevel+
TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,family=binomial)

summary(myresult)

#Gives best fitted model
#To choose a good model

?step
reduced<-step(myresult,direction="backward")


# Iteration 2: 
myresult<-glm(data=train,AttritionTarget ~ Age + BusinessTravel + DailyRate + Department + 
                DistanceFromHome + EnvironmentSatisfaction + Gender + JobInvolvement + 
                JobSatisfaction + MaritalStatus + MonthlyIncome + NumCompaniesWorked + 
                OverTime + RelationshipSatisfaction + StockOptionLevel + 
                TrainingTimesLastYear + YearsInCurrentRole + YearsSinceLastPromotion + 
                YearsWithCurrManager,family=binomial)

summary(myresult)

# Creating dummy variables

train$BTF <- ifelse(train$BusinessTravel == "Travel_Frequently",1,0)
train$OTY <- ifelse(train$OverTime == "Yes",1,0)


test$BTF <- ifelse(test$BusinessTravel == "Travel_Frequently",1,0)
test$OTY <- ifelse(test$OverTime == "Yes",1,0)

#Iteration # 3:


myresult<-glm(data=train,AttritionTarget ~ BTF + EnvironmentSatisfaction + JobInvolvement + 
                JobSatisfaction + MonthlyIncome + NumCompaniesWorked + 
                OTY + YearsSinceLastPromotion,family=binomial)

summary(myresult)

# Iteration # 4


myresult<-glm(data=train,AttritionTarget ~ BTF + EnvironmentSatisfaction + JobInvolvement + 
                JobSatisfaction + MonthlyIncome +  
                OTY,family=binomial)

summary(myresult)


#Finding Predicted Values

?glm

myresult$fitted.values


train$predicted <- myresult$fitted.values
train$predicted


# Compare with actual data

head(train$AttritionTarget)

head(train$predicted)

# Let us convert the probabilities also into Good/Bad response 
# based on a cut-off probability

#Confusion Matrix
train$predclass<-ifelse(train$predicted>0.5,1,0)
table(train$predclass,train$AttritionTarget)

#True Positive+ True Negative should be high. 

# Accuracy = (TP+TN)/(P+N)

(848+29)/(848+29+137+15)

# For different cutoff probabilities, the confusion matrix will be different

# To find accuracies for different cut-off probabilities

# There are a lot of performance parameters available in ROCR package

#install.packages("ROCR")
library(ROCR)


# The prediction function of the ROCR library basically creates 
# a structure to validate our predictions with actual values

pred<-prediction(train$predicted,train$AttritionTarget)


pred
?performance

perf <- performance(pred,"acc")
class(perf)
perf
# x values contain the cut-off probabilities

#use @ to access the slots

class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))

cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))

cutoffs <- data.frame(cutoffprob, accuracies )
# In the decreasing order of accuracy
cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

# Pick cutoff for which Accuracy is highest 

train$predclass <- ifelse(train$predicted>0.5175264,1,0)

# Kappa values and Confusion Matrix from caret package

library(caret)





library(irr)

kappa2(data.frame(train$AttritionTarget,train$predclass))


confusionMatrix(as.factor(train$AttritionTarget),as.factor(train$predclass), positive = "1")


## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
# Receiver Operating Characteristic Curve (ROC) a plot of TPR versus FPR 
# for the possible cut-off classification probability values.
# A good ROC curve should be almost vertical in the beginning and 
# almost horizontal in the end.
# "tpr" and "fpr" are arguments of the "performance" function 
# indicating that the plot is between the true positive rate and 
# the false positive rate.

?abline
# Draw a straight line with intercept 0 and slope = 1
# lty is the line type (dotted or dashed etc.)
# The straight line is a random chance line
# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")


# Area under the curve should be more than 50%

auc<-performance(pred,"auc")
auc

#Creating a Gains chart

#install.packages("gains")

library(gains)


gains(as.numeric(train$AttritionTarget),train$predicted, groups =10)
quantile(train$predicted, seq(0,1,0.1))

targeted <- which(train$predicted >= 0.185868869)

targeted

# To obtain predictions from the model, use the predict() function.

?predict()
test$pred <- predict(myresult, type = "response",newdata = test)


# The value 'response' to the parameter type would make sure 
# that these predictions are returned as probability of events.


?predict
