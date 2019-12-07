library(ggplot2)
library(tree)
library(rpart)
library(randomForest)
library(caret)
library(rpart.plot)
library(data.table) 

# dataset from https://www.kaggle.com/uciml/pima-indians-diabetes-database
diabetes <- read.csv(file = "diabetes.csv",
         header = TRUE,
         sep = ",")

print(str(diabetes))
Age <- cut(diabetes$Age, c(seq(20, 70, by = 10), Inf), include.lowest = TRUE)
Age_fac <- factor(Age, levels=c('[20,30]', '(30,40]', '(40,50]','(50,60]', '(60,70]', '(70,Inf]'))
outcomes_fac <- ifelse(diabetes$Outcome, "yes", "no")
boxplot(diabetes$BMI~outcomes_fac,data=diabetes, main="Diabetes outcome based on BMI results ",
        xlab="Outcome", ylab="BMI", col="brown")
I1<-sample(768,615)
dsampletrain<-diabetes[I1,]
dsampletest<-diabetes[-I1,]
model_dt<-rpart(Outcome~Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction+Age, data=dsampletrain)
rpart.plot(model_dt)



