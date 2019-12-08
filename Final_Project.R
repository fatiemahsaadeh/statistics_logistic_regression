library(ggplot2)
library(tree)
library(rpart)
library(randomForest)
library(caret)
library(rpart.plot)
library(data.table) 


# Dataset from https://www.kaggle.com/uciml/pima-indians-diabetes-database
diabetes <- read.csv(file = "diabetes.csv",header = TRUE,sep = ",")

# Show the data structure, There are 768 observations of 9 variables
str(diabetes)

# Some plots
Age <- cut(diabetes$Age, c(seq(20, 70, by = 10), Inf), include.lowest = TRUE)
Age_fac <- factor(Age, levels=c('[20,30]', '(30,40]', '(40,50]','(50,60]', '(60,70]', '(70,Inf]'))
outcomes_fac <- ifelse(diabetes$Outcome, "yes", "no")
boxplot(diabetes$DiabetesPedigreeFunction~Age_fac,data=diabetes, main="DiabetesPedigreeFunction per Age ",
        xlab="AGE", ylab="DiabetesPedigreeFunction", col="brown")
boxplot(diabetes$BMI~Age_fac,data=diabetes, main="BMI per Age",
        xlab="AGE", ylab="BMI", col="brown")
boxplot(diabetes$BMI~outcomes_fac,data=diabetes, main="Diabetes outcome based on BMI results ",
        xlab="Outcome", ylab="BMI", col="brown")
boxplot(diabetes$BMI~outcomes_fac,data=diabetes, main="Diabetes outcome based on DiabetesPedigreeFunction results ",
        xlab="Outcome", ylab="DiabetesPedigreeFunction", col="brown")

# index_train using 1/3 of the data 
index_train<-sample(768,256)

# Create the TRAINING SET
training_set <- data[index_train, ]

# Create the TEST SET
test_set <- data[-index_train, ]

outcomes_fac <- ifelse(test_set$Outcome, "yes", "no")

# Fit the Logistic Regression Model using the training_set 
logistic_model <- glm(Outcome ~ ., family = "binomial", data = training_set)

# The statistically significant variables : Pregnancies, Glucose, BMI, DiabetesPedigreeFunction 
# not statistically significant variables : BloodPressure, SkinThickness, Insulin and Age are 
print(summary(logistic_model))
print(summary(logistic_model)$coeff[-1,4] < 0.05)

# predictions using logistic regression model for the test set
predictions_logistic <- predict(logistic_model, newdata = test_set, type = "response") 

# wide range means the model is good for predicting diabetes

xInsulin <-seq (0, 1000, 10)
Insulin_logistic_model <- glm(Outcome ~ Insulin, family = "binomial", data = training_set)

Insulin_predictions_logistic <- predict(Insulin_logistic_model, list(Insulin=xInsulin),type="response")

plot(training_set$Insulin, training_set$Outcome, pch = 16, xlab = "Insulin Level", ylab = "Outcome")
lines(xInsulin, Insulin_predictions_logistic, col = "blue", lwd = 2)

# Construct a Decision Tree 
model_dt<-rpart(Outcome~Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction+Age, data=training_set, method = 'class')

# Plot the decision trees 
rpart.plot(model_dt)



# Make binary predictions for the original tree using the test set 
predictions_tree <- predict(model_dt, newdata = test_set, type = "class")

# Construct confusion matrices using the predictions.
confmatrix_tree <- table(test_set$Outcome, predictions_tree)


# Calculate for the decision tree accuracy 90.5%
accuracy_tree <- sum(diag(confmatrix_tree))/sum(confmatrix_tree)
print(confmatrix_tree)
print (accuracy_tree)

