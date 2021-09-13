# Regression Tree Video 14 & 15
# Regression Tree considering only the first 40  records.
CustCsv <- read.csv("c:\\R\\Customer_Age_Income.csv");
Income_DF <- data.frame(Inc = CustCsv$Income[1:40], Spend = CustCsv$SalesAmt[1:40]);
Income_DF
# install.packages("rpart")

library(rpart)
m.rpart <- rpart(Spend ~ ., data = Income_DF)


# Information about the tree
m.rpart

# Detailed information 
summary(m.rpart)

# Use library
library(rpart.plot)


# Regression Decision tree diagram
rpart.plot(m.rpart, digits = 3)

# Predict
p.rpart <- predict(m.rpart, Income_DF)

p.rpart;


# compare the  predicted values vs. actual values
summary(p.rpart)
summary(Income_DF$Spend)

cor(p.rpart, Income_DF$Spend) 




# Regression Tree Video 16
# Regression Tree considering only the first 40  records.
CustCsv <- read.csv("c:\\R\\Customer_Age_Income.csv");
Income_DF <- data.frame(Job = CustCsv$Job[1:40], Inc = CustCsv$Income[1:40], Spend = CustCsv$SalesAmt[1:40]);
Income_DF
# install.packages("rpart")

library(rpart)
rpart_class <- rpart(Spend ~ ., data = Income_DF,method="class")
rpart_anova <- rpart(Spend ~ ., data = Income_DF,method="anova")
rpart_poisson <- rpart(Spend ~ ., data = Income_DF,method="poisson")


# Information about the tree
#rpart_class

# Detailed information 
#summary(rpart_class)

# Use library
library(rpart.plot)


# Regression Decision tree diagram
rpart.plot(rpart_class, digits = 3)
# Regression Decision tree diagram
rpart.plot(rpart_anova, digits = 3)
# Regression Decision tree diagram
rpart.plot(rpart_poisson, digits = 3)

class_pred <- predict(rpart_class, Income_DF)
anova_pred <- predict(rpart_anova, Income_DF)
rpart_poisson <- predict(rpart_poisson, Income_DF)

MeanError <- function(Actual,Predicted) {mean(abs(Actual-Predicted))}
MeanError(Income_DF$Spend,class_pred)
MeanError(Income_DF$Spend,anova_pred)
MeanError(Income_DF$Spend,rpart_poisson)

