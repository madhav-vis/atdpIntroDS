diabetes_data <- read.csv("C:/Users/Vish/Downloads/diabetes_data_upload.csv")
dd <- na.omit(diabetes_data)

#cleaning for decision trees 
names(dd) = c("Age", "Gender", "Polyuria", "Polydipsia", "WeightLoss", "Weakness", "Polyphagia",
              "GenitalThrush", "Blurring", "Itching", "Irritability", "DelayedHealing", "PartialParesis",
              "Stiffness", "Alopecia", "Obesity", "class")

cols = c("Gender", "Polyuria", "Polydipsia", "WeightLoss", "Weakness", "Polyphagia", 
         "GenitalThrush", "Blurring", "Itching", "Irritability", "DelayedHealing", "PartialParesis", 
         "Stiffness", "Alopecia", "Obesity", "class")

dd[cols] = lapply(dd[cols], factor)
library(dplyr)

# decsion tree for polyuria 

dd1 <- select(dd, Polyuria, class)
library(party)
tree = ctree( class~ ., data = dd1)

plot(tree)

#decision tree for polydipsia 

dd1 <- select(dd, Polydipsia, class)
library(party)
tree = ctree( class~ ., data = dd1)

plot(tree)

#decision tree for Gender 

dd1 <- select(dd, Gender, class)
library(party)
tree = ctree( class~ ., data = dd1)

plot(tree)

#decision tree for Age

dd1 <- select(dd, Age, class)
library(party)
tree = ctree( class~ ., data = dd1)

plot(tree)

#decision tree for Partial Paresis 

dd1 <- select(dd, PartialParesis, class)
library(party)
tree = ctree( class~ ., data = dd1)

plot(tree)

#cleaning for logit 

dd <- na.omit(diabetes_data)
View(dd)

names(dd) = c("Age", "Gender", "Polyuria", "Polydipsia", "WeightLoss", "Weakness", 
              "Polyphagia", "GenitalThrush", "Blurring", "Itching", "Irritability", 
              "DelayedHealing", "PartialParesis", "Stiffness", "Alopecia", "Obesity", "class")

cols = c("Gender", "Polyuria", "Polydipsia", "WeightLoss", "Weakness", 
         "Polyphagia", "GenitalThrush", "Blurring", "Itching", "Irritability", 
         "DelayedHealing", "PartialParesis", "Stiffness", "Alopecia", "Obesity", "class")

#logit for Polyuria 

dd1 <- select(dd, Polyuria, class)


dd1$Polyuria[dd1$Polyuria == "Yes"] <- 1
dd1$Polyuria[dd1$Polyuria == "No"] <- 0

dd1$class[dd1$class == "Positive"] <- 1
dd1$class[dd1$class == "Negative"] <- 0

dd1$Polyuria <- as.numeric(dd1$Polyuria)
dd1$class <- as.numeric(dd1$class)

log = glm(class ~ Polyuria, family = binomial, data = dd1)
print(log)

xgen <- seq(0, 1, 0.00192308)

ygen <- predict(log, list(Polyuria = xgen), type = "response")

plot(dd1$Polyuria, dd1$class, pch = 16, xlab = "Polyuria", ylab = "class")

lines(xgen, ygen)

library(pROC)

roc(dd1$class, log$fitted.values, plot = TRUE)


#logit for polydipsia 

dd1 <- select(dd, Polydipsia, class)


dd1$Polydipsia[dd1$Polydipsia == "Yes"] <- 1
dd1$Polydipsia[dd1$Polydipsia == "No"] <- 0

dd1$class[dd1$class == "Positive"] <- 1
dd1$class[dd1$class == "Negative"] <- 0

dd1$Polydipsia <- as.numeric(dd1$Polydipsia)
dd1$class <- as.numeric(dd1$class)

log = glm(class ~ Polydipsia, family = binomial, data = dd1)
print(log)

xgen <- seq(0, 1, 0.00192308)

ygen <- predict(log, list(Polydipsia = xgen), type = "response")

plot(dd1$Polydipsia, dd1$class, pch = 16, xlab = "Polydipsia", ylab = "class")

lines(xgen, ygen)

library(pROC)

roc(dd1$class, log$fitted.values, plot = TRUE)


#logit for Gender

dd1 <- select(dd, Gender, class)


dd1$Gender[dd1$Gender == "Male"] <- 1
dd1$Gender[dd1$Gender == "Female"] <- 0

dd1$class[dd1$class == "Positive"] <- 1
dd1$class[dd1$class == "Negative"] <- 0

dd1$Gender <- as.numeric(dd1$Gender)
dd1$class <- as.numeric(dd1$class)

log = glm(class ~ Gender, family = binomial, data = dd1)
print(log)

xgen <- seq(0, 1, 0.00192308)

ygen <- predict(log, list(Gender = xgen), type = "response")

plot(dd1$Gender, dd1$class, pch = 16, xlab = "Gender", ylab = "class")

lines(xgen, ygen)

library(pROC)

roc(dd1$class, log$fitted.values, plot = TRUE)


#logit for Age

dd1 <- select(dd, Age, class)


dd1$class[dd1$class == "Positive"] <- 1
dd1$class[dd1$class == "Negative"] <- 0

dd1$class <- as.numeric(dd1$class)
dd1$Age <- as.numeric(dd1$Age)


log = glm(class ~ Age, family = binomial, data = dd1)
print(log)

xgen <- seq(0, 90, 1)

ygen <- predict(log, list(Age = xgen), type = "response")

plot(dd1$Age, dd1$class, pch = 16, xlab = "Age", ylab = "class")

lines(xgen, ygen)

library(pROC)

roc(dd1$class, log$fitted.values, plot = TRUE)




#logit for Partial Paresis 

dd1 <- select(dd, PartialParesis, class)


dd1$PartialParesis[dd1$PartialParesis == "Yes"] <- 1
dd1$PartialParesis[dd1$PartialParesis == "No"] <- 0

dd1$class[dd1$class == "Positive"] <- 1
dd1$class[dd1$class == "Negative"] <- 0

dd1$PartialParesis <- as.numeric(dd1$PartialParesis)
dd1$class <- as.numeric(dd1$class)

log = glm(class ~ PartialParesis, family = binomial, data = dd1)
print(log)

xgen <- seq(0, 1, 0.00192308)

ygen <- predict(log, list(PartialParesis = xgen), type = "response")

plot(dd1$PartialParesis, dd1$class, pch = 16, xlab = "PartialParesis", ylab = "class")

lines(xgen, ygen)

library(pROC)

roc(dd1$class, log$fitted.values, plot = TRUE)




#decision tree for Polydipsia vs Polyuria

dd1$Polyuria <- as.factor(dd1$Polyuria)
dd1$Polydipsia <- as.factor(dd1$Polydipsia)

library(party)
tree = ctree( Polyuria~ ., data = dd1)

plot(tree)
