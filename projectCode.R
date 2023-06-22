#importing libraries 

library(dplyr)
library(randomForest)
library(e1071)
library(party)
library(ggplot2)

#get data
data <- read.csv("C:/Users/Vish/Downloads/adult.csv", header=FALSE)

#data cleaning

#renaming columns 
colnames(data) <- c("Age", "Workclass", "FinalWeight", "Education", "EducationNum", "MaritalStatus", "Occupation", "Relationship", "Race", "Sex", "CapitalGain", "CapitalLoss", "HoursPerWeek", "NativeCountry", "target")
#omitting NA values 
data[] <- lapply(data, gsub, pattern = "?", replacement = NA, fixed = TRUE)
data <- na.omit(data)
#change columns to factors/numeric values so svm/random forests can process it 
data <- data%>%mutate(Age = as.numeric(Age), Workclass = as.factor(Workclass), FinalWeight = as.numeric(FinalWeight), 
                    Education= as.factor(Education), EducationNum = as.numeric(EducationNum), 
                    MaritalStatus = as.factor(MaritalStatus), Occupation = as.factor(Occupation), 
                    Relationship = as.factor(Relationship), Race = as.factor(Race), Sex = as.factor(Sex), 
                    CapitalGain = as.numeric(CapitalGain), CapitalLoss = as.numeric(CapitalLoss), 
                    HoursPerWeek = as.numeric(HoursPerWeek), NativeCountry = as.factor(NativeCountry),
                    target = as.factor(target))

#getting 10% of the total data to process due to memory limitations 
df <- data[sample(nrow(data),3016),]

#randomForest 
set.seed(1)

rfo = randomForest(target~., data = df, proximity=TRUE, mtry = 2, ntree = 225)
rfo
#finding the most important attributes 
View(rfo$importance)

#SVM
SVM = svm(target~., data = df)
summary(SVM)

#finding the SVM accuracy 
table(Predicted = predict(SVM,df ), Actual = df$target)

#Decision trees for looking at single attributes 

tree = ctree( target~ Relationship, data = df)

plot(tree)

tree = ctree( target~ MaritalStatus, data = df)

plot(tree)

tree = ctree( target~ Age, data = df)

plot(tree)

tree = ctree( target~ Occupation, data = df)

plot(tree)

tree = ctree( target~ CapitalGain, data = df)

plot(tree)





