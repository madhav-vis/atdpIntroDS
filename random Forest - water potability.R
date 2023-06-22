library(ggplot2)
library(randomForest)
library(dplyr)


#data cleaning 
data = water_potability %>% filter(!is.na(ph), !is.na(Sulfate), !is.na(Hardness), !is.na(Solids), !is.na(Chloramines), !is.na(Conductivity), !is.na(Organic_carbon), !is.na(Trihalomethanes), !is.na(Turbidity), !is.na(Potability))
View(data)

#convert columns to factors 
data$Potability = as.factor(data$Potability)

#set RNG
set.seed(42)

#runs the model 
random.forest = randomForest(Potability~., data = data, proximity=TRUE)

random.forest

View(random.forest$err.rate)

error.rate = data.frame(numTrees = rep(1:500, times = 3), Error = random.forest$err.rate[,"OOB"])
View(error.rate)
ggplot(error.rate, aes(numTrees, Error)) + geom_line()

error.values = list(length = 10)
for(i in 1:10){
  model = randomForest(Potability~., data = data, proximity= TRUE, mtry =i )
  error.values[i] = model$err.rate[nrow(model$err.rate), 1]
}

error.values

random.forest.optimal = randomForest(Potability~., data = data, proximity=TRUE, mtry = 9, ntree = 500)
random.forest.optimal
random.forest