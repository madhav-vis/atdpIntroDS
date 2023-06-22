library(dplyr)
library(ggplot2)
library(party)
library(randomForest)

bd <- Ballons.Dataset %>% filter(!is.na(Color), !is.na(Size), !is.na(Age), !is.na(Inflated), !is.na(Act))

bd$targetF = factor(bd$Inflated)
bd$Color = factor(bd$Color)
bd$Size = factor(bd$Size)
bd$Age = factor(bd$Age)
bd$Act = factor(bd$Act)

bd = select(bd, -Inflated)

# Decision tree

tree = ctree(targetF ~ ., data = bd)

plot(tree)

test = predict(tree, newdata = bd)

table(predict(tree), bd$targetF)

#random Forest 
set.seed(42)
random.forest = randomForest(targetF~., data = bd, proximity=TRUE)

random.forest

View(random.forest$err.rate)

error.rate = data.frame(numTrees = rep(1:500, times = 3), Error = random.forest$err.rate[,"OOB"])
View(error.rate)
ggplot(error.rate, aes(numTrees, Error)) + geom_line()

error.values = list(length = 4)
for(i in 1:4){
  model = randomForest(targetF~., data = bd, proximity= TRUE, mtry =i )
  error.values[i] = model$err.rate[nrow(model$err.rate), 1]
}

error.values

random.forest.optimal = randomForest(targetF~., data = bd, proximity=TRUE, mtry = 2, ntree = 100)
random.forest.optimal
random.forest