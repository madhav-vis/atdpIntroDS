data <- na.omit(heart)
data$target = as.factor(data$target)


#decision tree
library(party)
tree = ctree( target~ ., data = data)

plot(tree)

test = predict(tree, newdata = data)

table(predict(tree), data$target)

#random forest

random.forest = randomForest(target~., data = data, proximity=TRUE)

random.forest

View(random.forest$err.rate)

error.rate = data.frame(numTrees = rep(1:500, times = 3), Error = random.forest$err.rate[,"OOB"])
View(error.rate)
ggplot(error.rate, aes(numTrees, Error)) + geom_line()

error.values = list(length = 10)
for(i in 1:10){
  model = randomForest(target~., data = data, proximity= TRUE, mtry =i )
  error.values[i] = model$err.rate[nrow(model$err.rate), 1]
}

error.values

random.forest.optimal = randomForest(target~., data = data, proximity=TRUE, mtry = 2, ntree = 500)
random.forest.optimal
random.forest

#SVM

model = svm(target~., data = data)
summary(model)

predication <- predict(model, data)
table(Predicated = predication, Actual = data$target)

plot(model, data = data, trestbps~thalach)

#colleges

cd <- na.omit(TopColleges)
View(cd)
cd <- select(cd, -Website)

cd <- mutate(cd, Good = case_when(Rank <= 295 ~ 1, Rank > 295 ~ 0))

cd$Good = as.factor(cd$Good)

cd <- select(cd, -Rank)

random.forest = randomForest(Good~., data = cd, proximity=TRUE)

random.forest

View(random.forest$err.rate)

error.rate = data.frame(numTrees = rep(1:500, times = 3), Error = random.forest$err.rate[,"OOB"])
View(error.rate)
ggplot(error.rate, aes(numTrees, Error)) + geom_line()

error.values = list(length = 10)
for(i in 1:10){
  model = randomForest(Good~., data = cd, proximity= TRUE, mtry =i )
  error.values[i] = model$err.rate[nrow(model$err.rate), 1]
}

error.values

random.forest.optimal = randomForest(Good~., data = cd, proximity=TRUE, ntree = 500, mtry = 3)
random.forest.optimal
random.forest

