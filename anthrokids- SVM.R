library(dplyr)
library(ggplot2)
library(e1071)
data <- na.omit(anthrokids)
data <- select(data, Height, Weight, Race)
View(data)

ggplot(data, aes(Height, Weight, color = Race)) + geom_point()

data$Race = as.factor(data$Race)

model = svm(Race~., data = data)
summary(model)

plot(model, data = data, target)

predication <- predict(model, data)
table(Predicated = predication, Actual = data$Race)
