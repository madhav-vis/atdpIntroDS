library(dplyr)
ggplot(riceClassfiction, aes(Area, Roundness, color = Class)) + geom_point()
library(ggplot2)
ggplot(riceClassification, aes(Area, MajorAxisLength , color = Class)) + geom_point()
riceClassification = na.omit(riceClassification)

p = 0.8 
total.row = nrow(riceClassification)
row.nums = sample.int(total.row, p * total.row)
riceClassification = select(riceClassification, c("Area", "MajorAxisLength"))
training.data = slice(riceClassification, row.nums)
validating.data = slice(riceClassification, -row.nums)
classification = select(riceClassification, c("Class"))
training.data = select(validating.data, c("Area", "MajorAxisLength"))

o <- knn(training.data, validating.data, classification , k = 135 )
