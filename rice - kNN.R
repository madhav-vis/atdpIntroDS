#data cleaning 

riceClassification = riceClassification %>% filter_all(any_vars(!is.na(.)))

#getting training and validating data 

p = 0.99
total.row = nrow(riceClassification)
row.nums = sample.int(total.row, p * total.row)

t.data = slice(riceClassification, row.nums)
v.data = slice(riceClassification, -row.nums)


t1.data = select(t.data, c("Area", "MajorAxisLength"))
v2.data = select(v.data, c("Area", "MajorAxisLength"))
class = select(t.data, c("Class"))

#kNN classification

library(class)
knn1 <- knn(t1.data, v2.data, class$Class, k = 7)

#accuracy of knn 
acc <- 100 * (sum(v.data$Class == knn1) / nrow(v.data))
print(acc)
