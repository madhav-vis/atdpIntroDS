library(dplyr)
p = 0.8 
total.row = nrow(weather)
row.nums = sample.int(total.row, p * total.row)

colnames(weather)[1] <- "outlook"

#reassigning strings to numbers for knn
weather$outlook[weather$outlook == "sunny"] <- 1
weather$outlook[weather$outlook == "overcast"] <- 2
weather$outlook[weather$outlook == "rainy"] <- 3

weather$windy <- as.integer(weather$windy)

t.data = slice(weather, row.nums)
v.data = slice(weather, -row.nums)


t1.data = select(t.data, c("outlook", "windy", "temperature"))
v2.data = select(v.data, c("outlook", "windy", "temperature"))
class = select(t.data, c("play"))

library(class)
knn1 <- knn(t1.data, v2.data, class$play, k = 5)

#accuracy of knn 
acc <- 100 * (sum(v.data$play == knn1) / nrow(v.data))
print(acc)


