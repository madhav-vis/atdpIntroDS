normalize1 = function(x) {
  if(x < 60)
    return(1)
  else if(x >= 60 & x < 70) {
    return(2)
  }
  else if(x >= 70 & x < 80) {
    return(3)
  }
  else if(x >= 70 & x < 80) {
    return(3)
  }
  else 
    return(4)
}

normalize <- Vectorize(normalize1)

library(dplyr)

swiss <- Swiss1 %>% mutate(Fertility = normalize(Fertility)) %>% select(Provinces, Fertility, Agriculture, Examination, Education, Catholic, Infant.Mortality)
View(swiss)

library(ggplot2)
ggplot(Swiss1, aes(x = Catholic, y = Fertility)) + geom_point()


p = 0.9
total.row = nrow(swiss)
row.nums = sample.int(total.row, p * total.row)

t.data = slice(swiss, row.nums)
v.data = slice(swiss, -row.nums)


t1.data = select(t.data, c("Catholic", "Examination"))
v2.data = select(v.data, c("Catholic", "Examination"))
class = select(t.data, c("Fertility"))

library(class)
knn1 <- knn(t1.data, v2.data, class$Fertility, k = 3)

#accuracy of knn 
acc <- 100 * (sum(v.data$Fertility == knn1) / nrow(v.data))
print(acc)
