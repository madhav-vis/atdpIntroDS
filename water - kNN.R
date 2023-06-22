#data cleaning 
water_potability = water_potability %>% filter(!is.na(ph), !is.na(Sulfate), !is.na(Hardness), !is.na(Solids), !is.na(Chloramines), !is.na(Conductivity), !is.na(Organic_carbon), !is.na(Trihalomethanes), !is.na(Turbidity), !is.na(Potability))
View(water_potability)

ggplot(water_potability, aes(x = Sulfate, y = Organic_carbon, color = Potability)) + geom_point()
ggplot(water_potability, aes(x = Chloramines, y = Turbidity, color = Potability)) + geom_point()

#getting training and validating data 

p = 0.9
total.row = nrow(water_potability)
row.nums = sample.int(total.row, p * total.row)

t.data = slice(water_potability, row.nums)
v.data = slice(water_potability, -row.nums)


t1.data = select(t.data, c("Chloramines", "Turbidity"))
v2.data = select(v.data, c("Chloramines", "Turbidity"))
class = select(t.data, c("Potability"))

#kNN classification

library(class)
knn1 <- knn(t1.data, v2.data, class$Potability, k = 7)

#accuracy of knn 
acc <- 100 * (sum(v.data$Potability == knn1) / nrow(v.data))
print(acc)
