
tj <- tipjoke %>% filter(!is.na(Card), !is.na(Tip), !is.na(Ad), !is.na(Joke), !is.na(None)) %>% select(-Card) 
View(tj)


tj$TipF = factor(tj$Tip)
tj = select(tj, -Tip)


library(party)
tree = ctree(tj$TipF ~ ., data = tj)

plot(tree)

test = predict(tree, newdata = tj)

table(predict(tree), tj$TipF)
