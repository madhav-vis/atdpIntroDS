log = glm(Survived ~ Speed, family = binomial, data = crash)
print(log)

xgen <- seq(26, 117, 0.01)

ygen <- predict(log, list(Speed = xgen), type = "response")

plot(crash$Speed, crash$Survived, pch = 16, xlab = "Speed", ylab = "Survived")

lines(xgen, ygen)

library(pROC)

roc(crash$Survived, log$fitted.values, plot = TRUE)


log1 = glm(Survived ~ Age, family = binomial, data = crash)
print(log1)

xgen <- seq(2, 58, 0.01)

ygen <- predict(log1, list(Age = xgen), type = "response")

plot(crash$Age, crash$Survived, pch = 16, xlab = "Age", ylab = "Survived")

lines(xgen, ygen)

library(pROC)

roc(crash$Survived, log1$fitted.values, plot = TRUE)
