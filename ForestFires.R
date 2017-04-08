
## Descriptive Analysis

forestfires <- read.csv("forestfires.csv",header = TRUE, fill = TRUE)

forestfires<-forestfires[which(forestfires$area != 0),]


null <- lm(log(area + 1) ~ 1, forestfires[,c(-3, -4)])
full <- lm(log(area + 1)~., forestfires[,c(-3, -4)])
summary(full)
par(mfrow=c(2,2))
plot(full, which=c(1,2,4,5))

forestfires=forestfires[-500,]



n = nrow(forestfires)
test <- sample(1:n, round(n)/10)
forestfires.train <- forestfires[-test, ]
forestfires.test <- forestfires[test, ]







par(mfrow = c(1,2))
hist(forestfires.train$area, freq=FALSE, col="blue", main="Histogram",xlab="")
lines(density(forestfires.train$area), col="black")
title(outer=TRUE, main="\n Distribution of the burned area")
plot(ecdf(forestfires.train$area), main="Cumulative distribution",xlab="")



forestfires.train$area <- log((forestfires.train$area) + 1)




par(mfrow = c(1,2))
hist(forestfires.train$area, freq=FALSE, col="blue", main="Histogram",xlab="")
lines(density(forestfires.train$area), col="black")
plot(ecdf(forestfires.train$area), main="Cumulative distribution function",xlab="")
title(outer=TRUE, main="\n Distribution of the burned area")







are.factor <- sapply(forestfires.train, is.factor)

are.factor
library(ggplot2)
qplot(month, area, data = forestfires.train, geom = "boxplot")



library(ggplot2)
qplot(day, area, data = forestfires.train, geom = "boxplot")





factor_day <- factor(forestfires.train$day, levels=c("mon","tue","wed","thu","fri","sat","sun"))
barplot(table(factor_day),las=3)





factor_month <- factor(forestfires.train$month, levels=c("jan","feb","mar",
                                                         "apr","may","jun","jul","aug","sep","oct","nov","dec"))
barplot(table(factor_month),las=3)



library(GGally)
#ggpairs(forestfires.train, columns = which(!are.factor))





heatmap(abs(cor(forestfires.train[, !are.factor])))



n <- nrow(forestfires.train)
scope <- list(lower = terms(area ~ 1, data=forestfires.train[,c(-3, -4)]),
              upper = terms(area ~ ., data=forestfires.train[,c(-3, -4)]))
step.AIC <- step(null, scope, direction='both', trace=FALSE)
step.BIC <- step(null, scope, direction='both', k=log(n), trace=FALSE)
step.AIC
step.BIC

par(mfrow=c(2,2))
plot(step.AIC, which=c(1,2,4,5))
summary(step.AIC)


# Ridge regression


library(Matrix)
library(foreach)
library(glmnet)
x <- as.matrix(forestfires.train[, c(-3, -4, -13)])
y <- forestfires.train$area
ridge <- glmnet(x,y,alpha=0)

par(mfrow=c(1,3))
plot(ridge, xvar="lambda",label=TRUE)
plot(ridge, xvar="norm",label=TRUE)
plot(ridge, xvar="dev",label=TRUE)
ridge$dev.ratio

ridge.10cv <- cv.glmnet(x,y,nfolds=10, alpha=0, grouped=FALSE)
ridge.loo <- cv.glmnet(x,y,nfolds=n, alpha=0, grouped=FALSE)
par(mfrow=c(1,2))
plot(ridge.10cv)
plot(ridge.loo)
x0 <- as.matrix(forestfires.test[ , c(-3, -4, -13)])
result <- predict(ridge, newx=x0, s=ridge.10cv$lambda.min)
difference <- result - forestfires.test[, 13]

# lasoo regression

lasso <- glmnet(x, y, alpha = 1)
par(mfrow=c(1,3))
plot(lasso, xvar="lambda")
plot(lasso, xvar="norm")
plot(lasso, xvar="dev")
lasso$dev.ratio


lasso.10cv <- cv.glmnet(x,y,nfolds=10, grouped=FALSE)
lasso.loo <- cv.glmnet(x,y,nfolds=n , grouped=FALSE)
par(mfrow=c(1,2))
plot(lasso.10cv)
plot(lasso.loo)


# polynomial regression
modelpoly1 <- lm(y ~ poly(forestfires.train$temp, 4) )
modelpoly2 <- lm(y ~ poly(forestfires.train$FFMC, 4) )
summary(modelpoly1)
summary(modelpoly2)

# Linear regression

FFMC2 <- (forestfires.train$FFMC)^2
FFMC3 <- (forestfires.train$FFMC)^3

DMC2 <- (forestfires.train$DMC)^2
DMC3 <- (forestfires.train$DMC)^3

DC2 <- (forestfires.train$DC)^2
DC3 <- (forestfires.train$DC)^3

ISI2 <- (forestfires.train$ISI)^2
ISI3 <- (forestfires.train$ISI)^3

temp2 <- (forestfires.train$temp)^2
temp3 <- (forestfires.train$temp)^3

RH2 <- (forestfires.train$RH)^2
RH3 <- (forestfires.train$RH)^3

wind2 <- (forestfires.train$wind)^2
wind3 <- (forestfires.train$wind)^3

rain2 <- (forestfires.train$rain)^2
rain3 <- (forestfires.train$rain)^3

lenearmodel <- lm( y ~ forestfires.train$FFMC + I(FFMC2) + I(FFMC3) +
                     forestfires.train$DMC + I(DMC2) + I(DMC3) +
                     forestfires.train$DC + I(DC2) + I(DC3) +
                     forestfires.train$ISI + (ISI2) + (ISI3) +
                     forestfires.train$temp + I(temp2) + I(temp3) +
                     forestfires.train$RH + I(RH2) + I(RH3) +
                     forestfires.train$wind + I(wind2) + I(wind3) +
                     forestfires.train$rain + I(rain2) + I(rain3) )

# SVM
library(e1071)
svm.model <- svm( area ~ X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain,scale = T,
                  data=forestfires.train,kernel="radial",cost=100,gamma=0.1);
testx <- as.matrix(forestfires.test[ , -13])
svm.pred <- predict(svm.model, testx,decision.values =TRUE);

# accuray <- svm.pred - forestfires.test[, 13]/forestfires.test[, 13]

summary(svm.model)

# SVM2

library(e1071)
svm.model <-svm(area~X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind,scale = T,
                data=forestfires.train,kernel="radial",cost=100,gamma=0.1);
predictedarea <- predict(svm.model, forestfires.test[, -13], decision.values =TRUE);
#realarea <- as.matrix(forestfires.test[, 13])
#accuray <- (predictedarea - forestfires.test[, 13])/forestfires.test[, 13]
#fit <- table(svm.pred, testy)
#summary(svm.model)
#summary(fit)

error <- svm.model$residuals
rmse <- function(error)
  sqrt(mean(error^2))
rmse(error) 
mean(forestfires.train$area)



cor(forestfires.train$area, predict(svm.model))
accuracy <- (predict(svm.model) - forestfires.train$area)/forestfires.train$area
























