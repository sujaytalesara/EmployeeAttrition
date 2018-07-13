library(randomForest)
library(mlbench)
library(caret)
library(caretEnsemble)
library(lattice)
library(ggplot2)
library(corrplot)
library(dplyr)
library(ggplot2)
library(gbm)
library(ROCR)
library(pROC)


data.frame <- data.frame(winequality.white)
sum(is.na(data.frame))

# Distribution plot -----------------------------------------------------------------
barplot(table(data.frame$quality))
cor(data.frame$density,data.frame$quality)


# Heatplot-----------------------------------------------------------------
cor.white <- cor(data.frame, use='pairwise')
corrplot(cor.white, method = 'number')


# Feature Engineering -----------------------------------------
data.frame <- data.frame[-3]
data.frame <- data.frame[-5]

data.frame$taste <- ifelse(data.frame$quality < 6, 'bad', 'excellent')
data.frame$taste[data.frame$quality == 6] <- 'average'
data.frame$taste <- as.factor(data.frame$taste)

# Feature Scaling
#data.frame <- scale(data.frame)


# Training & Test Data -----------------------------------------------------------------
set.seed(123)
samp <- sample(nrow(data.frame), 0.6 * nrow(data.frame))
train <- data.frame[samp, ]
test <- data.frame[-samp, ]


# Random Forest Model -----------------------------------------------------------------
fit.rf <- randomForest(taste ~ ., data = train)
plot(fit.rf$importance)



# Stochastic Gradient Boosting Model -----------------------------------------------------------------
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=3)
fit.gbm <- train(taste ~ .- quality, data=train, method="gbm",
                 metric=metric, trControl=control, verbose=FALSE)


# Prediction -----------------------------------------------------------------
pred.rf <- predict(fit.rf, newdata = test)
pred.gbm <- predict(fit.gbm , newdata = test)

# Accuracy -----------------------------------------------------------------
confusionMatrix(pred.rf,test$taste)
confusionMatrix(pred.gbm,test$taste)

# Plotting -----------------------------------------------------------------
barplot(pred.rf, test$taste,xlab="predicted",ylab="actual")
table(pred.rf, test$quality)


plot(pred.gbm, test$taste,xlab="predicted",ylab="actual")
table(pred.gbm, test$taste)

# ROC Curve -----------------------------------------------------------------
plot(roc(test$taste,as.numeric(pred.rf)))
plot(roc(test$taste,as.numeric(pred.gbm)))

