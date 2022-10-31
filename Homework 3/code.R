library(data.table)
setwd("C:/Users/User/Desktop/R_data")
alldata <- fread("csv_result-wave_2_classes_with_irrelevant_attributes.csv")
traindata <- fread("csv_result-wave_2_classes_with_irrelevant_attributes_train.csv")
testdata <- fread("csv_result-wave_2_classes_with_irrelevant_attributes_test.csv")
head(alldata)
summary(alldata)
attach(alldata)
table(alldata$sample)

traindata$group <- ifelse(traindata$classe=="A",1,0) 
testdata$group <- ifelse(testdata$classe=="A",1,0)
traindata = traindata[,-c(1,123)]
testdata = testdata[,-c(1,123)]

glm.fits_train=glm(group~.,data=traindata,family =binomial)
summary(glm.fits_train)
glm.probs_test = predict(glm.fits_train,testdata,type="response")
head(glm.probs_test)
glm.pred_test=rep("0",23334)
glm.pred_test[glm.probs_test >.5]="1"
table(glm.pred_test,testdata$group)
mean(glm.pred_test==testdata$group)
max(vif(glm.fits_train))
mean(vif(glm.fits_train))
median(vif(glm.fits_train))

prob <- predict(glm.fits_train, testdata, type="response")
library(InformationValue)
optCutOff <- optimalCutoff(actuals = testdata$group,predictedScores = prob)[1]
optCutOff
confusionMatrix(actuals = testdata$group,predictedScores = prob,threshold = optCutOff)
precision(actuals = testdata$group,predictedScores = prob,threshold = optCutOff)
plotROC(actuals = testdata$group,predictedScores = prob)

dataafterlasso <- traindata[,-1]
select.ind_new <- as.integer(c(select.ind,123)) 
dataafterlasso <- dataafterlasso[,c(1,4,5,6,9,10,11,12,13,14,15,16,17,18,19,20,21,22,25,29,35,36
                                     ,37,39,41,45,46,48,50,51,58,61,65,76,81,87,88,91,92,95,97,98,99,103
                                     ,105,116,119,121,123)]
glm.fits_train_lasso=glm(group~.,data=dataafterlasso ,family =binomial)
summary(glm.fits_train)
dataafterlasso_test <- testdata[,-1]
dataafterlasso_test <- dataafterlasso_test[,c(1,4,5,6,9,10,11,12,13,14,15,16,17,18,19,20,21,22,25,29,35,36
                                    ,37,39,41,45,46,48,50,51,58,61,65,76,81,87,88,91,92,95,97,98,99,103
                                    ,105,116,119,121,123)]


train_x <- model.matrix(group ~ ., data = traindata)
train_y <- traindata$group
test_x <- model.matrix(group ~ ., data = testdata)
test_y <- testdata$group

library(glmnet)
dataridge <- glmnet(x = train_x[,-1],y = train_y,alpha = 0,family="binomial")
plot(dataridge, xvar = "lambda",main="Ridge\n\n")
dataridge$lambda
dataridge_cv <- cv.glmnet(x = train_x[,-1],y = train_y,alpha = 0,family="binomial")
plot(dataridge_cv,main="Ridge Cross-Validation\n\n")
dataridge_cv$lambda.min
log(dataridge_cv$lambda.min)
plot(dataridge, xvar = "lambda",main="Ridge\n\n")+
  abline(v = log(dataridge_cv$lambda.min), col = "red", lty = "dashed")
ridge.test = predict(dataridge,s = dataridge_cv$lambda.min, newx = as.matrix(test_x[,-1]))
rsq <- function (x, y) cor(x, y) ^ 2
rsq(testdata$group, ridge.test)


library(magrittr)
library(dplyr)
library(ggplot2)
coef(dataridge_cv,s="lambda.min")%>%
  as.matrix() %>% 
  as.data.frame() %>% 
  add_rownames(var = "var") %>% 
  `colnames<-`(c("var","coef")) %>%
  filter(var != "(Intercept)") %>%  #剔除截距項
  top_n(25, wt = coef) %>% 
  ggplot(aes(coef, reorder(var, coef))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

datalasso <- glmnet(x = train_x[,-1],y = train_y,alpha = 1,family="binomial")
plot(datalasso,xvar = "lambda",main="Lasso \n\n")
datalasso_cv <- cv.glmnet(x = train_x[,-1],y = train_y,alpha = 1,family="binomial")
plot(datalasso_cv,main="Lasso Cross-Validation\n\n")
datalasso_cv$lambda.min
log(datalasso_cv$lambda.min)
plot(datalasso, xvar = "lambda",main="Lasso \n\n")+
abline(v = log(datalasso_cv$lambda.min), col = "red", lty = "dashed")
lasso.test = predict(datalasso,s = datalasso_cv$lambda.min, newx = as.matrix(test_x[,-1]))
rsq(testdata$group, lasso.test)
coef(datalasso_cv, s = "lambda.min")
select.ind = which(coef(datalasso_cv, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 
select.varialbes = colnames(traindata)[select.ind]
select.varialbes
select.varialbes_group <- c(select.varialbes,"group")
glm.fits_train_lasso=glm(group~.,data=traindata[,c(3,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20,21,28,34,35,36,38,44
                                                   ,45,47,50,64,75,80,87,90,91,94,96,97,102,104,115,118,120,122)],family =binomial)
summary(glm.fits_train_lasso)
glm.probs_test_lasso = predict(glm.fits_train_lasso,testdata,type="response")
head(glm.probs_test_lasso)
glm.pred_test_lasso=rep("0",23334)
glm.pred_test_lasso[glm.probs_test_lasso >.5]="1"
table(glm.pred_test_lasso,testdata$group)
mean(glm.pred_test_lasso==testdata$group)
vif(glm.fits_train_lasso)

coef(datalasso_cv,s="lambda.min")%>%
  as.matrix() %>% 
  as.data.frame() %>% 
  add_rownames(var = "var") %>% 
  `colnames<-`(c("var","coef")) %>%
  filter(var != "(Intercept)") %>%  #剔除截距項
  top_n(47, wt = coef) %>% 
  ggplot(aes(coef, reorder(var, coef))) +
  geom_point() +
  ggtitle("Top 47 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

lasso    <- glmnet(train_x, train_y, alpha = 1.0,family = "binomial") 
elastic1 <- glmnet(train_x, train_y, alpha = 0.75,family = "binomial") 
elastic2 <- glmnet(train_x, train_y, alpha = 0.25,family = "binomial") 
ridge    <- glmnet(train_x, train_y, alpha = 0.0,family = "binomial")

par(mfrow = c(2,2), mar = c(4,2,4,2), + 0.1)
plot(lasso, xvar = "lambda", main = "Lasso (Alpha = 1) \n\n")
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = 0.75) \n\n")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = 0.25) \n\n")
plot(ridge, xvar = "lambda", main = "Ridge (Alpha = 0) \n\n")

testdataframe=as.data.frame(testdata)
newX <- model.matrix(group~.,data=testdataframe)[,-1]
lasso_cv    <- cv.glmnet(x = train_x[,-1],y = train_y,alpha = 1,family="binomial")
min(lasso_cv$cvm)
pred_lasso_cv <- predict(lasso_cv,newx = newX, s = lasso_cv$lambda.min)
mean((test_y - pred_lasso_cv)^2)

elastic1_cv <- cv.glmnet(x = train_x[,-1],y = train_y,alpha = 0.75,family="binomial") 
min(elastic1_cv$cvm)
pred_elastic1_cv <- predict(elastic1_cv,newx = newX, s = elastic1_cv$lambda.min)
mean((test_y - pred_elastic1_cv)^2)

elastic2_cv <- cv.glmnet(x = train_x[,-1],y = train_y, alpha = 0.25,family = "binomial") 
min(elastic2_cv$cvm)
pred_elastic2_cv <- predict(elastic2_cv,newx = newX, s = elastic2_cv$lambda.min)
mean((test_y - pred_elastic2_cv)^2)

ridge_cv    <- cv.glmnet(x = train_x[,-1],y = train_y, alpha = 0,family = "binomial")
min(ridge_cv$cvm)
pred_ridge_cv <- predict(ridge_cv,newx = newX, s = ridge_cv$lambda.min)
mean((test_y - pred_ridge_cv)^2)

#Random Forest
install.packages("caret")
install.packages("h2o")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
library(dplyr)
library(magrittr)

rf3 <- randomForest(as.factor(group) ~ .,data = traindata, n_tree = 100)
rf3.pred.prob <- predict(object = rf3, newdata = testdata, type = "prob")
rf3.pred <- predict(rf3, newdata = testdata, type = "class")
tb3 <- table(rf3.pred, testdata$group)
tb3
(11020+10336)/23334
plotROC(actuals = testdata$group,predictedScores = rf3.pred.prob)

#SVM
library(kernlab)
svm4 <- ksvm(as.factor(group) ~ .,data = traindata)
svm4.pred.prob <- predict(svm4,testdata, type = "decision")
svm4.pred <- predict(svm4,testdata, type = "response")
tb4 <- table(svm4.pred, testdata$group)
tb4
(10876+10614)/23334
plotROC(actuals = testdata$group,predictedScores = svm4.pred.prob)
