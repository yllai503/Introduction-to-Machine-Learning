library(data.table)
setwd("C:/Users/User/Desktop/R_data")
white <- fread("winequality-white.csv")
red <-fread("winequality-red.csv")
allwine <- fread("allwine.csv")
attach(allwine)
head(allwine)
str(allwine)
library(ggplot2)
allwine12=allwine
corr=cor(allwine123)
corred=cor(red)
corrwhite=cor(white)
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr,type = "lower",lab = TRUE)
ggcorrplot(corred,type = "lower",lab = TRUE)
ggcorrplot(corrwhite,type = "lower",lab = TRUE)

ggplot(aes(x=`fixed acidity`),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=`volatile acidity`),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=`citric acid`),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=`residual sugar`),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=chlorides),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=`free sulfur dioxide`),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=`total sulfur dioxide`),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=density),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=pH),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=sulphates),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=alcohol),data =allwine) + 
  geom_density(alpha = 0.4,aes(fill = category))+
  theme_light()
ggplot(aes(x=quality),data =allwine) + 
  geom_bar(alpha = 0.4,position = "dodge",aes(fill = category))+
  scale_x_continuous(breaks=c(0:10))+
  theme_light()

summary(allwine)
allwine[,13]=as.numeric(allwine[,13]=="white")
allwine$group <- ifelse(category == "white", 1, 0) 
attach(allwine)
table(group)
glm.fits=glm(group~`fixed acidity`+`volatile acidity`+`citric acid`+
               `residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+
               density+pH+sulphates+alcohol+quality,
             data=allwine ,family =binomial)
summary(glm.fits)
library(car)
vif(glm.fits)
glm.probs = predict(glm.fits,type="response")
head(glm.probs)
glm.pred=rep("red",6497)
glm.pred[glm.probs >.5]="white"
table(glm.pred,category)
mean(glm.pred==category)

attach(allwine)
library(dplyr)
train_white=rep(F,4898)
train_white[sample(4898,1000)]=T
train_red=rep(F,1599)
train_red[sample(1599,1000)]=T
train=c(train_white,train_red)
test=allwine[!train,]
category.test=category[!train]
group.test=group[!train]
glm.fits_train=glm(group~`fixed acidity`+`volatile acidity`+`citric acid`+
               `residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+
               density+sulphates+alcohol+quality,
             data=allwine ,family =binomial,subset = train)
summary(glm.fits_train)
glm.probs_test = predict(glm.fits_train,test,type="response")
head(glm.probs_test)
glm.pred_test=rep("red",4497)
glm.pred_test[glm.probs_test >.5]="white"
table(glm.pred_test,category.test)
mean(glm.pred_test==category.test)

library(MASS)
lda.fit=lda(group~`fixed acidity`+`volatile acidity`+`citric acid`+
              `residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+
              density+sulphates+alcohol+quality,data=allwine ,subset =train)
lda.fit
lda.pred=predict(lda.fit,test)
lda.class =lda.pred$class
table(lda.class,category.test)
(595+3881)/4497

qda.fit=lda(group~`fixed acidity`+`volatile acidity`+`citric acid`+
              `residual sugar`+chlorides+`free sulfur dioxide`+`total sulfur dioxide`+
              density+sulphates+alcohol+quality,data=allwine ,subset =train)
qda.fit
qda.pred=predict(qda.fit,test)
qda.class =qda.pred$class
table(lda.class,category.test)
(595+3881)/4497

library(class)
train.X=cbind(`fixed acidity`,`volatile acidity`,`citric acid`,
                `residual sugar`,chlorides,`free sulfur dioxide`,`total sulfur dioxide`,
                density,sulphates,alcohol,quality)[train ,]
test.X=cbind(`fixed acidity`,`volatile acidity`,`citric acid`,
              `residual sugar`,chlorides,`free sulfur dioxide`,`total sulfur dioxide`,
              density,sulphates,alcohol,quality)[!train ,]
train.category=category[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.category,k=200)
table(knn.pred,category.test)
mean(knn.pred==category.test)

plot(lda.fit)
plot(qda.fit)
plot(knn.pred)
means <- colSums(lda.fit$prior * lda.fit$means)
train.mod <- scale(train, center = means, scale = FALSE) %*% lda.fit$scaling
