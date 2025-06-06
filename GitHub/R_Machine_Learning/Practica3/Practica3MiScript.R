#Introducción
library(tree)

#Ajuste de árboles para clasificación
library(ISLR)
attach(Carseats)
High <- factor(ifelse(Sales <=8,"No","Yes"))

Carseats = data.frame(Carseats, High)

tree.carseats = tree(High ~ . -Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats

set.seed(2)
train<-sample(1: nrow(Carseats ), 200)
Carseats.train<-Carseats[train ,]
Carseats.test<-Carseats[-train ,]
High.test=High[-train]
tree.carseats <- tree(High ~ . - Sales, Carseats.train)

tree.pred=predict(tree.carseats ,Carseats.test ,type="class")

table(tree.pred ,High.test)

# Calcular medida de observaciones de prueba correctamente clasificadas.
Accuracy = (104+50)/200

set.seed(3)
cv.carseats <- cv.tree(tree.carseats,FUN=prune.misclass )
names(cv.carseats )
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

prune.carseats =prune.misclass (tree.carseats ,best=13)
plot(prune.carseats )
text(prune.carseats ,pretty =0)

tree.pred=predict(prune.carseats ,Carseats.test , type="class")
table(tree.pred ,High.test)
Accuracy1 = (102+52)/200

#Ajuste de árboles para regresión
library(MASS)
set.seed(1)
train = sample (1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv ~ .,Boston , subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston , pretty =0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type='b')

yhat=predict (tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)

mean((yhat -boston.test)^2)

#Bagging y random forest
library( randomForest)

set.seed(1)
bag.boston= randomForest( medv~.,data=Boston , subset=train, mtry=13,importance =TRUE)
bag.boston

yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
plot(yhat.bag , boston.test)
abline (0,1)

mean((yhat.bag -boston.test)^2)

bag.boston= randomForest( medv~.,data=Boston , subset=train ,
                          mtry=13,ntree=25)
yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
mean((yhat.bag -boston.test)^2)

set.seed(1)
rf.boston= randomForest(medv~.,data=Boston , subset=train ,
                        mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)

importance(rf.boston)

varImpPlot(rf.boston)
