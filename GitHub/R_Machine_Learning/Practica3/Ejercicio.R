#Introducción 
library(tree)
library(ISLR)
attach(Carseats)

#Creamos nuestro trainset
set.seed(1)
train = sample (1:nrow(Carseats), nrow(Carseats)/2)
tree.carseats=tree(Sales ~ .,Carseats , subset=train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats , pretty =0)

cv.carseats=cv.tree(tree.carseats)
plot(cv.carseats$size ,cv.carseats$dev ,type='b')

#No tenemos que podar el arbol ya que su mejor tamaño es el máximo (18)
#con lo cual vamos a hacer la prediccion con el arbol original

yhat=predict (tree.carseats ,newdata=Carseats[-train ,])
carseats.test=Carseats[-train ,"Sales"]
plot(yhat ,carseats.test)
abline (0,1)

mean((yhat -carseats.test)^2)

#Calculamos un MSE de 4.92, lo que quiere decir que, aproximadamente, el 
#error cometido de media será de cerca de 2.24(tomando la raíz cuadrada del MSE).

#BAGGING
library( randomForest)

set.seed(1)
bag.carseats= randomForest( Sales~.,data=Carseats , subset=train, mtry=10,importance =TRUE)
bag.carseats

yhat.bag = predict (bag.carseats , newdata=Carseats[-train ,])
plot(yhat.bag , carseats.test)
abline (-2,1.2)

mean((yhat.bag -carseats.test)^2)

#Vemos cómo hemos mejorado el MSE de prueba con respecto al modelo anterior.
#En este caso obtenomos un MSE de 2.60 con lo cual el error cometido medio sera de 1.61

importance(bag.carseats)
varImpPlot(bag.carseats)

#Las variables más importantes son el precio(Price) y ShelvLoc.

#RANDOM FOREST
set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                        mtry=10, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=9, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=8, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=7, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=6, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=5, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=4, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=3, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=2, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats= randomForest(Sales~.,data=Carseats , subset=train ,
                          mtry=1, importance =TRUE)
yhat.rf = predict(rf.carseats ,newdata=Carseats[-train ,])
mean((yhat.rf-carseats.test)^2)

#Habiendo probado toadas las opciones m el mejor resultado nos lo da m=9
#del m=10 al m=9 el error baja pero si seguimos bajando el valor de m el error irá aumentando positivamente

importance(rf.carseats)

varImpPlot(rf.carseats)

#Las variables más importantes siguen siendo el Price y el ShelveLoc