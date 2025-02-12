library(ISLR)
library(GGally)
library(caret) # para conjuntos de validación y test
attach(Wage)

#Regresion con kNN
#Descripcion de datos
df <- read.csv("datos_reducidos_600.csv")
df<-df[,-which(names(df) == "X")]

#Descripicion del problem

#Analisis de las variables
ggpairs(df)

#Funcion de la distancia
distance <- function(a,b){
  sqrt(sum((a-b)^2))
}

#Seleccion de vecinos
nearest_neighbors <- function(x, obs, k, FUN){
  dist <- apply(x,1,FUN,obs)
  dist_ind <- cbind(1:length(dist),dist)
  sorted_dist <- dist_ind[order(dist_ind[, 2]),]
  neighbor_ind <- sorted_dist[1:k,1]
  return(neighbor_ind)
}
x <- df[1:(nrow(df)-1),-which(names(df) == "wage")]
obs <- df[nrow(df),-which(names(df) == "wage")]
print(obs)

nn <- nearest_neighbors(x,obs,7,distance)
neighbors<-as.matrix(x[nn,])
print(neighbors)
data_norm <- data.frame(scale(df[,-which(names(df) == "wage")]))

x_ <- data_norm[1:(nrow(df)-1),]
obs_ <- data_norm[nrow(df),]

nn2 <- nearest_neighbors(x_,obs_,7,distance)
neighbors2<-as.matrix(x[nn2,])
print(neighbors2)

#Prediccion
knn_prediction <- function(y,nn){
  mean(y[nn])
}

#Seleccion de hiperparametros
set.seed(1234) #para poder reproducir resultados (aleatorio)

trainIndex <- createDataPartition(df$wage, p = 0.8, list = FALSE, times = 1)
testIndex <- 1:600
testIndex <- testIndex[-trainIndex]
trainData <- df[trainIndex, ]
row.names(trainData)<-NULL
testData <- df[-trainIndex, ]
row.names(testData)<-NULL # reset indices

K <- 5  # Number of folds

# Perform k-fold cross-validation with approximately equal-sized folds
set.seed(123)  # For reproducibility
folds <- createFolds(1:nrow(trainData), k = K, list = TRUE)
cat("Number of folds:", length(folds), "\n")
for (i in 1:length(folds)) {
  cat("Fold", i, "size:", length(folds[[i]]), "\n")
}

kmax <- 20
msevec <- rep(0,kmax)
for (KK in 1:K) {
  valSet <- trainData[folds[[K]],]
  row.names(valSet)<-NULL
  trainSet <- trainData[-folds[[K]],]
  row.names(trainSet)<-NULL
  xVal <- valSet[,-which(names(valSet)=="wage")]
  yVal <- valSet[,which(names(valSet)=="wage")]
  xTrain  <- trainSet[,-which(names(trainSet)=="wage")]
  yTrain <- trainSet[,which(names(trainSet)=="wage")]
  print(KK)
  for (k in 1:kmax) {
    mse <- 0
    print(k)
    for (ii in 1:length(yVal)){
      print(ii)
      nn <- nearest_neighbors(xTrain,xVal[ii,],k = k,FUN = distance)
      pred <- knn_prediction(yTrain,nn = nn)
      mse <- mse + (pred - yVal[ii])^2
    }
    msevec[k] <- msevec[k] + mse/length(yVal)
  }  
  msevec <- msevec/K
}

plot(msevec, type="l")

k <- 11
error <- rep(0,length(yTest))
xTest <- testData[,-which(names(testData)=="wage")]
yTest <- testData[,which(names(testData)=="wage")]
xTrain  <- trainData[,-which(names(trainData)=="wage")]
yTrain <- trainData[,which(names(trainData)=="wage")]
mse <- 0
for (ii in 1:length(yTest)){
  #print(ii)
  nn <- nearest_neighbors(xTrain,xTest[ii,],k = k,FUN = distance)
  pred <- knn_prediction(yTrain,nn = nn)
  error[ii] <- pred - yTest[ii]
  mse <- mse + (error[ii])^2
}
mse <- mse/length(yTest)

print(mse)
plot(error,type="l")


xTrain <- data_norm[trainIndex, ]
yTrain <- trainData[,which(names(trainData)=="wage")]
xTest <- data_norm[-trainIndex,]
yTest <- testData[,which(names(testData)=="wage")]

k <- 6

mse <- 0
error <- rep(0,length(yTest))
for (ii in 1:length(yTest)){
  #print(ii)
  #print(df[testIndex[ii],])
  nn <- nearest_neighbors(xTrain,xTest[ii,],k = k,FUN = distance)
  #print(df[trainIndex[nn],])
  pred <- knn_prediction(yTrain,nn = nn)
  error[ii] <- pred - yTest[ii]
  mse <- mse + (error[ii])^2
}
mse <- mse/length(yTest)

print(mse)
plot(error,type="l")




#Clasificación con kNN
df$wage_above_125k <- ifelse(df$wage > 125, 1, 0)

trainData <- df[trainIndex, ]
row.names(trainData)<-NULL
testData <- df[-trainIndex, ]
row.names(testData)<-NULL # reset indices

Mode <- function(binary_vector){
  table_result <- table(binary_vector)
  mode_value <- as.numeric(names(table_result[table_result == max(table_result)]))
}

knn_class_prediction <- function(y,nn){
  Mode(y[nn])
}



xTrain <- data_norm[trainIndex, ]
yTrain <- trainData[,which(names(trainData)=="wage_above_125k")]
xTest <- data_norm[-trainIndex,]
yTest <- testData[,which(names(testData)=="wage_above_125k")]

k <- 6

pred <- rep(0,length(yTest))
for (ii in 1:length(yTest)){
  #print(ii)
  #print(df[testIndex[ii],])
  nn <- nearest_neighbors(xTrain,xTest[ii,],k = k,FUN = distance)
  #print(df[trainIndex[nn],])
  pred[ii] <- knn_class_prediction(yTrain,nn = nn)
}
actual <- yTest
predicted <- pred

confusionMatrix(data=as.factor(predicted),reference=as.factor(actual))













#Ejercicio
#Vamos a intentar buscar la k para que el error de nuestra regresión normalizada sea la mínima

#Normalizamos los datos
data_norm <- data.frame(scale(df[,-which(names(df) == "wage")]))

x_ <- data_norm[1:(nrow(df)-1),]
obs_ <- data_norm[nrow(df),]

nn2 <- nearest_neighbors(x_,obs_,7,distance)
neighbors2<-as.matrix(x[nn2,])
print(neighbors2)


#Dividimos el dataset normalizado
set.seed(1234) #para poder reproducir resultados (aleatorio)

trainIndex <- createDataPartition(df$wage, p = 0.8, list = FALSE, times = 1)
testIndex <- 1:600
testIndex <- testIndex[-trainIndex]
trainData <- data_norm[trainIndex, ]
row.names(trainData)<-NULL
testData <- data_norm[-trainIndex, ]
row.names(testData)<-NULL # reset indices


#Partimos el subconjunto de entrenamiento en 5 partes
K <- 5  # Number of folds

# Perform k-fold cross-validation with approximately equal-sized folds
set.seed(123)  # For reproducibility
folds <- createFolds(1:nrow(trainData), k = K, list = TRUE)
cat("Number of folds:", length(folds), "\n")
for (i in 1:length(folds)) {
  cat("Fold", i, "size:", length(folds[[i]]), "\n")
}


#Vamos a calculr la mejor k para los datos normalizados:
kmax <- 20
msevec <- rep(0,kmax)
for (KK in 1:K) {
  valSet <- trainData[folds[[K]],]
  row.names(valSet)<-NULL
  trainSet <- trainData[-folds[[K]],]
  row.names(trainSet)<-NULL
  xVal <- valSet[,-which(names(valSet)=="wage")]
  yVal <- valSet[,which(names(valSet)=="wage")]
  xTrain  <- trainSet[,-which(names(trainSet)=="wage")]
  yTrain <- trainSet[,which(names(trainSet)=="wage")]
  print(KK)
  for (k in 1:kmax) {
    mse <- 0
    print(k)
    for (ii in 1:length(yVal)){
      print(ii)
      nn <- nearest_neighbors(xTrain,xVal[ii,],k = k,FUN = distance)
      pred <- knn_prediction(yTrain,nn = nn)
      mse <- mse + (pred - yVal[ii])^2
    }
    msevec[k] <- msevec[k] + mse/length(yVal)
  }  
  msevec <- msevec/K
}

plot(msevec, type="l")





#Vamos a calcular el error que nos genera el modelo con la k óptima elegida
xTrain <- data_norm[trainIndex, ]
yTrain <- trainData[,which(names(trainData)=="wage")]
xTest <- data_norm[-trainIndex,]
yTest <- testData[,which(names(testData)=="wage")]

k <- 6

mse <- 0
error <- rep(0,length(yTest))
for (ii in 1:length(yTest)){
  #print(ii)
  #print(df[testIndex[ii],])
  nn <- nearest_neighbors(xTrain,xTest[ii,],k = k,FUN = distance)
  #print(df[trainIndex[nn],])
  pred <- knn_prediction(yTrain,nn = nn)
  error[ii] <- pred - yTest[ii]
  mse <- mse + (error[ii])^2
}
mse <- mse/length(yTest)

print(mse)
plot(error,type="l")



#El error generado con k=6 que es el de la practica es mse=1119.323
#Nuestro error con la k óptima k= es mse=