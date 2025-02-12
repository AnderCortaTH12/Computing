#Carga de los datos
library(readxl)
library(ggplot2)
library(GGally)
df = read_excel("finalData.xlsx")
print(df)

#Análisis descriptivo
#Ventas totales
ventasTotales <- df[df$Province=="Madrid",!(names(df) %in% c("Province", "SalesProvince", "Population"))]
summary(ventasTotales)
colnames(ventasTotales)
ventasTotales$YearFactor <- as.factor(ventasTotales$Year)
pp <- ggplot(data = ventasTotales,aes(x=Month,y=SalesTotal,color=YearFactor))+
  geom_line(size=1) + # para dibujar las lineas
  theme_bw() #cambia el formato de la figura
pp
pp_2 <- ggplot(data = ventasTotales,aes(fill=YearFactor,y=SalesTotal))+
  geom_boxplot(size=1) + # para dibujar el boxplot
  theme_bw() +
  ggtitle("Ventas por año")
pp_2
colnames(ventasTotales)
ggpairs(ventasTotales[,c(2,9,10,4,8)], title="correlogram with ggpairs()") 
dfplot <- data.frame(Date = rep(ventasTotales$Date,2),
                     Price = c(ventasTotales$Price,ventasTotales$PriceM),
                     Type = rep(c("Price","PriceM"),each=nrow(ventasTotales)))
dfplot$Type <- as.factor(dfplot$Type)

pp_3 <- ggplot(data = dfplot,aes(x=Date,y=Price,color=Type))+
  geom_line(size=1) + # para dibujar las lineas
  theme_bw() #cambia el formato de la figura
pp_3
pp_4 <- ggplot(data = ventasTotales,aes(x=Date,y=UnemploymentRate))+
  geom_line(size=1) + # para dibujar las lineas
  theme_bw() #cambia el formato de la figura
pp_4
dfplot <- data.frame(Date = rep(ventasTotales$Date,2),
                     Days = c(ventasTotales$MonthDays,ventasTotales$WorkingDays),
                     Type = rep(c("MonthDays","WorkingDays"),each=nrow(ventasTotales)))
dfplot$Type <- as.factor(dfplot$Type)

pp_5 <- ggplot(data = dfplot,aes(x=Date,y=Days,color=Type))+
  geom_line(size=1) + # para dibujar las lineas
  theme_bw() #cambia el formato de la figura
pp_5
ventasTotales$WorkingDaysFactor <- as.factor(ventasTotales$WorkingDays)
pp_6 <- ggplot(data = ventasTotales,aes(fill=WorkingDaysFactor,y=SalesTotal))+
  geom_boxplot(size=1) + # para dibujar el boxplot
  theme_bw() +
  ggtitle("Ventas en función del número de días laborales")
pp_6
ventasTotales$MonthDaysFactor <- as.factor(ventasTotales$MonthDays)
pp_7 <- ggplot(data = ventasTotales,aes(fill=MonthDaysFactor,y=SalesTotal))+
  geom_boxplot(size=1) + # para dibujar el boxplot
  theme_bw() +
  ggtitle("Ventas en función del número de días laborales")
pp_7

#Ventas por provincia
ventasProvincia <- df[,-which(names(df)=="SalesTotal")]
ventasProvincia$Province <- as.factor(ventasProvincia$Province)
pp_8 <- ggplot(data = ventasProvincia,aes(x=Province,y=SalesProvince))+
  geom_boxplot(size=1) + # para dibujar el boxplot
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Ventas por provincia")
pp_8
dfplot <- data.frame(Date = rep(ventasProvincia[which(ventasProvincia$Province=="Madrid"),]$Date,2),
                     Sales = c(ventasProvincia[which(ventasProvincia$Province=="Madrid"),]$SalesProvince,ventasProvincia[which(ventasProvincia$Province=="Barcelona"),]$SalesProvince),
                     Provincia = rep(c("Madrid","Barcelona"),each=nrow(ventasProvincia[which(ventasProvincia$Province=="Madrid"),])))
dfplot$Provincia <- as.factor(dfplot$Provincia)

pp_9 <- ggplot(data = dfplot,aes(x=Date,y=Sales,color=Provincia))+
  geom_line(size=1) + # para dibujar las lineas
  theme_bw() #cambia el formato de la figura
pp_9
dfplot <- data.frame(Date = rep(ventasProvincia[which(ventasProvincia$Province=="Ceuta"),]$Date,2),
                     Sales = c(ventasProvincia[which(ventasProvincia$Province=="Ceuta"),]$SalesProvince,ventasProvincia[which(ventasProvincia$Province=="Melilla"),]$SalesProvince),
                     Provincia = rep(c("Ceuta","Melilla"),each=nrow(ventasProvincia[which(ventasProvincia$Province=="Ceuta"),])))
dfplot$Provincia <- as.factor(dfplot$Provincia)

pp_9 <- ggplot(data = dfplot,aes(x=Date,y=Sales,color=Provincia))+
  geom_line(size=1) + # para dibujar las lineas
  theme_bw() #cambia el formato de la figura
pp_9

#Analisis de regresion
#Ventas totales
ventasTotales <- subset(ventasTotales, select = -Date) # eliminamos la variable date, ya que esa información está en Month y Year
mdl <- lm(data=ventasTotales, SalesTotal ~ factor(Year) + factor(Month) + . - Year - Month)
summary(mdl)
mdl2 <- lm(data=ventasTotales, SalesTotal ~ factor(Year) + factor(Month) + . - Year - Month - PriceM)
summary(mdl2)
mdl3 <- lm(data=ventasTotales, SalesTotal ~ factor(Year) + factor(Month) + . - Year - Month - PriceM - UnemploymentRate)
summary(mdl3)
mdl4 <- lm(data=ventasTotales, SalesTotal ~ factor(Year) + factor(Month) + . - Year - Month - PriceM - OvernightStays)
summary(mdl4)
mdl4 <- lm(data=ventasTotales, SalesTotal ~ factor(Year) + factor(Month) + . - Year - Month - PriceM - OvernightStays - UnemploymentRate)
summary(mdl4)
mdl5 <- lm(data=ventasTotales, SalesTotal ~  factor(Month) + WorkingDays + Price)
summary(mdl5)
plot(mdl5)
ventasTotales$logPrice <- log10(ventasTotales$Price)
ventasTotales$logSalesTotal <- log10(ventasTotales$SalesTotal)

mdllog <- lm(data=ventasTotales, logSalesTotal ~ factor(Month) + WorkingDays + logPrice)
summary(mdllog)
newData <- ventasTotales
newData$Price <- 1.1*newData$Price
ventasPredict <- predict(mdl5,newdata = newData)

mean((ventasPredict-ventasTotales$SalesTotal)/ventasTotales$SalesTotal)

#Prediccion
ventasTotales$ShiftSalesTotal <- c(rep(NA,12),ventasTotales$SalesTotal[-(37:48)])
ventasTotales$ShiftUnemploymentRate <- c(rep(NA,12),ventasTotales$UnemploymentRate[-(37:48)])
ventasTotales$ShiftOvernightStays <- c(rep(NA,12),ventasTotales$OvernightStays[-(37:48)])
head(ventasTotales)
ventasTotales$Month <- as.factor(ventasTotales$Month)
train <- ventasTotales[ventasTotales$Year==2014 | ventasTotales$Year==2015,]
head(train)
modelo_train <- lm(SalesTotal~WorkingDays + Month + Year + ShiftSalesTotal,data=train)
test <- ventasTotales[ventasTotales$Year==2016,]
head(test)
prediction <- predict(modelo_train, newdata = test)
summary(modelo_train)
sum(abs(prediction-test$SalesTotal))/12
#relative error (unkown metric name)
sum(prediction - test$SalesTotal)/sum(test$SalesTotal)
plot(test$SalesTotal,type="b",pch=16,ylim =c(1.5e8,2.3e8))
points(prediction,type="b",pch=16,col="red")

#Nuestra prediccion
modelo_train1 = lm(SalesTotal~  Month  + ShiftSalesTotal , data=train)
summary(modelo_train1)
prediction1 = predict(modelo_train1, newdata = test)
sum(abs(prediction1-test$SalesTotal))/12
sum(prediction1 - test$SalesTotal)/sum(test$SalesTotal)

train2 = ventasTotales[ventasTotales$Year==2014 | ventasTotales$Year==2015 | ventasTotales$Year==2016,]
modelo_train2 = lm(SalesTotal~  Month  + ShiftSalesTotal , data=train2)
datos_2016 = ventasTotales[ventasTotales$Year == 2016,]
datos_2017 = data.frame("Month" = as.factor(1:12), "ShiftSalesTotal" = datos_2016$ShiftSalesTotal)
prediction2 = predict(modelo_train2, newdata = datos_2017)
plot(prediction2,type="b",pch=16,col="black")
