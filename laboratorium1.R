#norbertryciak@gmail.com   prowadzacy


dane <-read.table(file="SBP.txt",header=T, row.names = 1)
library(MASS)

?lm()
?abline()
?boxcox(MASS)
#Wykres rozproszenia
plot(dane$Age, dane$SBP)
model<-lm(SBP~Age, data=dane)


model_summary<-summary(model)
model_summary$coefficients
model_summary$residuals


model$fitted.values
model_summary$r.squared

abline(model, col="blue", lwd=2)

M<-max(dane$SBP)
s<-dane
wiersz<-
s<-dane[wiersz,]<-NULL



dane2<-dane[dane$SBP<max(dane$SBP),]
plot(dane2$Age, dane2$SBP)
model2<-lm(SBP~Age, data=dane2)
abline(model2)
summary(model2)$r.squared
#r2 jest pomocny wskaznik ale to nie jest wyrocznia


#ZADANIE2

dane <-read.table(file="windspeed.txt", sep="," , header=T)
dane$speed<-log(log(dane$speed)) #z logarytmem lepiej
plot(dane$speed,dane$output)
model3<-lm(output~speed,data=dane)
abline(model3, lwd=2, col="red")
summary(model3)$r.squared


library(MASS)
data("pressure")


plot(pressure$temperature,pressure$pressure^0.12)
model4<-lm(pressure~temperature,data=pressure)
abline(model4,lwd=2)
boxcox(model4,lambda=seq(0.1,0.15,0.01))
model5<-lm(I(pressure^0.12)~temperature,data=pressure)
abline(model4,lwd=2)
summary(model4)$r.squared

