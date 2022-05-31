?glm()
?step()
?anova()
#regresja logistyczna 
#prawdopodobienstwo ze to bedzie 1 
#1/1+e^-beta z

#szansa , szansa, szansa
#wczytanie danych
kredyt<-read.table(file="kredit.asc",header = T)

#a)
model=glm(kredit~.,data=kredyt,family="binomial")

#b)
summary(model)
wsp<-summary(model)$coefficients[,1]

#iloczyn skalarny %*%
c(1,2) %*% c(10,10)
pierwszy_klient

prawdopodobienstwa <-function(beta,x){
  return(1/(1+exp(as.numeric(-1*beta) %*% as.numeric(x))))
}


prawdopodobienstwa(wsp,c(1,kredyt[1,2:ncol(kredyt)]))
klient1<-kredyt[1,2:ncol(kredyt)]

predict(model,klient1,type="response")
