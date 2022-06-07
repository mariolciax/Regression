#kontynuacja regresji logistycznej

#--------------------Zadanie 1---------------
#wczytanie danych
dane<-read.table(file="SAheart.data",sep = ",",header=T,row.names=1)
#mamy do czynienia z malymi danymi 
#dzielimy dane na dwa zbiory: zbior treningowy i testowy

#przyciecie danych losowo - REPREZENTATYWNA PRÓBKA
#(poniewaz dane moga byc w pewien sposob uporzadkowane)
ciecie<-sample(1:nrow(dane),floor(0.7*nrow(dane))) 
trening<-dane[ciecie,]
test<-dane[-ciecie,]

#padlo haslo przeuczenie modelu (ang.overfitting)
#model zle dopasowny bedzie zle przewidywal ale nie moze byc zbyt dobrze dopasowany

model<-glm(chd~.,data=trening,family="binomial")
przewidziano<-predict(model,test,type="response")
porownanie<-as.data.frame(cbind(test[,ncol(test)],przewidziano ))

#klasyfikacja

y_pred<-as.numeric(przewidziano>0.5)
y_true<-test$chd


#------Wykonaj tabele klasyfikacji

#tabelka klasyfikacji
#ile razy jaka para wystepuje y-prawdziwy,y-przewidywany
#        y_pred 
#y_true  0  1
#0       74 15
#1       22 28
t<-table(y_true,y_pred)
t
 
#------Jaki procent pacjentów jest dobrze klasyfikowany

#jaki % jest dobrze sklasifikowanych przypadkow, 
#ja ten wzor kojarze z uczenia maszynowego, mozecie w necie poszukac 
sum(diag(t))/sum(t)
#kazdy ma inny wynik, poniewaz mamy losowosc w dobraniu danych


#------Jaki procent chorych pacjentów model rozpoznaje poprawnie?
#moj sposob
t2<-table(y_true[which(y_true==1)],y_pred[which(y_true==1)])
sum(diag(t2))/sum(t2)

#pana sposob z wykorzystaniem poprzedniej tabelki
t[2,2]/sum(t[2,])


#-----Dokonaj selekcji zmiennych uzywajac funkcji step. Dla mniejszego 
#----modelu wykonaj tabele klasyfikacji. Porównaj wyniki 
#-----z tymi otrzymanymi dla duzego modelu.

#MOJE- chyba zle
summary(model)
summary(step(model))
p1<-predict(step(model),test,type="response")
y_pred<-as.numeric(p1>0.5)
y_true<-test$chd
t<-table(y_true,y_pred)
t
sum(diag(t))/sum(t)


#Pana
model_bic<-step(model,k=log(nrow(trening)))
p1<-predict(model_bic,test,type="response")
y_pred<-as.numeric(p1>0.5)
y_true<-test$chd
t<-table(y_true,y_pred)
t
sum(diag(t))/sum(t)
t[2,2]/sum(t[2,])


#----3 model wyraznie gorszy
model<-glm(chd~alcohol+sbp,data=trening,family="binomial")
przewidziano<-predict(model,test,type="response")
porownanie<-as.data.frame(cbind(test[,ncol(test)],przewidziano ))

#klasyfikacja

y_pred<-as.numeric(przewidziano>0.5)
y_true<-test$chd


#------Wykonaj tabele klasyfikacji

t<-table(y_true,y_pred)
t

#------Jaki procent pacjentów jest dobrze klasyfikowany

#jaki % jest dobrze sklasifikowanych przypadkow, 
sum(diag(t))/sum(t)
t[2,2]/sum(t[2,])

