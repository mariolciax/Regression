#------------------------------------zad1-----------------------------------------
n=500
sigma=1

x=runif(n,1,10)
eps= rnorm(n,0,sigma)
y=2+3*sin(x)+log(x)+eps
plot(x,y)
curve(2+3*sin(x)+log(x),add=T,col=2)


dane=data.frame(y=y,x=x,x2=x^2,x3=x^3,x4=x^4,sin=sin(x),cos=cos(x),tan=tan(x),log=log(x))
model=lm(y~x+x2+x3+x4+sin+cos+tan+log,data=dane)
#model=lm(y~.,data=dane)   to samo co wyzej ale skrotowo


summary(model)


points(x,model$fitted.values,col=3)


model_aic=step(model,k=2) #Akaike
summary(model_aic)
points(x,model_aic$fitted.values,col=4)


model_bic=step(model,k=log(nrow(dane))) #
summary(model_bic)
points(x,model_bic$fitted.values,col=6)




#-------------------------------------------zad2-----------------------------
#install.packages("AER")
library(AER)
data("USMacroG")
MacroDif<-data.frame(apply(USMacroG,2,diff))
MacroDif<-MacroDif[,c("consumption","dpi","cpi","government","unemp")]
pairs(MacroDif)

cor(MacroDif)


