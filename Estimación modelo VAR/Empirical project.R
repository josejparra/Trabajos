library(readxl)
library(vars)
library(urca)
library(normtest)     
library(tsDyn)
library(aod)
library(zoo)
library(tseries)

######################### Data reading ###############################################################
data <- read.csv("Empirical Project.csv",sep=";")[,1:10]
gdp<-ts(data=data[2],start=1950,frequency = 1)
  plot(gdp)
labor<-ts(data=data[3],start=1950,frequency = 1)
    plot(labor)
cap<-ts(data=data[5],start=1950,frequency = 1)
    plot(cap)
soc<-ts(data=data[6],start=1950,frequency = 1)
    plot(soc)
infr<-ts(data=data[7],start=1950,frequency = 1)
    plot(infr)
just<-ts(data=data[8],start=1950,frequency = 1)
    plot(just)
admin<-ts(data=data[9],start=1950,frequency = 1)
    plot(admin)
debt<-ts(data=data[10],start=1950,frequency = 1)
    plot(debt)

par(mfrow=c(3,3))    
    
#Dividing by labor
    
y=gdp/labor
plot(y)
k=cap/labor
g1=soc
g2=infr
g3=just
g4=admin
g5=debt

plot(y)
plot(k)
plot(g1)
plot(g2)
plot(g3)
plot(g4)
plot(g5)

g=g1+g2+g3+g4+g5
############################ unit root test #########################################
#y
testy=ur.df(y,type=c("trend"), selectlags=c("AIC"))
    summary(testy)
    plot(diff(y))
testy2=ur.df(diff(y),type=c("none"), lags=2)
    summary(testy2)
    plot(testy2)
    
#k
test_k=ur.df(k,type=c("trend"), selectlags=c("AIC"))
    summary(test_k)
    plot(diff(k))
test_k2=ur.df(diff(k),type=c("none"),selectlags=c("AIC"))
    summary(test_k2)

#soc
plot(g1)
test_soc=ur.df(g1,type=c("trend"), selectlags=c("AIC"))
    summary(test_soc)
   
test_soc2=ur.df(diff(soc),type=c("none"),selectlags=c("AIC"))
  summary(test_soc2)
  

#infr
plot(g2)
test_infr=ur.df(g2,type=c("none"),selectlags=c("AIC"))
    summary(test_infr)
    plot(diff(g2))
test_infr2=ur.df(diff(g2),type=c("none"),selectlags=c("AIC"))
  summary(test_infr2)

#just
plot(g3)
test_just=ur.df(g3,type=c("drift"), selectlags=c("AIC"))
    summary(test_just)
    plot(diff(g3))
test_just2=ur.df(diff(g3),type=c("none"),selectlags=c("AIC"))
    summary(test_just2)
    

#admin
plot(g4)
test_admin=ur.df(g4,type=c("trend"), selectlags=c("AIC"))
    summary(test_admin)
    plot(diff(g4))
test_admin2=ur.df(diff(g4),type=c("none"),selectlags=c("AIC"))
  summary(test_admin2)
 

#debt
plot(g5)
test_debt=ur.df(g5,type=c("trend"),selectlags=c("AIC"))
  summary(test_debt)
  plot(diff(g5))
test_debt2=ur.df(diff(g5),type=c("none"),selectlags=c("AIC"))
  summary(test_debt2)
 
  

########################## Cointegration test ###################################################
series<-cbind(y,k,g1,g2,g3,g4,g5)#data matrix
VARselect(series)#Select lags
#Johansen test
summary(ca.jo(series, type = "trace", K = 4))
#4 cointegration relationships

###################################### Estimation#####################################
var.2 <- VAR(series, p = 2)
var.g1.3 <- VAR(series[,c("y","k","g1")], p = 3)
var.g2.4 <- VAR(series[,c("y","k","g2")], p = 4)
var.g3.3 <- VAR(series[,c("y","k","g3")], p = 3)
var.g4.3 <- VAR(series[,c("y","k","g4")], p = 3)
var.g5.3 <- VAR(series[,c("y","k","g5")], p = 3)
series2<-cbind(y,k,g)
var.g<- VAR(series2, p = 4)

######################## Diagnostics #########################################
# Pormanteau test
serial.test(var.2, lags.pt = 16)
serial.test(var.g1.3, lags.pt = 16)
serial.test(var.g2.4, lags.pt = 16)
serial.test(var.g3.3, lags.pt = 16)
serial.test(var.g4.3, lags.pt = 16)
serial.test(var.g5.3, lags.pt = 16)
serial.test(var.g, lags.pt = 16)

# Jarque-Bera multivariate normality test
normality.test(var.2)
normality.test(var.g1.3)
normality.test(var.g2.4)
normality.test(var.g3.3)
normality.test(var.g4.3)
normality.test(var.g5.3)
normality.test(var.g)



############################# Inference ###################################################
#Toda-Yamamoto adjustment for Wald test
var.3<- VAR(series, p = 3)
var.g1.4 <- VAR(series[,c("y","k","g1")], p = 4)
var.g2.5 <- VAR(series[,c("y","k","g2")], p = 5)
var.g3.4 <- VAR(series[,c("y","k","g3")], p = 4)
var.g4.4 <- VAR(series[,c("y","k","g4")], p = 4)
var.g5.4 <- VAR(series[,c("y","k","g5")], p = 4)
var.g.5<- VAR(series2, p = 5)

#Wald-test (H0: public expenditure doesn't cause gdp)
wald.test(b=coef(var.3$varresult[[1]]), Sigma=vcov(var.3$varresult[[1]]), Terms=c(3:7,10:14,17:21))
# rejected at  10% but not at  5%
#wald-test (H0: social public expenditure doesn't cause gdp)
wald.test(b=coef(var.g1.4$varresult[[1]]), Sigma=vcov(var.g1.4$varresult[[1]]), Terms=c(3,6,9,12))
# rejected  1% 
#wald-test (H0: public expenditure in infrastructure doesn't cause gdp)
wald.test(b=coef(var.g2.5$varresult[[1]]), Sigma=vcov(var.g2.5$varresult[[1]]), Terms=c(3,6,9,12))
#it's not rejected 
#wald-test (H0: public expenditure in justice doesn't cause gdp)
wald.test(b=coef(var.g3.4$varresult[[1]]), Sigma=vcov(var.g3.4$varresult[[1]]), Terms=c(3,6,9,12))
#it's not rejected 
#wald-test (H0: admin expenditure doesn't cause gdp)
wald.test(b=coef(var.g4.4$varresult[[1]]), Sigma=vcov(var.g4.4$varresult[[1]]), Terms=c(3,6,9,12))
#it's not rejected 
#wald-test (H0: public expenditure in debt doesn't cause gdp)
wald.test(b=coef(var.g5.4$varresult[[1]]), Sigma=vcov(var.g5.4$varresult[[1]]), Terms=c(3,6,9,12))
#it's not rejected 

#wald-test (H0: gdp doesn't cause expenditure)
wald.test(b=coef(var.g.5$varresult[[3]]), Sigma=vcov(var.g.5$varresult[[3]]), Terms=c(1,4,7,10,13))
#it's not rejected 
#wald-test (H0: gdp doesn't cause social expenditure)
wald.test(b=coef(var.g1.4$varresult[[3]]), Sigma=vcov(var.g1.4$varresult[[3]]), Terms=c(1,4,7,10))
# it's not rejected 
#wald-test (H0: gdp doesn't cause expenditure in infrastructure)
wald.test(b=coef(var.g2.5$varresult[[3]]), Sigma=vcov(var.g2.5$varresult[[3]]), Terms=c(1,4,7,10))
#Se rejected  1%
#wald-test (H0: gdp doesn't cause expenditure in justice)
wald.test(b=coef(var.g3.4$varresult[[3]]), Sigma=vcov(var.g3.4$varresult[[3]]), Terms=c(1,4,7,10))
#it's not rejected 
#wald-test (H0: gdp doesn't cause admin expenditure)
wald.test(b=coef(var.g4.4$varresult[[3]]), Sigma=vcov(var.g4.4$varresult[[3]]), Terms=c(1,4,7,10))
#it's not rejected 
#wald-test (H0: gdp doesn't cause expenditure in debt)
wald.test(b=coef(var.g5.4$varresult[[3]]), Sigma=vcov(var.g5.4$varresult[[3]]), Terms=c(1,4,7,10))
#it's not rejected  at 10% but it is at 5%

