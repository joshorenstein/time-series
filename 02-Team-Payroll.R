library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)
library(forecast)
library(tseries)
library(ggthemes)
library(here)

#load data
b <- read.csv('baseball.csv')
View(b)
b <- b %>% arrange(Year) %>% filter(Year>1984)
b

#change date class to date type
b$Year <- as.Date(paste(b$Year, 1, 1, sep = "-"))

##CHECK FOR STATIONARITY##
names(b)
#check time series plot
ggplot(b,aes(x=Year,y=Team.Payroll)) + geom_line()
#check ACF plot
ggAcf(b$Team.Payroll,type="correlation")
#run ADF test
ggPacf(b$Team.Payroll,type="correlation")
##Transforming for Stationarity and Identifying Model Parameters
#fit AR model
ar.model.b  <- auto.arima(b$Team.Payroll,max.d=0,max.q=0,allowdrift=T)
ar.model.b
#fit MA model
ma.model.b  <- auto.arima(b$Team.Payroll,max.d=0,max.q=0,allowdrift=T)
ma.model.b

#fir ARMA model
arma.model.b  <- auto.arima(b$Team.Payroll,max.d=0,allowdrift=T)
arma.model.b


#fit ARIMA model
arima.model.b <- auto.arima(b$Team.Payroll,allowdrift=T)
arima.model.b

##Checking Residuals
#calculate residuals of each model
ar.residual.b  <- resid(ar.model.b)
ma.residual.b  <- resid(ma.model.b)
arma.residual.b  <- resid(arma.model.b)
arima.residual.b  <- resid(arima.model.b)

#plot PACF plot of each models residuals
ggAcf(ar.residual.b,type="partial")
ggAcf(ma.residual.b,type="partial")
ggAcf(arma.residual.b,type="partial")
ggAcf(arima.residual.b,type="partial")

#run the Ljung Box test on the residuals
Box.test(ar.residual.b,type="Ljung-Box",lag=1)
Box.test(ma.residual.b,type="Ljung-Box",lag=1)
Box.test(arma.residual.b,type="Ljung-Box",lag=1)
Box.test(arima.residual.b,type="Ljung-Box",lag=1)
#fail to reject the null of white noise in the model. generally, reasonably well behaved resiudals on the model so dont have to worry about non-stationarity

#Make a forecast
#make forecast for each model
??forecast
ar.forecast <- forecast(ar.model.b,h=10,level=80)
ma.forecast <- forecast(ma.model.b,h=10,level=80)
arma.forecast <- forecast(arma.model.b,h=10,level=80)
arima.forecast <- forecast(arima.model.b,h=10,level=80)

#plot forecast for each model
g1  <- autoplot(ar.forecast)
g2  <- autoplot(ma.forecast)
g3  <- autoplot(arma.forecast)
g4  <- autoplot(arima.forecast)

grid.arrange(g1,g2,g3,g4,nrow=2,ncol=2)
arima.forecast
