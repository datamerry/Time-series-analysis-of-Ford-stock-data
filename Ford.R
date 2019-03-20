
install.packages('fpp')
require(fpp)

setwd ("~/Desktop")
da= read.csv("F.csv",header = T)
fprice= da$Adj.Close
plot.ts(fprice)

f.simplereturns=diff(fprice)/ fprice[-length(fprice)]
f.returns = log(1+f.simplereturns)
mean_returns = mean(f.returns)
f_center_returns = f.returns - mean_returns

plot.ts(f_center_returns)

acf(f_center_returns)

pacf(f_center_returns)

acf(f_center_returns^2)
pacf(f_center_returns^2)

Box.test(f_center_returns)

Box.test(f_center_returns^2)

#there is a garch effect, autocorrelation on the volatility of the center returns

fit1 = ar(f_center_returns)
fit1
library(fGarch)
m2=garchFit(~arma(5,0)+garch(1,1),data=f_center_returns,trace=F)
m2
par(mar=c(1,1,1,1))
tsdisplay(residuals(m2))
m3=garchFit(~arma(5,0)+garch(1,1),data=f_center_returns,trace=F,cond.dist=c("std"))
m3
tsdisplay(residuals(m3))
#m2:-5.357481
#m3:-5.477046
#i will choose m3

m4 = garchFit(~arma(1,0)+garch(1,1),data=f_center_returns,trace=F)
tsdisplay(residuals(m4))
m5 = garchFit(~arma(1,0)+garch(1,1),data=f_center_returns,trace=F,cond.dist=c("std"))
tsdisplay(residuals(m5))
#m4: aic -5.356243
#m5: aic -5.474096
# I chose model m5 with student t-innovation which has the lowest AIC
#model arma(1,0) garch(1,1) is used. 

predict(m5,10)
