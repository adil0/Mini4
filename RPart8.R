library(quantmod)
library(forecast)
library(car)

OneYearTreas = as.numeric(getSymbols("WGS1YR", src="FRED", auto.assign=F))
ThreeYearTreas = as.numeric(getSymbols("WGS3YR", src="FRED", auto.assign=F))


holdfit1 = lm(ThreeYearTreas ~ OneYearTreas)
holdfit2 = arima(ThreeYearTreas, order=c(1,1,1), xreg = OneYearTreas)
holdfit3 = auto.arima(ThreeYearTreas, xreg = OneYearTreas, d=1, 
      max.order=10,approx=F, step=F)

plot(OneYearTreas, ThreeYearTreas)

durbinWatsonTest(holdfit1,10)

