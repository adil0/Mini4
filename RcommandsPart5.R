library(quantmod)

# Fit ARCH(2) Model to MSFT Data

MSFT = getSymbols("MSFT", from="2010-1-1", to="2012-12-31", auto.assign=F)
logret = dailyReturn(MSFT, type="log")
holdfit = garchFit(~garch(2,0), data=logret, include.mean=FALSE)


# Calculating standardized residuals

sresids = residuals(holdfit)/holdfit@sigma.t


# Look at PACF of squared series to judge choice of p

pacf(logret^2)


# Look at normal probability plot of residuals

qqnorm(sresids)
qqline(sresids)


# Simulating a GARCH model

simout = garchSim(n=200, 
    garchSpec(list(omega=0.1,alpha=0.3,beta=0.5)),extended=T)


# Fitting a GARCH(1,1) Model

holdfit = garchFit(~garch(1,1), data=logret, include.mean=FALSE)


#-------------------------------------------------------------------
# GOOG Example

library(quantmod)
library(fGarch)

# Get data

GOOG = getSymbols("GOOG",from="2005-1-1",to="2013-4-19",
    auto.assign=F,src="google")

logret = dailyReturn(Cl(GOOG),type="log")


# Fit model with normal errors, construct qq plot

holdfit1 = garchFit(~garch(1,1),dat=logret, include.mean=F)

sresids = residuals(holdfit1)/holdfit1@sigma.t

qqnorm(sresids)
qqline(sresids)


# Fit model with T errors, construct qq plot

holdfit2 = garchFit(~garch(1,1),dat=logret, include.mean=F,
      cond.dist="std")
sresids = residuals(holdfit2)/holdfit2@sigma.t
plot(sort(sresids),qstd(ppoints(length(sresids)),
      nu=holdfit2@fit$matcoef[4,1]))
abline(0,1)


# Try skewed Version

holdfit3 = garchFit(~garch(1,1),dat=logret, include.mean=F,
                    cond.dist="sstd")
sresids = residuals(holdfit3)/holdfit3@sigma.t
plot(sort(sresids),qsstd(ppoints(length(sresids)),
      nu=holdfit3@fit$matcoef[5,1],xi=holdfit3@fit$matcoef[4,1]))
abline(0,1)


#------------------------------------------------------------------
# FORD Example

FORD = getSymbols("F", from ="1990-1-1", to="2012-12-31", 
   auto.assign=F)
logret = dailyReturn(Ad(FORD),type="log")

fordfit0 = garchFit(~garch(1,1), dat=logret, include.mean=F)

fordfit1 = garchFit(~aparch(1,1), dat=logret, include.mean=F)

attributes(fordfit0)$fit$ics
attributes(fordfit1)$fit$ics


# Try with delta = 2

fordfit2 = garchFit(~aparch(1,1), dat=logret, include.mean=F,
      delta=2, include.delta=FALSE)


# Fit combined ARMA/GARCH Model

library(forecast)

fordfit3 = auto.arima(logret,approx=F,step=F)

acf(residuals(fordfit3)^2)

fordfit4 = garchFit(~arma(0,5)+garch(1,1), dat=logret, include.mean=F)

