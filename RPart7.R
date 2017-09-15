#------------------------------------------------------------------------
# The AR(2) Example

library(dlm)

# This function needs to return: m0, C0, FF, GG, V, W

# The parameter order is (phi1, phi2, sigma2)

buildAR2 = function(x)
{
   FF = matrix(c(1,0),nrow=1)

   GG = matrix(c(x[1],1,x[2],0),nrow=2)

   V = 0

   W = matrix(c(exp(x[3]),0,0,0),nrow=2)

   m0 = rep(0,2)

   C0 = matrix(solve(diag(4) - GG %x% t(GG)) %*%
         matrix(W,ncol=1),ncol=2)

   return(list(m0 = m0, C0 = C0, FF = FF, GG = GG,
       V = V, W = W))
}

simseries = arima.sim(list(order=c(2,0,0),ar=c(0.5,0.1)),n=1000,sd=0.1)
holdfit = dlmMLE(simseries,c(0,0,1),buildAR2)



#------------------------------------------------------------------------
# Local Trends Model


# This function needs to return: m0, C0, FF, GG, V, W
# The parameter order is (sigma2e, sigma2eta, alpha)

buildLT = function(x)
{
   FF = diag(1)
  
   GG = diag(1)*x[3]

   V = diag(1) * exp(x[1])

   W = diag(1) * exp(x[2])

   m0 = diag(1)

   C0 = diag(1) * 1e7

   return(list(m0 = m0, C0 = C0, FF = FF, GG = GG, 
       V = V, W = W))
}

# Read in the data

realvol = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/aa-3rv.txt")

rvser = log(realvol[,2])

holdfit = dlmMLE(rvser,c(0,0,1),buildLT)

filtout = dlmFilter(rvser,buildLT(holdfit$par))
smoothout = dlmSmooth(rvser,buildLT(holdfit$par))


postscript(file="serandfilt.eps",width=8,height=6,horiz=F)
plot(ts(rvser),lwd=1,cex.axis=1.3,cex.lab=1.3,ylab="Log Volatility")
points(ts(filtout$m),lwd=4,col=4,lty=1,type="l")
dev.off()

postscript(file="serandsmoo.eps",width=8,height=6,horiz=F)
plot(ts(rvser),lwd=1,cex.axis=1.3,cex.lab=1.3,ylab="Log Volatility")
points(ts(smoothout$s),lwd=4,col=4,lty=1,type="l")
dev.off()


#------------------------------------------------------------------------
# CAPM Example


# This function needs to return: m0, C0, FF, GG, V, W
# The parameter order is (sigma2eta, sigma2tau, sigma2eps)

buildVarCoefReg = function(pars,x)
{
   FF = matrix(c(1,1),nrow=1)
 
   JFF = matrix(c(0,1),nrow=1)

   X = matrix(x,ncol=1)
 
   GG = diag(2)

   W = diag(c(exp(pars[1]),exp(pars[2])))

   V = diag(1) * exp(pars[3])

   m0 = c(1,1)

   C0 = diag(2) * 1e7

   return(list(m0 = m0, C0 = C0, FF = FF, GG = GG, 
       V = V, W = W, JFF=JFF, X=X))
}

# Obtain Data

library(quantmod)
source("http://www.stat.cmu.edu/~cschafer/MSCF/getFamaFrench.txt")


PNC = getSymbols("PNC", from="2012-1-1", 
   to="2012-6-30", auto.assign=F)

ffhold = getFamaFrench(from = "2012-1-1", 
   to="2012-6-30")

ffhold$PNCexret = 100*dailyReturn(PNC) - ffhold$RF

holdfit = dlmMLE(ffhold$PNCexret, c(0,0,0), buildVarCoefReg,
   x=ffhold$Mkt.RF)

filtout = dlmFilter(ffhold$PNCexret, 
   buildVarCoefReg(holdfit$par,ffhold$Mkt.RF))
smoothout = dlmSmooth(ffhold$PNCexret, 
   buildVarCoefReg(holdfit$par,ffhold$Mkt.RF))


#------------------------------------------------------------------------
# Stochastic Vol Example

# This function needs to return: m0, C0, FF, GG, V, W
# The parameter order is (alpha, sigma2tau)
buildSV = function(x)
{
   FF = matrix(c(-1.27,1),nrow=1)
  
   GG = matrix(c(1,0,0,x[1]),nrow=2)

   V = diag(1) * pi^2/2

   W = matrix(c(0,0,0,exp(x[2])),nrow=2)

   m0 = matrix(c(1,1),nrow=1)

   C0 = diag(c(0,1e7))

   return(list(m0 = m0, C0 = C0, FF = FF, GG = GG, 
       V = V, W = W))
}

# Read in the data
library(quantmod)

IBM = getSymbols("IBM",from="2013-1-1",to="2013-12-31",auto.assign=F)
holdser = log(dailyReturn(Ad(IBM))^2)
holdser = holdser[-1]

holdfit = dlmMLE(holdser,c(0,0),buildSV)
filtout = dlmFilter(rvser,buildLT(holdfit$par))
smoothout = dlmSmooth(rvser,buildLT(holdfit$par))

postscript(file="serandfilt.eps",width=8,height=6,horiz=F)
plot(ts(rvser),lwd=1,cex.axis=1.3,cex.lab=1.3,ylab="Log Volatility")
points(ts(filtout$m),lwd=4,col=4,lty=1,type="l")
dev.off()

postscript(file="serandsmoo.eps",width=8,height=6,horiz=F)
plot(ts(rvser),lwd=1,cex.axis=1.3,cex.lab=1.3,ylab="Log Volatility")
points(ts(smoothout$s),lwd=4,col=4,lty=1,type="l")
dev.off()
#------------------------------------------------------------------------
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
durbinWatsonTest(holdfit,10)

