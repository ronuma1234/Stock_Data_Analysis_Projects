mydata=read.csv("/Users/robertwillfindyou/Downloads/ibm_vol1516.csv") 
mydata
summary(mydata)
colnames(mydata)#names of the columns of the data
dat=mydata[1] #gives first column (Date)
price=mydata[2]
hist=mydata[4] # gives 30 days historical vol
impl=mydata[7] #gives implied vol
class(dat) #class data.frame


hist=as.vector(as.matrix(hist))  #convert from data.frame class to vector
dat=as.Date(mydata$Date,"%m/%d/%Y")
impl=as.vector(as.matrix(impl))
price=as.vector(as.matrix(price))

#plot historical and implied vol superimposed 
plot(dat,hist,type="o",ylim=c(15,40),ylab="vol")
par(new=T)
plot(dat,impl,type="o",col="red",ylim=c(15,40),ylab="vol",lty=2)
legend(dat[124],40,c("historical","implied"),col=c("blue","red"), lty=1:2)
arrows(dat[5],hist[5]+5,dat[5],hist[5],col="blue")
text(dat[5],hist[5]+6,"cluster",col="blue")

#compute log-returns of prices
log_returns = diff(log(price), lag=1)
plot(log_returns,type="o")
abline(h=0)

#fit GARCH(1,1) to log returns
require(tseries)
print(garch(log_returns)) #may give warning message: "singular information"

#in case of singular information default to FTSE data below
##############################################################################
#fit GARCH(1,1) to FTSE
data(EuStockMarkets) #load time series data - this is old, get new from Bloomberg
plot(EuStockMarkets[,"FTSE"]) #price data plot
ftse=diff(log(EuStockMarkets))[,"FTSE"] # calculate log returns for FTSE
plot(ftse,main="FTSE log-returns",col="red")
abline(h=0)

ftse_garch=garch(ftse) #fit GARCH(1,1)
print(summary(ftse_garch)) # accept or reject normality of residuals based on Jarque-Bera test?
round(ftse_garch$coef,6) #gives GARCH(1,1) coefficients, write the model as in page 4

plot(ftse_garch)  #several plots, check residuals for normality

qqnorm(ftse_garch$residuals) # qq plot of residuals of GARCH(1,1) vs normal
qqline(ftse_garch$residuals) #notice deviations from the line in both tails

#jarque.bera.test(ftse_garch$residuals)   # not working because of NA missing observations

rr=raw_residuals=residuals(ftse_garch) # may have NA missing observations
missing_residuals=rr[rr==NA]
filtered_residuals=setdiff(raw_residuals,missing_residuals) # no more missing observations
jarque.bera.test(filtered_residuals)  #rejects H0: normality of residuals for small p-values

library(astsa)
acf2(filtered_residuals)  # should be mostly within the band just like white noise

#alternatives
library(fGarch)
model=garchFit(~garch(1,1),data=ftse)
summary(model)