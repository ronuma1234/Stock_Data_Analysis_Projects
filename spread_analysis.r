mydata=read.csv("/Users/robertwillfindyou/Downloads/spread.csv")
mydata
summary(mydata)
mydata[!complete.cases(mydata),] #list rows with missing values (NA)
newdata = mydata[complete.cases(mydata),] #omit NA
mydata=na.omit(mydata)
#mydata[is.na(mydata)]=0
newdata[!complete.cases(newdata)]
colnames(mydata)#names of the columns of the data
print(summary(mydata))
dat=mydata[1] #gives first column (Date)
eonia=mydata[3] # gives EONIA
ecb=mydata[4] #gives ECB
class(dat) #class data.frame
spread=eonia-ecb
#spread=log(eonia-ecb)


spread=as.vector(as.matrix(spread))  #convert from data.frame class to vector
eonia=as.vector(as.matrix(eonia))
ecb=as.vector(as.matrix(ecb))
dat=as.Date(newdata$Date,format="%d/%m/%Y")

#plot EONIA and ECB superimposed 
plot(dat,eonia,type="l",ylim=c(-0.4,0.7),ylab="rate")
abline(h=0,col="red")

require(astsa)
acf2(spread) #exp decay in ACF, one non-zero spike in PACF suggests AR(1)

sp=ts(spread)
lag.plot(sp,lag=1) # shows auto-dependence of lag 1

fit=arima(spread,order=c(1,0,0)) # estimation of AR(1), which is the discrete time equivalent of Vasicek
phi=fit$coef[1] #coeff of AR(1)
alpha=fit$coef[2] #constant drift
sigma=sqrt(fit$sigma2) #std dev of innovations
phi
alpha
sigma
