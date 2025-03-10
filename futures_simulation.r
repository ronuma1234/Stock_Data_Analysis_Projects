r0=0.02
theta=0.05
kappa=0.2 #then change to 5
sigma=0.015
T0=1
nt=252*5 #trading days per year
n=10  #number of simulated paths
maturity=T0
dt=maturity/(nt+1)  #delta t=t(i)-t(i-1)
timepoints=seq(0,maturity, dt)

nextone=function(current,kappa,theta,sigma,dt)
{
  first=exp(-kappa*dt)
  third=sqrt((sigma^2)*(1-first^2)/(2*kappa))
  return(first*current+theta*(1-first)+third*rnorm(1))
}

output=matrix(r0,(nt+2),n)

for(i in 2:(nt))
  {
  for(j in 1:n)
    {
    output[i,j]= nextone(output[i-1,j],kappa,theta,sigma,dt)
    }
}

plot(timepoints,output[,1], ylim=range(output,theta), type="l", col="purple",ylab="value")
for(j in 2:n){
  lines(timepoints,output[,j], col=colors()[floor(runif(1,1,657))] )
}
abline( h = theta, col = "red")
text(0, theta+0.005,expression(paste("long term mean:", theta)),adj=0)

title(main="Simulated paths of Vasicek model", col.main="red", font.main=4)