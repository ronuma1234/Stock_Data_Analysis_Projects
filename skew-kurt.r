#Using RStudio interface to change working directory where you save the script and the data
#Session-Set working Directory - to source file location
mydata=read.csv("/Users/robertwillfindyou/Downloads/prices.csv",header=T,sep=",")
#second column has price, first has dates, third has volume etc.
print(summary(mydata[2]))
#library moments has built-in functions for skewnewss and kurtosis
library(moments)
ku=kurtosis(mydata[2])
sk=skewness(mydata[2])
prices=as.vector(as.matrix(mydata[2]))
hist(prices)
#mode=names(sort(table(prices)))[length(sort(table(prices)))]

if(sk>0) {
    print("positive skewnewss, skewed to the right")
    } else {
        if(sk<0) {
                print("negative skewnewss, skewed to the left")
                 }
 else {print("symmetric")}
         }

if(ku>3) {
  print("excess kurtosis>0, leptokurtic (fat tails)")
} else {
  if(ku<3) {
    print("excess kurtosis<0, platykurtic (thin tails)")
  }
  else {print("zero excess kurtosis, mesokurtic")}
}
