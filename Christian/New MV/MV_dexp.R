#load these packages
library(tseries)
library(XML)
library(foreign)
library(lubridate)

#Assumptions for MV models

#sample order is biased
#events in the past affect things less than events in the present
#the unbiased estimate of the future event is combination of a point estimate and a (linear)trend estimate
#errors are normally distributed


##set parameters
ticker = "AAPL"
ticker_1 = "AAPL"
ticker_2 = "CNK"
start<-as.Date("2007/01/01", "%Y/%m/%d")
end<-as.Date("2014/01/01", "%Y/%m/%d")

start<-as.Date("2009/01/01", "%Y/%m/%d")
end<-as.Date("2013/06/01", "%Y/%m/%d")


#set time series
ts_daily_prices = get.hist.quote(ticker,start= start,end = end, quote = "AdjClose", compression = "w", retclass = "zoo")
#interp_ts_daily_prices = interpNA(ts_daily_prices, method = "linear") #interpolate
#unsmoothed<-ts(interp_ts_daily_prices,start=decimal_date(start),frequency=365)
unsmoothed<-ts(as.numeric(ts_daily_prices),start=decimal_date(start),end=frequency=52)
plot(unsmoothed,main="aapl")

ts_daily_prices_1 = get.hist.quote(ticker_1,start= start,end = end, quote = "AdjClose", compression = "w", retclass = "zoo")
#interp_ts_daily_prices = interpNA(ts_daily_prices, method = "linear") #interpolate
#unsmoothed<-ts(interp_ts_daily_prices,start=decimal_date(start),frequency=365)
stock_1<-ts(as.numeric(ts_daily_prices_1),start=decimal_date(start),frequency=52)
plot(stock_1,main="aapl")

ts_daily_prices_2 = get.hist.quote(ticker_2,start= start,end = end, quote = "AdjClose", compression = "w", retclass = "zoo")
#interp_ts_daily_prices = interpNA(ts_daily_prices, method = "linear") #interpolate
#unsmoothed<-ts(interp_ts_daily_prices,start=decimal_date(start),frequency=365)
stock_2<-ts(as.numeric(ts_daily_prices_2),start=decimal_date(start),frequency=52)
plot(stock_2,main="CNK")

###########
#Functions#
###########

#Double Exponential

double_exp<-function(unsmoothed,alpha=.1,beta=.1,greed=0,predict_ahead=1){
  
  x<-unsmoothed
  #Set initial values
  x_length<-length(x)
  s<-NULL;s[1:2]<-x[1:2]
  b<-NULL;b[1:2]<-x[2]-x[1]
  p<-NULL;p[1:2]<-s[1:2] #predict
  
  #Smooth Functions s= smoothed b=trend
  for(t in 3:length(unsmoothed)){
    s[t] = alpha*x[t] + (1-alpha)*(s[t-1]+b[t-1])
    b[t] = beta*(s[t]-s[t-1])+(1-beta)*b[t-1]
    p[t]=  greed*x[t-1]+(1-greed)*s[t-1]+b[t-1]
  }
  
  last_expected_return<-(predict_ahead*b[t])/x[x_length]
  
  results<-list(x,s,p,last_expected_return)
  names(results)<-c("unsmoothed","smoothed","prediction","expect_return")
  plot(as.numeric(unsmoothed),type="l");lines(y=s,x=1:length(s),col="red");lines(y=p,x=1:length(p),col="blue")
  
  return(results)
}

###DEMO###
results_1<-double_exp(unsmoothed,greed=1)
plot(as.numeric(results_1[[1]]),type="l");lines(y=results_1[[2]],x=1:length(s),col="red");lines(y=results_1[[3]],x=1:length(p),col="blue")

results_2<-double_exp(unsmoothed,greed=0)
plot(as.numeric(results_2[[1]]),type="l");lines(y=results_2[[2]],x=1:length(s),col="red");lines(y=results_2[[3]],x=1:length(p),col="blue")

results_3<-double_exp(unsmoothed,greed=.5)
plot(as.numeric(results_3[[1]]),type="l");lines(y=results_3[[2]],x=1:length(s),col="red");lines(y=results_3[[3]],x=1:length(p),col="blue")


###Risk###

get_risk<-function(results_1){
  mse<-sum((results_1[[3]]-results_1[[1]])^2) #the srt of predicted - actual
  return(sqrt(mse))
}

get_risk(results_1)
get_risk(results_2)
get_risk(results_3)

mse<-sum((unsmoothed[-1]-unsmoothed[-length(unsmoothed)])^2)
sqrt(mse)

###Portfolio###
combine_port<-function(stock_1,stock_2,weight_1=.5,weight_2=.5){
  
  #normalize
  n_ratio<-stock_1[1]/stock_2[1]
  stock_2_n<-stock_2*n_ratio
  stock_2_n[1]
  
  #combine
  portfolio<-weight_1*stock_1+weight_2*stock_2_n
  plot(portfolio,main="portfolio")
  lines(stock_2_n,col="red")
  lines(stock_1,col="blue")
  
  return(portfolio) 
}

###Average Returns###

get_ave_return<-function(portfolio){
  total_return<-(portfolio[length(portfolio)]-portfolio[1])
  first_price<-portfolio[1]
  last_price<-portfolio[length(portfolio)]
  ave_return<-exp(log(portfolio[length(portfolio)]/portfolio[1])/length(portfolio))
  #portfolio[1]*ave_return^length(portfolio) #test 
  return(ave_return-1)
}

#get_ave_return(portfolio)

###Make Real MV###

MV_curve<-function(stock_1,stock_2,alpha=.08,beta=.08,greed=0){
  #make variables
  ave_returns<-NULL
  risks<-NULL
  
  p_ratios_1 <- seq(0,1,.01)  
  weight_1<-p_ratios_1
  weight_2<-1-weight_1
  
  for(i in 1:length(p_ratios_1)){
    combined_port<-combine_port(stock_1,stock_2,weight_1=weight_1[i],
                                weight_2=weight_2[i])
    
    results_1<-double_exp(combined_port,alpha=alpha,beta=beta,greed=greed)
    ave_returns<-c(ave_returns,get_ave_return(combined_port))
    risks<-c(risks,get_risk(results_1))
  }
  plot(risks,ave_returns,main="Mean vs Risk")
  
  MV_results<-list(ave_returns,risks)
  names(MV_results)<-c("returns","risks")
  return(MV_results)
}

###Make Real Expected Return MV###

Expect_MV_curve<-function(stock_1,stock_2,alpha=.06,beta=.06,greed=0){
  #make variables
  expect_returns<- NULL
  risks<-NULL
  
  p_ratios_1 <- seq(0,1,.01)  
  weight_1<-p_ratios_1
  weight_2<-1-weight_1
  
  
  for(i in 1:length(p_ratios_1)){
    combined_port<-combine_port(stock_1,stock_2,weight_1=weight_1[i],
                                weight_2=weight_2[i])
    
    results_1<-double_exp(combined_port,alpha=alpha,beta=beta,greed=greed)
    expect_returns<-c(expect_returns,results_1[[4]])
    risks<-c(risks,get_risk(results_1))
  }
  plot(risks,expect_returns,main="Expected Returns vs Risk")
  
  MV_results<-data.frame(expect_returns,risks,weight_1,weight_2)
  #names(MV_results)<-c("expect_returns","risks",)
  return(MV_results)
}

###OLD MV MODEL###

OLD_MV<-function(stock_1,stock_2){
#Establish returns of stocks
l_stock<-length(stock_1)
  
r1 <- (stock_1[-1]-stock_1[-l_stock])/stock_1[-l_stock]
r2 <- (stock_2[-1]-stock_2[-l_stock])/stock_2[-l_stock]

#Place the returns together in a data frame:
returns <- as.data.frame(cbind(r1,r2))

#Create many portfolios (combinations of the two stocks):
a <- seq(0,1,.01)
b <- 1-a

#Compute the expected return of each portfolio:
rp_bar <- a*mean(returns$r1)+b*mean(returns$r2)

#Compute the variance and standard deviation of each portfolio:
var_p <- a^2*var(returns$r1)+b^2*var(returns$r2)+
  2*a*b*cov(returns$r1,returns$r2)

sd_p <- var_p^.5
qq <- as.data.frame(cbind(sd_p, rp_bar))

#get the minimum variance 
x1 <- (var(returns$r2)-cov(returns$r1,returns$r2))/
  (var(returns$r1)+var(returns$r2)-2*cov(returns$r1,returns$r2))
x2 <- 1-x1
rp_bar_min <- x1*mean(returns$r1)+x2*mean(returns$r2)
sd_p_min <- (x1^2*var(returns$r1)+x2^2*var(returns$r2)+
               2*x1*x2*cov(returns$r1,returns$r2))^0.5
qqq <- qq[qq$rp_bar > rp_bar_min,]

#And then draw the efficient frontier:
plot(qq, xlab="Portfolio risk (standard deviation)", 
     ylab="Expected return", type="l",main="MV Efficient Frontier")
points(qqq, col="blue", type="l", lwd=5)
legend(x="bottomright",legend=c("efficiency frontier","original point","new point")
       ,fill=c("blue","green","red"))

}

###DEMO###
double_exp(stock_1,alpha=.06,beta=.06)[[4]]
double_exp(stock_2,alpha=.06,beta=.06)[[4]]

get_risk(double_exp(stock_1,alpha=.2,beta=.2))
get_risk(double_exp(combined_port,alpha=.2,beta=.2))

plot(stock_1)
plot(stock_2)

MV_curve(stock_1,stock_2,alpha=.06,beta=.06,greed=.5)
Expect_MV_curve(stock_1,stock_2,alpha=.06,beta=.06,greed=.5)
OLD_MV(stock_1,stock_2)

#DEBUG #plot(as.numeric(unsmoothed),type="l");lines(y=c(s,predict),x=1:length(c(s,predict)),col="red")


