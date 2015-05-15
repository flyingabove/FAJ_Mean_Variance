
#Problems with OLD MV Model
#Biased Estimators
#Hindsight Bias
#Not Weighted Through time

#OLD MEAN DEMO
a1<-c(1,3,4,7,11,11,11,29,37)
r1=(a1[-1]-a1[-length(a1)])/a1[-length(a1)]
mean(r1)
var(r1)

a2<-c(1,3,4,7,11,2,11,29,37)
r2=(a2[-1]-a2[-length(a2)])/a2[-length(a2)]
mean(r2)
var(r2)

#new MV
gao_smooth<-function(x,alpha=.5,beta=.5,gamma=0){
  
  smoothed<-c(x[1],x[2],rep(0,length(x)-2))
  b_trend<-c(0,x[2]-x[1],rep(0,length(x)-2))
  predict<-c(x[1],x[2],rep(0,length(x)-1))
  for(t in 3:length(x)){
    smoothed[t]=alpha*x[t] + (1-alpha)*(smoothed[t-1]+b_trend[t-1])
    b_trend[t]=beta*(smoothed[t]-smoothed[t-1])+(1-beta)*b_trend[t-1]
    predict[t]=gamma*x[t-1]+(1-gamma)*smoothed[t-1]+b_trend[t-1]
  }
  predict[t+1]=gamma*x[t]+(1-gamma)*smoothed[t]+b_trend[t]
  return(list(smoothed,predict))
}

x<-c(1,1,1,1,1,1,1,1,20)
plot(1:9,x,xlim=c(0,10),ylim=c(0,30))
smoothed<-gao_smooth(x,gamma=1)
lines(smoothed[[1]]) #smoothed
lines(smoothed[[2]],col="red") #predict
(smoothed[[2]][length(smoothed[[2]])]-20)/20#new mean
resid<-(smoothed[[2]][-10]-x)/x
MSS<-sum(resid)/9
MSS

plot(1:9,a1,xlim=c(0,10),ylim=c(0,50))
smoothed<-gao_smooth(a1,gamma=1)
lines(smoothed[[1]]) #smoothed
lines(smoothed[[2]],col="red") #predict
(smoothed[[2]][length(smoothed[[2]])]-37)/37#new mean
resid<-(smoothed[[2]][-10]-a1)/a1
MSS<-sum(resid)/9
MSS

plot(1:9,a2,xlim=c(0,10),ylim=c(0,50))
smoothed<-gao_smooth(a2,gamma=1)
lines(smoothed[[1]]) #smoothed
lines(smoothed[[2]],col="red") #predict
(smoothed[[2]][length(smoothed[[2]])]-37)/37#new mean
resid<-(smoothed[[2]][-10]-a2)/a2
MSS<-sum(resid)/9
MSS







