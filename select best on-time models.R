
lastd<-"7/20"  #the date of doing inseason estimate (last day is 7/30)

obs.cpue<-read.table("obsCPUE.csv", sep = ",", header=T)#input 2021 observed daily cpue from test fishery.table
coefs<-read.table("NLINCOEF.csv", sep = ",", header=T)#input historic run curve a, b parameters 

#Y=1/(1+exp(-(a+b*d))) #d is day
#use 1979~2020 historical run timing curve to calculate cumulated proporiton of total run or cpue, y, it is y_yr,d in Equation 5
#first day is June 24; and last year is July 31.
firstd<-"6/24"
n<-as.Date(lastd, format="%m/%d") - as.Date(firstd, format="%m/%d") +1 #total days of test fishery
n<-as.numeric(n) #total days of test fishery
day<-rep(1:n)
y<-rep(NA, n)
m=matrix(NA, nrow=1, ncol=4)
colnames(m)=c("yr", "lag","total_cpue", "mse") 
m=as.data.frame(m)
mylag<-(10:-10) #from run 10-day early  to run 10-day late.lag of run as early(>0) or late(<0) as 10 days.
#mylag<-(5:-5)
#mylag<-0 #assume on-time only
my.yr<-coefs$year #histrical run curves of 1979~2016
for (yr in my.yr){
  a<-coefs$a[which(coefs$year==yr)] #take a, b for a particular year
  b<-coefs$b[which(coefs$year==yr)]
  for (lag in mylag){ #
    for (d in 1:n){
      #run may not on time. lag>0: run early; lag<0: run late
      y[d]<-1/(1+exp(-a-b*(d+lag)))#cumulated proportion by day d of total cpue (ccum cpue by final day).
    }
    yh=cbind(day, y) #cumulative proportion of ccpuef, final day of season's ccpue from historic curve 
    colnames(yh)=c("d", "y") #day and proportion
    x<-obs.cpue$dailyCPUE #to calculate cumulated cpue below
    obs.cpue$ccumCPUE[!is.na(x)] <- cumsum(na.omit(x)) #calculate cumulated cpue from daily cpue
    comb=merge(yh, obs.cpue, by="d", all=T)
    comb$sq.ccumCPUE<-with(comb, ccumCPUE^2)
    x<-comb$sq.ccumCPUE
    comb$ccum.sq.ccumCPUE[!is.na(x)]<-cumsum(na.omit(x))#cumulated values, numerator in equation 6
    comb$yc<-with(comb, y*ccumCPUE)
    x<-comb$yc
    comb$ccum.yc[!is.na(x)]<-cumsum(na.omit(x)) #denominator in equation 6
    comb$ccpuef<-with(comb, ccum.sq.ccumCPUE/ccum.yc) #a projection of the current year's CCPUEd at the end of the season 
    #comb$yd<-with(comb, ccumCPUE/ccpuef)
    ccpuef.D<-with(comb, ccpuef[n])#ccpuef on day D
    comb$yd<-with(comb, ccumCPUE/ccpuef.D) #obs.proportion of ccpue by day d. It is equation (7). Ycpue column in SABUND.OUT
    total_cpue<-comb$ccpuef[n] #estimated total cupe by the last day of test fishery (7/31)
    #calculate MSE
    x<-with(comb, sum((y-yd)^2,na.rm=TRUE))#sum of squared deviations
    #nrow(na.omit(comb)) to delete NA. It is # of days of test fishery
    mse<-with(comb, x/nrow(na.omit(comb))) #mean squared error (MSE)
    la<-cbind(yr, lag, total_cpue, mse)
    m=rbind(m,la)
  }
}

m <- na.omit(m) 
m <- with(m, m[order(mse),]) #sorted by MSE

best5<-m[1:5,] #best 5 models with smallest MSE.
best.yr<-best5$yr
n.yr<-length(best.yr)
for ( i in 1: n.yr){
  best5$yr.mean[i]<-coefs$mean[which(coefs$year==best.yr[i])]
}  
best5$mid.run<-as.Date(best5$yr.mean,format="%d-%b")-best5$lag
best5$mid.run<-with(best5, format(mid.run, format="%m/%d"))

