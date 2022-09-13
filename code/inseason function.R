#function of inseason estimates
inseason<-function(mydate=mydate){ 
  mydate<-as.Date(mydate,format="%m/%d")
  lastd<-acum.run$date[which(acum.run$date == mydate)] #up to the day to fit OTF data to historical run curves
  lastacum<-acum.run$acumRun[which(acum.run$date == mydate)]
 
  #Y=1/(1+exp(-(a+b*d))) #d is day
  #use 1979~2021 historical run timing curve to calculate cumulated proporiton of total run or cpue, y, it is y_yr,d in Equation 5
  #first day is June 24; and last year is July 31.
  firstd<-"6/24"
  #lastd<-"7/12" #to do inseason estimate before 7/31, just change "lastd", like "7/20"
  n<-as.Date(lastd, format="%m/%d") - as.Date(firstd, format="%m/%d") +1 #total days of test fishery
  n<-as.numeric(n) #total  days of test fishery
  day<-rep(1:n)
  y<-rep(NA, n) 
  m=matrix(NA, nrow=1, ncol=4)
  colnames(m)=c("yr", "lag","total_cpue", "mse") 
  m=as.data.frame(m)
  #mylag<-(10:-10) #from run 10-day early  to run 10-day late.lag of run as early(>0) or late(<0) as 10 days.
  #mylag<-(5:-5)
  mylag<-0 #assume on-time only
  my.yr<-coefs$year #histrical run curves of 1979~2021
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
  m <- with(m, m[order(mse),]) # sorted by mse only
  #Select minimun MSE wihin group yr (yr is historical years of run timing curves)
  #It is the best lag(time shift) within each of historical curves(1979~2016)
  best.lags<-m[m$mse==ave(m$mse, m$yr,FUN=min),]#select rows with a condition
  best.yr<-best.lags$yr
  n.yr<-length(best.yr)
  for ( i in 1: n.yr){
    best.lags$yr.mean[i]<-coefs$mean[which(coefs$year==best.yr[i])]
  }  
  best.lags$mid.run<-as.Date(best.lags$yr.mean,format="%d-%b")-best.lags$lag
  best.lags$mid.run<-with(best.lags, format(mid.run, format="%m/%d"))
  best.lags <- best.lags[c("yr", "yr.mean", "lag","mse","total_cpue","mid.run")]#re-order the column
  best5<-best.lags[1:5,]
  
  write.csv(best.lags,'fitted run curves.csv') 
  write.csv(best5,'best5.csv')
  
  #read daily harvest and escapement
  #calculate catchabily (q) and passage rate(PR_d) as of day d.
  comb2=obs.cpue
  comb2$date<-as.Date(comb2$date,format="%d-%b")
  comb2$cum_RUN<-NA
  comb2$cum_RUN[which(comb2$date == lastd)]<-acum.run$accumRun[which(acum.run$date == lastd)]  #cumulative run by the day of lastd
  
  #Catchability -the fraction of the available population taken by a defined unit of fishing effort
  comb2$q<-with(comb2, ccumCPUE/cum_RUN)
  #Passage rate (PRd), as of day d, 
  #is the expansion factor used to convert CPUE into estimated numbers of salmon passing 
  #the test fishing transect line into UCI
  comb2$pr<-1/comb2$q
  
  #Projection of Total run at the end of season using 5 best run timing models from history
  m=matrix(NA, nrow=1, ncol=5) #inital a data frame from a matrix
  colnames(m)=c("d","year","lag", "ccumCPUE", "ccpuef") 
  m=as.data.frame(m)
  mylag<-best5$lag #read lags from 5 best models
  my.yr<-best5$yr #read historical years of 5 best run curve
  n.best<-length(my.yr)
  for (i in 1:n.best){ #5 years
    a<-coefs$a[which(coefs$year==my.yr[i])] #take a, b for a particular year
    b<-coefs$b[which(coefs$year==my.yr[i])]
    for (d in 1:n){
         y[d]<-1/(1+exp(-a-b*(d+mylag[i])))#cumulated proportion by day d of total cpue (ccum cpue by final day).
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
    comb$year<-my.yr[i]
    comb$lag<-mylag[i]
    myvars<-c("d","year","lag", "ccumCPUE", "ccpuef")
    #d=1(June 24) to 38(July 31); year= which year's run curve is used to estimate ccpuef
    comb.sub<-comb[myvars]
    m<-rbind(m, comb.sub)
  }
  m <- m[-1,]
  
  out<-merge(m, comb2, by="d", all=T)
  #out <- out[order(out$year),] #sorted by year
  out$est.total.run<-with(out, pr*ccpuef)#Inseaon esitmates using 5 best historical run curves.It is TR_f in equation 11
  out <- subset(out, select = -ccumCPUE.y ) #remove duplicate column
  out$date<-as.Date(out$date,format="%d-%b")#convert character to date
  lastd<-as.Date(lastd,format="%m/%d") #change date format
  out.D<-out[which(out$date == lastd),] #retrieve inseason estimate at day D=lastd; date=lastd
  out.D$date<-with(out.D, format(date, format="%m/%d")) #romove meaningless year value from date
  
  out.D2<-merge(out.D, best5, by.x = "year", by.y="yr")
  
  out.D2$remaining.run<-with(out.D2, round(out.D2$est.total.run - out.D2$cum_RUN))
  out.D2 <- out.D2[c("year","lag.x", "mse", "date", "ccumCPUE.x","ccpuef","cum_RUN","q","pr","mid.run","est.total.run","remaining.run")]
  out.D2sub <- out.D2[c("year", "mse", "date","ccumCPUE.x","cum_RUN","ccpuef","pr","mid.run","est.total.run","remaining.run")]
  out.D2sub <- out.D2sub[order(out.D2sub$mse),] #sorted by mse
  
  out.D2sub
}
