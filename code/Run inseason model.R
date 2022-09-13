obs.cpue<-read.table("obsCPUE.csv", sep = ",", header=T)#input observed daily cpue from test fishery.table
coefs<-read.table("NLINCOEF.csv", sep = ",", header=T)#input historic run curve a, b parameters 
#Y=1/(1+exp(-(a+b*d))) #d is day
#use 1979~2021 historical run timing curve to calculate cumulated proporiton of total run or cpue, y, it is y_yr,d in Equation 5
#first day is June 24; and last year is July 31.
firstd<-"6/24"
lastd<-"7/27" #to do inseason estimate before 7/31, just change "lastd", like "7/20"
#lastd<-"7/14" 
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
#m <- with(m, m[order(yr, mse),]) #sorted by yr and MSE
m <- with(m, m[order(mse),]) #if only on-time (lag=0) is fitted, sorted by mse only
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
comb2$cum_RUN<-NA
comb2$cum_RUN[comb2$d ==17]<-676409 #cumulative run by July 10 
comb2$cum_RUN[comb2$d ==18]<-943717 #cumulative run by July 11
comb2$cum_RUN[comb2$d ==19]<-932432  #cumulative run by July 12
comb2$cum_RUN[comb2$d ==21]<-1393660  #cumulative run by July 14

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
#mylag<-(-10:10) #lag of run as early(>0) or late(<0) as 10 days.
#mylag<-best.lags$lag #read lags from 5 best models
mylag<-best5$lag #read lags from 5 best models
#my.yr<-coefs$year #histrical run curves of 1979~2016
#my.yr<-best.lags$yr #read historical years of 5 best run curve
my.yr<-best5$yr #read historical years of 5 best run curve
n.best<-length(my.yr)
for (i in 1:n.best){ #5 years
  a<-coefs$a[which(coefs$year==my.yr[i])] #take a, b for a particular year
  b<-coefs$b[which(coefs$year==my.yr[i])]
  #for (lag in mylag){ #
    for (d in 1:n){
      #run may not on time. lag>0: run early; lag<0: run late
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
#out.D$mid.run<-best.lags$mid.run
#out.D$mse<-best.lags$mse

out.D2<-merge(out.D, best5, by.x = "year", by.y="yr")

out.D2$remaining.run<-with(out.D2, round(out.D2$est.total.run - out.D2$cum_RUN))
out.D2 <- out.D2[c("year","lag.x", "mse", "date", "ccumCPUE.x","ccpuef","cum_RUN","q","pr","mid.run","est.total.run","remaining.run")]
out.D2sub <- out.D2[c("year","lag.x", "mse", "date","ccpuef","cum_RUN","mid.run","est.total.run","remaining.run")]
out.D2sub <- out.D2sub[order(out.D2sub$mse),] #sorted by mse

write.csv(out.D2sub,'est totalrun.csv')

#format() will change number to character, not good
#out.Dsub$cum_RUN<-format(out.Dsub$cum_RUN, big.mark=",", scientific=FALSE)
#out.Dsub$est.total.run<-format(out.Dsub$est.total.run, big.mark=",", scientific=FALSE)
#out.Dsub$remaining.run<-format(out.Dsub$remaining.run, big.mark=",", scientific=FALSE)
#table(out.D$mid.run)
#plot(table(out.D$mid.run))
#total.run<-out.D$est.total.run[which(out.D$year!= 2015)] #remove the outline
#summary(total.run) 
#boxplot(total.run)


#The code is to calcuate percentage of run remaining by day during inseason 
#preseason forecasts of 2022 for UCI, Kenai, and Kasilof
f.uci<-4.373
f.ke<-2.325
f.ka<-0.881

run.prop<-read.table("run percentage.csv", sep = ",", header=T)
#input mean %complete. the date format must be like "7/25"
#Bob DeCino said they are constant for several years

run.prop$run.uci<-with(run.prop, f.uci* ccum.prop.uci)
run.prop$run.remain.uci<- with(run.prop, f.uci - run.uci)

run.prop$run.ke<-with(run.prop, f.ke* ccum.prop.ke)
run.prop$run.remain.ke<-with(run.prop, f.ke - run.ke)
run.prop$prop.remain.ke<- with(run.prop,run.remain.ke / run.remain.uci)

run.prop$run.ka<-with(run.prop, f.ka * ccum.prop.ka)
run.prop$run.remain.ka<- with(run.prop, f.ka - run.ka) 
run.prop$prop.remain.ka<- with(run.prop, run.remain.ka / run.remain.uci)

day<-"7/14" #input a day for an inseason estimate, like "7/1" or "7/20"
PKd<-run.prop$prop.remain.ke[which(run.prop$Date == day)]#proprotion of Kenai run remaiming in Equation(16)
PAd<-run.prop$prop.remain.ka[which(run.prop$Date == day)] #proprotion of Kasilof run remaiming


#Projecting Kenai River Total Run
#Read daily Kenai Run data to estimate Kenai Run
out.D2sub$p.ke<-0.65 #proportion of run remaining that are Kenai sockeye on day D
out.D2sub$r.ke <- with(out.D2sub, p.ke * remaining.run) #remaining Kenai run to go 
out.D2sub$p.ka<-0.13 #proportion of run remaining that are Kasilof sockeye on day D
out.D2sub$r.ka <- with(out.D2sub, p.ka * remaining.run) #remaining Kasilof run to go 

kenai.data<-read.table("KenaiDailyRun2022.csv", sep = ",", header=T)
kenai.data$Date<-as.Date(kenai.data$Date, format="%m/%d") #there is bug here. need to figure it out
out.Dsub$cum.run.ke<-kenai.data$cumRun[which(kenai.data$Date==lastd)]
out.Dsub$Total.run.ke<-with(out.Dsub, r.ke + cum.run.ke)


