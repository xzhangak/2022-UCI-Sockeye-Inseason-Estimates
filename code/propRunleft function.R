propRunleft<-function(){

  #setwd("H:/Upper Cook Inlet/Forecasts_RB/Inseason_Forecasts/using R/2022 Inseason/temp")
  #input mean cumulative proportion of run that came from UCI_SOCKEYE_RUN_BY_STOCK_2007-2011.xls by Mark Willette
  propRun<-read.table("cumProportion.csv", sep = ",", header=T) #average of proportions between 2007 and 2011
  #preseason forecasts for 2022 UCI, Kenai, and Kasilof
  propRun$uci.forecast<-4.967
  propRun$kenai.forecast<-2.902
  propRun$kasilof.forecast<-0.941
  #cumulative runs by date 
  propRun$uci.run<-with(propRun, uci.forecast*prop_UCI)
  propRun$kenai.run<-with(propRun, kenai.forecast*prop_Kenai)
  propRun$kasilof.run<-with(propRun, kasilof.forecast*prop_Kasilof)
  
  #run remaining to come
  propRun$uci.runleft<- with(propRun, uci.forecast - uci.run)
  propRun$kenai.runleft<-with(propRun, kenai.forecast - kenai.run)
  propRun$kasilof.runleft<-with(propRun, kasilof.forecast - kasilof.run)
  
  #proportion of Kenai and Kasilof runs remaining 
  propRun$kenai.propleft<-with(propRun, kenai.runleft/uci.runleft)
  propRun$kasilof.propleft<-with(propRun, kasilof.runleft/uci.runleft)
  
  myvars <- c("date", "kenai.propleft", "kasilof.propleft")
  propRun[myvars]

}