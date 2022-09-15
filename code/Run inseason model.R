obs.cpue<-read.table("obsCPUE.csv", sep = ",", header=T)#input observed daily cpue from test fishery; inseason OTF data
acum.run<-read.table("acumulative run.csv", sep = ",", header=T) #input inseason run data
#acum.run$accumRun<-acum.run$accumRun1 #use acumulative Run from TR2020 worksheet
acum.run$accumRun<-acum.run$accumRun2 #use acumulative Run including residual drift fisheries
acum.run$date<-as.Date(acum.run$date,format="%d-%b")#convert character to date
coefs<-read.table("NLINCOEF.csv", sep = ",", header=T)#input historic run curve a, b parameters 

#### initial a dataframe to save inseason estimates #############################
out.all=matrix(NA, nrow=1, ncol=10) #inital a data frame from a matrix
colnames(out.all)=c("year", "mse", "date","ccumCPUE.x","cum_RUN","ccpuef","pr","mid.run","est.total.run","remaining.run")
out.all=as.data.frame(out.all)
###############################################################################

source("inseason function.R") #Read the inseason model function from a file
inrun<-acum.run[!is.na(acum.run$accumRun),] #remove missing values in column accumRun
inrun$date<-format(as.Date(inrun$date), "%m/%d/%Y") #change date format in order to be consistant with date format in inseaon function
pred.date<-inrun$date  
 
for (mydate in pred.date){
  out.D2sub<-inseason(mydate) #call inseason estimate function above
  out.all<-rbind(out.all, out.D2sub) #append new results on the dataframe
}
out.all <- na.omit(out.all) #removing the NA row
write.csv(out.all,'est totalrun.csv')

#set on-time date run is 7/17.
#This was estimated from realtionship between run-timing and SST. Contact Bob DeCino for more info 
ontime<-"7/17" 
ontime<-as.Date(ontime,format="%m/%d")
out.all$mid.run<-as.Date(out.all$mid.run,format="%m/%d")
out.all$ontime<-ontime
out.all$timing<-out.all$mid.run - out.all$ontime
################ End of UCI inseason estimates of UCI run size #################

### Plot a figure of inseason esitmates of UCI run size ###################
library(tidyverse)
library(ggrepel)
run<-out.all #read inseason estimates of UCI run sizes 
run$date<-as.Date(run$date, format="%m/%d")
myplot<-ggplot(run, aes(x= date, y = est.total.run)) + 
  geom_point(color = "blue", size = 2) + 
  ggtitle("2022 UCI Sockeye Inseason Estimates Using 5 Best Running Curves") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text_repel(aes(label = year),
                   size = 2,   # font size in the text labels
                   box.padding   = 0.05, 
                   point.padding = 0.05,
                   max.overlaps = getOption("ggrepel.max.overlaps", default = 20),
                   segment.color = 'grey50') +
  scale_x_date(name="Date", date_breaks = "2 day", date_labels = "%b%d") +
 scale_y_continuous("Est. Total Run", labels = scales::comma)

myplot + theme(axis.text.y = element_text(size = 10))+ # change y-axis text size
  theme(axis.title.x = element_text(size = 10))+     # change x-axis text size 
  theme(plot.title = element_text(size = 10)) # change plot title text size
##### Finish the plot of UCI inseason estiamtes of run size #####################

##### Projecting Kenai River sockeye total run ####################################
#Read daily Kenai Run data to estimate Kenai Run
source("propRunleft function.R") #read the function that estimates proportion of remaining runs of Kenai and kasilof
prop.run<-propRunleft()
#input Kenai sockeye accumulative run(inseason data)
counts.ke<-read.table("acumulative run_ke.csv", sep = ",", header=T)
counts.ke$date<-as.Date(counts.ke$date,format="%d-%b")#convert character to date
prop.run$date<-as.Date(prop.run$date,format="%m/%d")
out.all$date<-as.Date(out.all$date,format="%m/%d")
# merge two data frames by date
total <- merge(out.all, prop.run,by.x= "date", by.y = "date" )
total<- merge(total, counts.ke, by.x= "date", by.y = "date" )
total$remaining.ke <- with(total, kenai.propleft * remaining.run) #remaining Kenai run will arrive. remaining.run is UCI sockeye as whole 
total$remaining.ka <- with(total, kasilof.propleft * remaining.run) #remaining Kasilof run to go 
total$run.ke<-with(total, acumRun_ke + remaining.ke )
write.csv(total,'withkenai.csv') # output a data file of total run, together with Kenai sockeye run

######## Plot Kenai River sockeye estimates of run size ##################
library(tidyverse)
library(ggrepel)
#run<-read.table("withKenai.csv", sep = ",", header=T)#input data file with Kenai estimates
run<-total #input data file with Kenai estimates
run$date<-as.Date(run$date) #convert charatcter like "2022-07-01" to date
#ggplot: model fitted vs. actual run
myplot<-ggplot(run, aes(x= date, y = run.ke)) + 
  geom_point(color = "blue", size = 2) + 
  ggtitle("2022 Kenai Sockeye Inseason Estimates Using 5 Best Running Curves") +
  theme(plot.title = element_text(hjust = 0.5))+
### geom_label_repel or geom_text_repel
  geom_text_repel(aes(label = year),
                   size = 3,   # font size in the text labels
                   box.padding   = 0.05, 
                   point.padding = 0.05,
                   max.overlaps = getOption("ggrepel.max.overlaps", default = 20),
                   segment.color = 'grey50') +
  scale_x_date("Date", date_breaks = "2 day", date_labels = "%b%d") +
 scale_y_continuous("Estimated Run", labels = scales::comma)
myplot 
