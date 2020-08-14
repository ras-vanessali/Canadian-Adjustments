#1) Run Schedule 
#2) Run Canada
#3) 

library(RODBC)
library(lattice)
library(latticeExtra)
library(dplyr)
library(xlsx)
library(lubridate)
library(stringr)
library(glue)


CanadaExlfile='20200727 CanadaManagement.xlsx'
mngfile_us = '20200717 SchedulesManagement.xlsx'

channel<-odbcConnect("production")
file_path<-"C:/Users/vanessa.li/Documents/GitHub/Canadian-Adjustments"
setwd(file_path)
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)
#read_dataload<-parse('dataread_CAN.r')
#eval(read_dataload)
publishDate <- Sys.Date() - days(day(Sys.Date()))
thirdLastM<-as.Date(publishDate%m-% months(1)- days(day(publishDate)))

starttime_dtload<-Sys.time()
#Canada inprogress schedule (current month)
CanSchedLoad<-sqlQuery(channel,inprog_sched)
CanSchedprior<-sqlQuery(channel,prior_sched)

### Retail
DataRet<-sqlQuery(channel,salesdt_ret)
### Auction
DataAuc<-sqlQuery(channel,salesdt_auc)
end_time_dtload <- Sys.time()
end_time_dtload - starttime_dtload


## read input files

In<-data.frame(read.xlsx(mngfile_us,sheetName='In')) %>% select(Schedule, Level2, CategoryId, SubcategoryId, CanadaPlots)
In_plots<-In %>% filter(CanadaPlots=='Y')


inputFeed <-rbind(read.xlsx(CanadaExlfile,sheetName='In') %>% select(Schedule,Level2,CategoryId, SubcategoryId),
                 read.xlsx(CanadaExlfile,sheetName='InR') %>% select(Schedule,Level2,CategoryId, SubcategoryId)) %>%
  filter(!str_detect(Schedule,'For Caps')) %>%
  rename(Sched.can = Schedule) 


## map the US sched mgmt file to Canada file to get USsched.name - CanadaAdj.name
Catlevel<-merge(subset(In_plots,In_plots$Level2 =='Category') %>% select(Schedule,CategoryId),
                subset(inputFeed,inputFeed$Level2 =='Category') %>% select(Sched.can,CategoryId),by='CategoryId',all.x=T)
Subcatlevel<-merge(subset(In_plots,In_plots$Level2 =='SubcatGroup') %>% select(Schedule,CategoryId,SubcategoryId),
                   subset(inputFeed,inputFeed$Level2 =='SubCategory') %>% select(Sched.can,CategoryId,SubcategoryId),by=c('CategoryId','SubcategoryId'),all.x=T)



## assign in progress Canada schedule with schedule name
CatSched<-CanSchedLoad %>% filter(is.na(SubcategoryId))
SubcatSched<-CanSchedLoad %>% filter(!is.na(SubcategoryId))


CatschedJoin <- merge(CatSched,Catlevel, by=c("CategoryId")) %>% select(Schedule,Sched.can,ModelYear,fmv_usa,flv_usa,fmv_can,flv_can)
SubcatschedJoin<-merge(SubcatSched,Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,Sched.can,ModelYear,fmv_usa,flv_usa,fmv_can,flv_can) %>% distinct()

inprogScheds<- rbind(CatschedJoin,SubcatschedJoin) %>% arrange(Schedule,desc(ModelYear)) %>%
  mutate(Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00) 

## assign prior Canada schedule with schedule name
CatSched.prior<-CanSchedprior %>% filter(is.na(SubcategoryId))
SubcatSched.prior<-CanSchedprior %>% filter(!is.na(SubcategoryId))


CatschedJoin.prior <- merge(CatSched.prior,Catlevel, by=c("CategoryId")) %>% select(Schedule,Sched.can,ModelYear,priorflv,priorfmv)
SubcatschedJoin.prior<-merge(SubcatSched.prior,Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,Sched.can,ModelYear,priorflv,priorfmv) %>% distinct()

priormonth = as.Date(publishDate- days(day(publishDate)))
priorScheds<- rbind(CatschedJoin.prior,SubcatschedJoin.prior) %>% arrange(Schedule,desc(ModelYear)) %>%
  mutate(Age= year(priormonth)-ModelYear + (month(priormonth)-6)/12.00) 


# data validation
valid.subcat <- Subcatlevel %>% select(Schedule,Sched.can) %>% distinct()
if(dim(Catlevel)[1]*10 == dim(CatschedJoin)[1] & dim(valid.subcat)[1]*10 == dim(SubcatschedJoin)[1]){
  t = dim(Catlevel)[1]+dim(valid.subcat)[1]
  message(sprintf("We have %s Canada schedules to print.",t))
  plots_schedlist<- rbind(Catlevel %>% select(Schedule,Sched.can),valid.subcat %>% select(Schedule,Sched.can))
} else{message("The numbers of schedules to print is not correct")}


## join sales data to get schedule name
# join to retail data import
CatDataRet <- merge(DataRet %>% filter(CountryCode=='CAN'),Catlevel, by=c("CategoryId")) %>% select(Schedule,Sched.can,SaleAB,Age,EffectiveDate,ModelYear)
SubcatDataRet<-merge(DataRet %>% filter(CountryCode=='CAN'),Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,Sched.can,SaleAB,Age,EffectiveDate,ModelYear)
Retail_Dt<-rbind(CatDataRet,SubcatDataRet)
recentSales_ret<-Retail_Dt %>% filter(as.Date(EffectiveDate)>=thirdLastM)
olderSales_ret<-Retail_Dt %>% filter(as.Date(EffectiveDate)<thirdLastM)

# join to auction data import
CatDataAuc <- merge(DataAuc %>% filter(CountryCode=='CAN'),Catlevel, by=c("CategoryId")) %>% select(Schedule,Sched.can,SaleAB,Age,EffectiveDate,ModelYear)
SubcatDataAuc<-merge(DataAuc %>% filter(CountryCode=='CAN'),Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,Sched.can,SaleAB,Age,EffectiveDate,ModelYear)
Auction_Dt<-rbind(CatDataAuc,SubcatDataAuc)
recentSales_auc<-Auction_Dt %>% filter(as.Date(EffectiveDate)>=thirdLastM)
olderSales_auc<-Auction_Dt %>% filter(as.Date(EffectiveDate)<thirdLastM)


############################### plots #############################
for (i in 1:t){

  plot_name = paste(plots_schedlist[i,2])
  # sales data
  auc_data = Auction_modDt %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  ret_data = Retail_modDt %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  
  # prior schedules
  lstm_scheds = priorScheds %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  
  # inprogress schedules
  new_scheds = inprogScheds %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  
  # dots
  older_auc<-olderSales_auc %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  older_ret<-olderSales_ret %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  newer_auc<-recentSales_auc %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  newer_ret<-recentSales_ret %>% filter(Schedule == as.character(plots_schedlist[i,1]))
  
  xaxis = c(-.5,11)
  yaxis = c(0,2)
  yaxis_name='SP /AB Cost'
  
  
  ######## Draw the dots #######
  drawolder_Auc<-xyplot(SaleAB ~ Age, older_auc,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="deepskyblue1", main=list(label=paste(plot_name,' - ' ,publishDate),font=2,cex=2))
  drawolder_Ret<-xyplot(SaleAB ~ Age, older_ret,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="lightpink")
  drawrecent_Auc<-xyplot(SaleAB ~ Age, newer_auc,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="dodgerblue3")
  drawrecent_Ret<-xyplot(SaleAB ~ Age, newer_ret,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="firebrick2")
  
  ######## Draw the lines #######
  # prior
  drawprior_Auc<-xyplot(priorflv ~ Age, lstm_scheds,ylim=yaxis,xlim=xaxis,type=c('p','l'),lty=3,col='dodgerblue3',ylab = yaxis_name,lwd=3,pch=4,cex=1.5)
  drawprior_Ret<-xyplot(priorfmv ~ Age, lstm_scheds,ylim=yaxis,xlim=xaxis,type=c('p','l'),lty=3,col='firebrick2',ylab = yaxis_name,lwd=3,pch=4,cex=1.5)
  
  # US
  drawusa_Auc<-xyplot(flv_usa ~ Age, new_scheds,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=12))
                           ,type=c("l","g"),col='dodgerblue3',lwd=1.5,lty=1,cex=1,ylab = yaxis_name)
  drawusa_Ret<-xyplot(fmv_usa ~ Age, new_scheds,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=12))
                           ,type=c("l","g"),col='firebrick2',lwd=1.5,lty=1,cex=1,ylab = yaxis_name)
  # CAN
  drawcan_Auc<-xyplot(flv_can ~ Age, new_scheds,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=12))
                         ,type=c('p','l','g'),col='dodgerblue3',lwd=3,pch=4,cex=1.5, lty=1
                         ,ylab = yaxis_name
                         ,panel = function(x,y,...){
                           panel.abline(v = 0:14, h = seq(0,2,.5), col="lightgrey")
                           panel.xyplot(x, y, ...)
                         })
  drawcan_Ret<-xyplot(fmv_can ~ Age, new_scheds,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=12))
                         ,type=c('p','l','g'),col='firebrick2',lwd=3,pch=4,cex=1.5,lty=1,ylab = yaxis_name)
  
  draw<- drawolder_Auc + as.layer(drawrecent_Auc) + as.layer(drawolder_Ret) + as.layer(drawrecent_Ret) + as.layer(drawusa_Auc) + as.layer(drawusa_Ret) + as.layer(drawprior_Auc) + as.layer(drawprior_Ret) + as.layer(drawcan_Ret) + as.layer(drawcan_Auc)
  
  mypath<-file.path(file_path,plotFolder,paste(plot_name,".png"))
  png(file=mypath,width=1600,height=1200)
  print(draw)
  dev.off()
  }






