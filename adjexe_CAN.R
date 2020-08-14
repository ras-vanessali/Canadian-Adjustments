"
Canada Adjustments - last modified date: 02/27/2020
"
##################################################################################################################
##################################################### Input ################################################
##################################################################################################################
### Call the libraries 
library(RODBC)
library(dplyr)
library(tidyr)
library(tibble)
library(xlsx)
library(lubridate)

### Set file path and read input file
setwd("C:/Users/vanessa.li/Documents/GitHub/Canadian-Adjustments")
CanadaExlfile='20200727 CanadaManagement.xlsx'
uploadFile = paste('CountryAdjusterImport',format(Sys.time(), "%Y%m%d%H%M"),'VL.csv',sep='')
channel<-odbcConnect("production")


thresholdMonth = as.Date(Sys.Date()%m-% months(2)- days(day(Sys.Date())))
thresholddtp = 75

### Set input parameters
# caps - global
lowerB<-0.85
upperB<-1.15

# min delta - global
Min_delta = 0.10
# MoM limit
LstM_limit = 0.03

# Name global
global = 'Global'
globalId = -1
##################################################################################################################
##################################################### Data Import ################################################
##################################################################################################################
read_dataload<-parse('dataread_CAN.r')
eval(read_dataload)
### Retail
DataRet<-sqlQuery(channel,salesdt_ret)
### Auction
DataAuc<-sqlQuery(channel,salesdt_auc)


### Last Month
LastMonth<-sqlQuery(channel,lastm_sched)
LastMonth_Global<-sqlQuery(channel,lastm_glob)

LM_Global <- LastMonth_Global %>% mutate(CodeCS = paste(globalId,'|')) %>% select(CodeCS,RetailPercent,AuctionPercent)
#LastMonth[is.na(LastMonth)]<-''
LastMonthconcat<- rbind(LastMonth %>% mutate(CodeCS = paste(CategoryId,SubcategoryId,sep = '|')) %>% select(CodeCS,RetailPercent,AuctionPercent),LM_Global)

##################################################################################################################
################################################## Join to Schedule ##############################################
##################################################################################################################

### Read and import input file
inputFeed <-read.xlsx(CanadaExlfile,sheetName='In')
inputBorw <-read.xlsx(CanadaExlfile,sheetName='InR')

#CatList<- rbind(inputFeed %>% select(CategoryId), inputBorw %>% select(CategoryId)) %>% distinct()

application<-data.frame(read.xlsx(CanadaExlfile,sheetName='Out'))
applBorw<-data.frame(read.xlsx(CanadaExlfile,sheetName='OutR'))
comb_apply <- rbind(application %>% select(Schedule,CategoryId,SubcategoryId,CategoryName,SubcategoryName),
                    applBorw %>% select(Schedule,CategoryId,SubcategoryId,CategoryName,SubcategoryName))

caps_table0 <- rbind(read.xlsx(CanadaExlfile,sheetName='Sched',startRow=6) %>% select(Schedule,Delta,CapsMin, CapsMax),
                     read.xlsx(CanadaExlfile,sheetName='SchedR',startRow=6) %>% select(Schedule,Delta,CapsMin, CapsMax))

# split the input file into category level and subcategory level
Catlevel<-subset(inputFeed,inputFeed$Level2 =='Category')
Subcatlevel<-subset(inputFeed,inputFeed$Level2 =='SubCategory')

# split the borrow schedules into AbR and RbA
AbRSched <-subset(inputBorw,inputBorw$BorrowType =='AuctionBorrowRetail')
RbASched <-subset(inputBorw,inputBorw$BorrowType =='RetailBorrowAuction')

# combine regular schedules and borrow schedules by channel
retail_categ <- rbind(Catlevel %>% select(Schedule,CategoryId),RbASched %>% select(Schedule,CategoryId))
auction_categ<- rbind(Catlevel %>% select(Schedule,CategoryId),AbRSched %>% select(Schedule,CategoryId))

# join to retail data import
CatDataRet <- merge(DataRet,retail_categ, by=c("CategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode,SaleDate,EffectiveDate,ModelYear)
SubcatDataRet<-merge(DataRet,Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode,SaleDate,EffectiveDate,ModelYear)
Retail_modDt<-rbind(CatDataRet,SubcatDataRet)

# join to auction data import
CatDataAuc <- merge(DataAuc,auction_categ, by=c("CategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode,SaleDate,EffectiveDate,ModelYear)
SubcatDataAuc<-merge(DataAuc,Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode,SaleDate,EffectiveDate,ModelYear)
Auction_modDt<-rbind(CatDataAuc,SubcatDataAuc)


#######################################################################################################################
##################################################### Build Functions  ################################################
#######################################################################################################################

### build a function to do the minimum delta calculation 
minimumDelta <- function(deltaA,deltaB,move,deltaIn){
  delta = deltaA-deltaB
  result = ifelse(delta > deltaIn,(delta - deltaIn) * (move/delta),0)
  return(result)
}

### build a function to do MoM limitation 
MoMlimit <- function(last_month,current_month,limit){
  upline = last_month + limit
  btline = last_month - limit
  result = ifelse(is.na(last_month), current_month,pmin(upline,pmax(btline,current_month)))
  return(result)
}

#######################################################################################################################
###################################################### Data Cleaning ##################################################
#######################################################################################################################

### Retail
Retail_count = Retail_modDt %>%
  group_by(Schedule,CountryCode) %>%
  arrange(desc(SaleDate)) %>%
  mutate(rowNum = row_number())%>%
  group_by(Schedule,CountryCode,EffectiveDate ) %>%
  summarise(max.rowNum = max(rowNum)) %>%
  mutate(n = abs(max.rowNum-thresholddtp)) 

### find what is the month that get 100 data
month_thres.Retail<-merge(Retail_modDt %>%
                            group_by(Schedule,CountryCode) %>%
                            summarise(TotalCounts = n()),
                         Retail_count,by=c('Schedule','CountryCode')) %>%
  filter((TotalCounts>=thresholddtp & max.rowNum>=thresholddtp) | TotalCounts<thresholddtp) %>%
  group_by(Schedule,CountryCode) %>%
  filter(n==min(n)) %>%
  select(Schedule,CountryCode,EffectiveDate) %>%
  rename(Monthback = EffectiveDate)

month_thres.Retail.trans<-spread(month_thres.Retail,CountryCode, Monthback)%>%
  mutate(Monthback = pmin(CAN,USA)) %>% select(Schedule,Monthback)

## if six month has greater than 100 data, use all 6 month, if not use up to 100
ModelData_Retail = merge(Retail_modDt,month_thres.Retail.trans,by='Schedule') %>%
  filter(as.Date(EffectiveDate) >= ifelse(as.Date(Monthback) >=thresholdMonth,thresholdMonth,as.Date(Monthback))) %>%
  select(-EffectiveDate,-Monthback)


### Auction
## give the number /count how many rows does one group have
Auction_count = Auction_modDt %>%
  group_by(Schedule,CountryCode) %>%
  arrange(desc(SaleDate)) %>%
  mutate(rowNum = row_number()) %>%
  group_by(Schedule,CountryCode,EffectiveDate ) %>%
  summarise(max.rowNum = max(rowNum)) %>%
  mutate(n = abs(max.rowNum-thresholddtp)) 


### find what is the month that get 100 data
month_thres.Auction<-merge(Auction_modDt %>%
                          group_by(Schedule,CountryCode) %>%
                          summarise(TotalCounts = n()),
                        Auction_count,by=c('Schedule','CountryCode')) %>%
  filter((TotalCounts>=thresholddtp & max.rowNum>=thresholddtp) | TotalCounts<thresholddtp) %>%
  group_by(Schedule,CountryCode) %>%
  filter(n==min(n)) %>%
  select(Schedule,CountryCode,EffectiveDate) %>%
  rename(Monthback = EffectiveDate)

month_thres.Auction.trans<-spread(month_thres.Auction,CountryCode, Monthback)%>%
  mutate(Monthback = pmin(CAN,USA)) %>% select(Schedule,Monthback)

## if six month has greater than 100 data, use all 6 month, if not use up to 100
ModelData_Auction = merge(Auction_modDt,month_thres.Auction.trans,by='Schedule') %>%
  filter(as.Date(EffectiveDate) >= ifelse(as.Date(Monthback) >=thresholdMonth,thresholdMonth,as.Date(Monthback))) %>%
  select(-EffectiveDate,-Monthback)



#######################################################################################################################
##################################################### Regression Model ################################################
#######################################################################################################################

### Create list of schedules that input into regression 


retail_list<-data.frame(rbind(SubcatDataRet %>% select(Schedule),retail_categ %>% select(Schedule)) %>% distinct())

### Number of category
nCat<-dim(retail_list)[1]

"    Retail   "

### define variable
SF_R<-matrix(0,nCat)

### Run model, loop through schedules
for (j in 1:nCat){

    groupData<-subset(ModelData_Retail,ModelData_Retail$Schedule==retail_list[j,1])
    groupDataft <- groupData %>%
      filter(Y <= quantile(Y,0.75) + 2*IQR(Y) & Y>= quantile(Y,0.25) - 2*IQR(Y))
    
    fitData<-within(groupDataft,CountryCode<-relevel(CountryCode,ref="USA"))
    
    fitG<-lm(log(Y)~CountryCode,data=fitData)
    
    SF_R[j]<-exp(fitG$coefficients[2])
    
}
  

### output results
rownames(SF_R)<-retail_list[,1]
SF_R<-rownames_to_column(data.frame(SF_R))
colnames(SF_R)<-c("Schedule","Retail")


"    Auction    "
### define variable
auction_list<-data.frame(rbind(SubcatDataAuc %>% select(Schedule),auction_categ %>% select(Schedule)) %>% distinct())

### Number of category
nCat_auc<-dim(auction_list)[1]

SF_A<-matrix(0,nCat_auc)

### Run model, loop through schedules
for (j in 1:nCat_auc){

  groupData_A<-subset(ModelData_Auction,ModelData_Auction$Schedule==auction_list[j,1])
  groupDataft <- groupData_A %>%
    filter(Y <= quantile(Y,0.75) + 2*IQR(Y) & Y>= quantile(Y,0.25) - 2*IQR(Y))
  
  fitData<-within(groupDataft,CountryCode<-relevel(CountryCode,ref="USA"))
  
  fitG<-lm(log(Y)~CountryCode,data=fitData)
  
  SF_A[j]<-exp(fitG$coefficients[2])
  
  
}


### output results
rownames(SF_A)<-auction_list[,1]
SF_A<-rownames_to_column(data.frame(SF_A))
colnames(SF_A)<-c("Schedule","Auction")


###############################################################################################################################
##################################################### Scale Factor Calculation ################################################
###############################################################################################################################

### pull the number of sales in each category in CAN
summaryAuc<-ModelData_Auction %>%
  filter(CountryCode=='CAN') %>%
  group_by(Schedule) %>%
  summarise(nAuc=n())

summaryRet<-ModelData_Retail %>%
  filter(CountryCode=='CAN') %>%
  group_by(Schedule) %>%
  summarise(nRet=n())

### merge retail and auction
Mergecounts<-merge(data.frame(summaryRet),data.frame(summaryAuc),by=c("Schedule"),all = T)

MergeAdj<-merge(SF_R,SF_A,by=c("Schedule"),all = T)
CalcAdj<-merge(MergeAdj,Mergecounts,by=c("Schedule"))

### calculate the global values #######################
Global <- CalcAdj %>%
  drop_na() %>%
  summarise(RetGlob = crossprod(Retail,nRet)/sum(nRet),
            AucGlob = crossprod(Auction,nAuc)/sum(nAuc),
            CountGlobRet = sum(nRet),
            CountGlobAuc = sum(nAuc))

Global<-rownames_to_column(data.frame(Global))
Global$rowname = global
colnames(Global) <- c("Schedule","Retail","Auction", "nRet", "nAuc")

### Scale factors by schedules before any adjustments
AdjusterTB<-data.frame(as.list(rbind(Global,CalcAdj)))
#write.csv(AdjusterTB,"CANSF_Subcat0228.csv")


# assign values for borrowing schedules

join_brwIn<-merge(AdjusterTB %>% select(-nRet,-nAuc),inputBorw %>% select(Schedule,BorrowSchedule) %>% distinct(), by=c('Schedule'),all.x=T)

adj_output<-merge(join_brwIn,AdjusterTB %>% select(-nRet,-nAuc), by.y='Schedule',by.x='BorrowSchedule',all.x=T) %>%
  mutate(Retail = ifelse(is.na(Retail.x), Retail.y, Retail.x),
         Auction = ifelse(is.na(Auction.x), Auction.y, Auction.x)) %>%
  select(Schedule, Retail, Auction)


#############################################################################################################################
##################################################### Cap, Channel, MoMLimit ################################################
#############################################################################################################################
#### prepare the table with caps
globcaps<-data.frame(global,Min_delta,lowerB,upperB)
names(globcaps)<-names(caps_table0)

caps_table <- rbind(caps_table0,globcaps)
join_cap <- merge(adj_output,caps_table,by='Schedule',all.x=T)

### CAP the adjusters to lower and upper bounds 
Cap_Canada<-join_cap %>%
  mutate(cap_retail = pmin(as.numeric(CapsMax),pmax(Retail,as.numeric(CapsMin))),
         cap_auction = pmin(as.numeric(CapsMax),pmax(Auction,as.numeric(CapsMin))))

### Apply minimum delta for channel 
MinDelta<-Cap_Canada %>%
  mutate(delta_ret = cap_retail -1, delta_auc = cap_auction -1) %>%
  mutate(move_ret = minimumDelta(delta_auc,delta_ret,delta_ret,as.numeric(Delta)),
         move_auc = minimumDelta(delta_auc,delta_ret,delta_auc,as.numeric(Delta))) %>%
  mutate(chancheck_ret = cap_retail - move_ret,
         chancheck_auc = cap_auction - move_auc) %>%
  select(-delta_ret, -delta_auc,  -move_ret,-move_auc)

""""""
#MinDelta<-Cap_Canada %>%
#  mutate(chancheck_ret = ifelse(cap_auction-cap_retail > as.numeric(Delta), cap_auction-as.numeric(Delta), cap_retail),
#         chancheck_auc = cap_auction)


### Join to the application file 

mergeAppl<-merge(comb_apply,MinDelta,by=c("Schedule"),all.x=TRUE) %>%
  replace(.=='NULL','') %>%
  mutate(CodeCS = paste(CategoryId,SubcategoryId,sep = '|')) %>%
  select(Schedule,CodeCS,CategoryId,SubcategoryId,CategoryName,SubcategoryName,Retail,Auction,cap_retail, cap_auction, chancheck_ret, chancheck_auc)

### global
Globalrow<-MinDelta %>% 
  filter(Schedule==global) %>%
  mutate(CodeCS = paste(globalId,'|'),Schedule=paste('.',global),CategoryId=globalId,SubcategoryId='',CategoryName=global,SubcategoryName='') %>%
  select(Schedule,CodeCS,CategoryId,SubcategoryId,CategoryName,SubcategoryName,Retail,Auction,cap_retail, cap_auction, chancheck_ret, chancheck_auc)


### append global with others
combApplication = rbind(Globalrow,mergeAppl)

### MoM limit 
lastM_cap<-merge(combApplication,LastMonthconcat,by=c('CodeCS'),all.x=T) %>%
  mutate(retail_final = MoMlimit(RetailPercent,chancheck_ret,LstM_limit),
       auction_final =  MoMlimit(AuctionPercent,chancheck_auc,LstM_limit))

#############################################################################################################################
##################################################### Final files and export ################################################
#############################################################################################################################

### Upload file
ExportTb <-lastM_cap %>%
  rename(retail=retail_final,auction=auction_final) %>%
  select(CategoryId,SubcategoryId,retail,auction) %>%
  mutate(CountryAdjusterTypeID=1)
ExportTb[is.na(ExportTb)]<-''

### Share page
sharepage<-lastM_cap %>%
  mutate(retailDiff = retail_final-RetailPercent,
         auctionDiff = auction_final-AuctionPercent) %>%
  select(Schedule,CategoryName,SubcategoryName, RetailPercent, AuctionPercent, retail_final, auction_final,    
         retailDiff,  auctionDiff, chancheck_ret ,chancheck_auc,cap_retail , cap_auction,Retail,Auction) %>%
  arrange(Schedule,CategoryName,SubcategoryName)

sharepage2<-rbind(merge(month_thres.Retail.trans,'Retail'),merge(month_thres.Auction.trans,'Auction')) %>%
  mutate(OldestMonthInuse = if_else(as.Date(Monthback) >=thresholdMonth,thresholdMonth,as.Date(Monthback)))%>%
  select(Schedule,y,OldestMonthInuse) %>%
  rename(SaleType=y)

### Export the files 
write.csv(ExportTb,uploadFile,row.names = F)
write.xlsx2(as.data.frame(sharepage),file = paste(Sys.Date(),'MoMSharePage_Canada.xlsx'), sheetName = 'SharePage1',row.names = F)
write.xlsx2(as.data.frame(sharepage2),file = paste(Sys.Date(),'MoMSharePage_Canada.xlsx'), sheetName = 'SharePage2',append=T,row.names = F)
