"
Canada Adjustments - last modified date: 08/27/2019
"
##################################################################################################################
##################################################### Input ################################################
##################################################################################################################
### Call the libraries 
library(RODBC)
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)


### Set file path and read input file
setwd("H:/Projects/71_ CAN adjusters/201912")
CanadaExlfile='20190829 CanadaManagement.xlsx'
uploadFile = paste('CountryAdjusterImport',format(Sys.time(), "%Y%m%d%H%M"),'VL.csv',sep='')

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
### Retail
channel<-odbcConnect("production")
DataRet<-sqlQuery(channel,"

-- Model year range (2008,2020) is decided on 5/8/2019 by Sunil T
Declare @StartDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @EndDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)
       
    select * from(            
                  SELECT 
                  [CustomerId]  
                  ,CustomerName  
                  ,[CustomerAssetId]     
                  ,[EquipmentTypeId]
                  ,[CategoryId]
                  ,[CategoryName]
                  ,[SubcategoryId]
                  ,[SubcategoryName]
                  ,[MakeId]
                  ,[MakeName]
                  ,[ModelId]
                  ,[ModelName]
                  ,[ModelYear]    
                  ,[MilesHours]
                  ,[MilesHoursCode]
                  ,[SaleDate]  
                  ,EOMONTH(SaleDate) as EffectiveDate
                  ,[SalePrice]     
                  ,[CurrentABCost]
                  ,[M1SFUsage]    
                  ,[M1AppraisalBookPublishDate]      
                  ,M1PrecedingFmv as Fmv
                  ,[M1PrecedingFmv]*(isnull(M1SFUsage,1)) as M1PrecedingFmv
                  ,[M1PrecedingFmvCanadian]*(isnull(M1SFUsage,1)) as M1PrecedingFmvCanadian  
				          ,SalePrice/([M1PrecedingFmv]*(isnull(M1SFUsage,1))) as Y  
				          ,[IsUsedForComparables]
				          ,case when CustomerId in (528,	343,	686,	546,	523,	562,	261,	337,	508,	520,	420,	519,	
                  654,	150,	699,	216,	521,	302,	95,	522,	318,	373,	515,	181,	630,	573) THEN 'CAN'
				            when [IsUsedForComparables]='Y' THEN 'USA' else 'others' END AS 'CountryCode'
                 
                  FROM [ras_sas].[BI].[Comparables]
                  WHERE SaleType ='Retail'
                  AND Modelyear between 2008 and 2020
                  AND SaleDate > @StartDate AND SaleDate<=@EndDate
                  AND CategoryId in  (30,	6,	313,	316,	15,	29,	2525,	14,	315,	360,	2509,	451,	27,	28,	23,	362,	2616,	2614,	
                  2610,	2612,	2609,	2839,	5,	2613,	2611,	35,	9)
                  AND SalePrice>10
                  AND [M1PrecedingFmv] >0
                  AND [M1PrecedingFmvCanadian] >0
                  AND M1PrecedingABCost is not NULL
                  AND (Option15 is NULL or Option15 ='0')      
                  ) A
                  where A.CountryCode <> 'others'                 
                  ")



### Auction
channel<-odbcConnect("production")
DataAuc<-sqlQuery(channel,"
                     Declare @StartDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
                     Declare @EndDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)
                     SELECT 
                     [InternetComparableId]
                     ,[EquipmentTypeId]
                     ,[CategoryId]
                     ,[CategoryName]
                     ,[SubcategoryId]
                     ,[SubcategoryName]
                     ,[MakeId]
                     ,[MakeName]
                     ,[ModelId]
                     ,[ModelName]
                     ,[ModelYear]    
                     ,[MilesHours]
                     ,[MilesHoursCode]
                     ,[SaleDate]
                     ,EOMONTH(SaleDate) as EffectiveDate
                     ,[SalePrice]
                     ,[CurrencyCode]
                     ,[SalePriceUSD]
                     ,[CurrentABCost]
                     ,[M1SFUsage]
                     ,[M1AppraisalBookPublishDate]
                     ,[LocationState]
                     ,[CountryCode]
                     ,[M1PrecedingFlv]*isnull(M1SFUsage,1) AS [M1PrecedingFlv]
                     ,[SalePriceUSD]/([M1PrecedingFlv]*isnull(M1SFUsage,1)) as Y
                    
                  
                     FROM [ras_sas].[BI].[AuctionSales]
                     WHERE CountryCode in ('CAN','USA') 
                     AND Modelyear between 2008 and 2020
                     AND SaleDate > @StartDate AND SaleDate<=@EndDate
                     AND CategoryId in  (30,	6,	313,	316,	15,	29,	2525,	14,	315,	360,	2509,	451,	27,	28,	23,	362,	2616,	2614,	
                  2610,	2612,	2609,	2839,	5,	2613,	2611,	35,	9)
                     AND [M1PrecedingFlv] >0
                     AND SalePrice>10
                      AND M1PrecedingABCost is not NULL
          
               ")


### Last Month
channel<-odbcConnect("production")
LastMonth<-sqlQuery(channel,"
      Declare @EndDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-2, -1) AS date)
      SELECT
       [CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[RetailPercent]
      ,[AuctionPercent]
      FROM [ras_sas].[BI].[AppraisalBookCountryAdjustersCategory]
      WHERE [DateAppraisalBookPublish]=@EndDate and IsGlobal=0
      ")

LastMonth_Global<-sqlQuery(channel,"
      Declare @EndDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-2, -1) AS date)
      SELECT top 1
       [CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[RetailPercent]
      ,[AuctionPercent]
      FROM [ras_sas].[BI].[AppraisalBookCountryAdjustersCategory]
      WHERE [DateAppraisalBookPublish]=@EndDate and IsGlobal=1
      ")

LM_Global <- LastMonth_Global %>% mutate(CodeCS = paste(globalId,'|')) %>% select(CodeCS,RetailPercent,AuctionPercent)

LastMonth[is.na(LastMonth)]<-''
LastMonthconcat<- rbind(LastMonth %>% mutate(CodeCS = paste(CategoryId,SubcategoryId,sep = '|')) %>% select(CodeCS,RetailPercent,AuctionPercent),LM_Global)



### Read and import input file
inputFeed <-read_excel(CanadaExlfile,sheet='In')
inputBorw <-read_excel(CanadaExlfile,sheet='InR')

#CatList<- rbind(inputFeed %>% select(CategoryId), inputBorw %>% select(CategoryId)) %>% distinct()

application<-read_excel(CanadaExlfile,sheet='Out') 
applBorw<-read_excel(CanadaExlfile,sheet='OutR')
comb_apply <- rbind(application %>% select(Schedule,CategoryId,SubcategoryId,CategoryName,SubcategoryName),
                    applBorw %>% select(Schedule,CategoryId,SubcategoryId,CategoryName,SubcategoryName))

caps_table0 <- rbind(read_excel(CanadaExlfile,sheet='Sched') %>% select(Schedule,Delta,CapsMin, CapsMax),
  read_excel(CanadaExlfile,sheet='SchedR') %>% select(Schedule,Delta,CapsMin, CapsMax))

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
CatDataRet <- merge(DataRet,retail_categ, by=c("CategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode)
SubcatDataRet<-merge(DataRet,Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode)
Retail_modDt<-rbind(CatDataRet,SubcatDataRet)

# join to auction data import
CatDataAuc <- merge(DataAuc,auction_categ, by=c("CategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode)
SubcatDataAuc<-merge(DataAuc,Subcatlevel, by=c("CategoryId","SubcategoryId")) %>% select(Schedule,CategoryId,Y,CountryCode)
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

    groupData<-subset(Retail_modDt,Retail_modDt$Schedule==retail_list[j,1])
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

  groupData_A<-subset(Auction_modDt,Auction_modDt$Schedule==auction_list[j,1])
  groupDataft <- groupData_A %>%
    filter(Y <= quantile(Y,0.75) + 2*IQR(Y) & Y>= quantile(Y,0.25) - 2*IQR(Y))
  
  fitData<-within(groupDataft,CountryCode<-relevel(CountryCode,ref="USA"))
  
  fitG<-lm(Y~CountryCode,data=fitData)
  
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
summaryAuc<-Auction_modDt %>%
  filter(CountryCode=='CAN') %>%
  group_by(Schedule) %>%
  summarise(nAuc=n())

summaryRet<-Retail_modDt %>%
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
caps_table <- rbind(caps_table0,c(global,Min_delta,lowerB,upperB))
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

### Share page
sharepage<-lastM_cap %>%
  mutate(retailDiff = retail_final-RetailPercent,
         auctionDiff = auction_final-AuctionPercent) %>%
  select(Schedule,CategoryName,SubcategoryName, RetailPercent, AuctionPercent, retail_final, auction_final,    
         retailDiff,  auctionDiff,cap_retail , cap_auction , chancheck_ret ,chancheck_auc,Retail,Auction) %>%
  arrange(Schedule,CategoryName,SubcategoryName)

### Export the files 
write.csv(ExportTb,uploadFile,row.names = F)
write.csv(sharepage,paste(Sys.Date(),'MoMSharePage_Canada.csv'))

