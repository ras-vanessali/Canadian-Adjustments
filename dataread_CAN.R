salesdt_ret<-"
SET NOCOUNT ON
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
                  ,[CurrentABCostUSNA]
                  ,[M1SFUsage]    
                  ,[M1AppraisalBookPublishDate]      
                  ,M1PrecedingFmv as Fmv
                  ,[M1PrecedingFmv]*(isnull(M1SFUsage,1)) as M1PrecedingFmv
                  ,[M1PrecedingFmvCanadian]*(isnull(M1SFUsage,1)) as M1PrecedingFmvCanadian  
				          ,SalePrice/([M1PrecedingFmv]*(isnull(M1SFUsage,1))) as Y  
				          ,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,4))  as Age
				          ,[SalePriceSF]/CurrentABCostUSNA AS [SaleAB]
				          ,[IsUsedForComparablesUSNA]
				          ,case when CustomerId in (528,	343,	686,	546,	523,	562,	261,	337,	508,	520,	420,	519,	
                  654,	150,	699,	216,	521,	302,	95,	522,	318,	373,	515,	181,	630,	573) THEN 'CAN'
				            when [IsUsedForComparablesUSNA]='Y' THEN 'USA' else 'others' END AS 'CountryCode'
                 
                  FROM [ras_sas].[BI].[Comparables]
                  WHERE SaleType ='Retail'
                  AND Modelyear between 2008 and 2020
                  AND SaleDate > @StartDate AND SaleDate<=@EndDate
                  AND CategoryId in  (30,	6,	313,	316,	15,	29,	2525,	14,	315,	360,	2509,	451,	27,	28,	23,	362,	2616,	2614,	
                  2610,	2612,	2609,	2839,	5,	2613,	2611,	35,	9)
                  AND SalePrice>10
                  AND [M1PrecedingFmv] >0
                  AND CurrentABCostUSNA>0
                  AND (Option15 is NULL or Option15 ='0')      
                  ) A
                  where A.CountryCode <> 'others'   "

salesdt_auc<-" SET NOCOUNT ON
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
                     ,SalePriceSFUSD
                     ,[CurrentABCostUSNA]
                     ,[M1SFUsage]
                     ,[M1AppraisalBookPublishDate]
                     ,[LocationState]
                     ,[CountryCode]
                     ,[M1PrecedingFlv]*isnull(M1SFUsage,1) AS [M1PrecedingFlv]
                     ,[SalePriceUSD]/([M1PrecedingFlv]*isnull(M1SFUsage,1)) as Y
                     ,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,4))  as Age
                    ,SalePriceSFUSD/CurrentABCostUSNA as [SaleAB]
                  
                     FROM [ras_sas].[BI].[AuctionSales]
                     WHERE CountryCode in ('CAN','USA') 
                     AND Modelyear between 2008 and 2020
                     AND SaleDate > @StartDate AND SaleDate<=@EndDate
                     AND CategoryId in  (30,	6,	313,	316,	15,	29,	2525,	14,	315,	360,	2509,	451,	27,	28,	23,	362,	2616,	2614,	
                  2610,	2612,	2609,	2839,	5,	2613,	2611,	35,	9)
                     AND [M1PrecedingFlv] >0
                     AND [CurrentABCostUSNA]>0
                     AND SalePrice>10
                      AND M1PrecedingABCostUSNA is not NULL "

lastm_sched<-"
      Declare @EndDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-2, -1) AS date)
      SELECT
       [CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[RetailPercent]
      ,[AuctionPercent]
      FROM [ras_sas].[BI].[AppraisalBookCountryAdjustersCategory]
      WHERE [DateAppraisalBookPublish]=@EndDate and IsGlobal=0"


lastm_glob<-"
      Declare @EndDate Date =CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-2, -1) AS date)
      SELECT top 1
       [CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[RetailPercent]
      ,[AuctionPercent]
      FROM [ras_sas].[BI].[AppraisalBookCountryAdjustersCategory]
      WHERE [DateAppraisalBookPublish]=@EndDate and IsGlobal=1"

inprog_sched<-"SET NOCOUNT ON      
Declare @topyear INT = 2020
SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as fmv_usa
      ,[FlvSchedulePercentage]/100 as flv_usa
      ,[FmvSchedulePercentageCanadian]/100 as fmv_can
      ,[FlvSchedulePercentageCanadian]/100 as flv_can

  FROM [ras_sas].[BI].[ClassificationValuesInProgressUSNA]
  Where ModelYear>=@topyear-9 and ModelYear<=@topyear
  and MakeId is null 
  and ((SubcategoryId is null) or ([SubcategoryName] not like 'DO NOT USE%'))
  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)"
          
prior_sched<-"SET NOCOUNT ON      
Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)
Declare @topyear INT = 2020
SELECT [ClassificationId]
,[CategoryId]
,[CategoryName]
,[SubcategoryId]
,[SubcategoryName]
,[AppraisalBookPublishDate]
,[ModelYear]
,[FMVSchedulePercentageCanadian]/100 as priorfmv
,[FLVSchedulePercentageCanadian]/100 as priorflv
FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
WHERE [AppraisalBookPublishDate] = @EffectiveDate
AND (NOT(Categoryid IN (220,1948,21) OR CategoryName LIKE 'DO NOT USE%'))
AND MakeId is null 
AND ModelYear>=@topyear-9 and ModelYear<=@topyear"
