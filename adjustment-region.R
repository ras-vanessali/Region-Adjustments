########################## Package Installation ########################

library(RODBC)
library(dplyr)
library(tibble)
library(tidyr)
library(readxl)

########################### * Define some variables used in following code ###########################################
mutingFactor<-2/3
adjCap_USA<-0.075
adjCap_CAN<-0.050
adjCap_HIAK<-0.03
globalId<-"0"
MarketCode = 'USNA'
HI_AK_fact = 1.05

MoM_limit = 0.03

setwd("C:/Users/vanessa.li/Documents/GitHub/Region-Adjustments")
Regionfile = '20190911RegionManagement.xlsx'

loadFile<-paste('RegionAdjusters',format(Sys.time(), "%Y%m%d%H%M"),'VL.csv',sep='')

#connect<-'rasgcp'
connect<-'production'
###################################################################################################################
###################################################################################################################
##################################### INPUT DATA FROM SQL DB /INTAKE FLAT FILE ####################################
###################################################################################################################
###################################################################################################################
channel<-odbcConnect(connect)
USAregion<-sqlQuery(channel,"

Declare @SD_Start Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @SD_End Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)

SELECT [CategoryId]
			,[CategoryName] 
			,SubcategoryId
			,[SubcategoryName]
			,MakeId
			,[MakeName]
			,ModelId
			,[ModelName]
			,[ModelYear]
			,[MilesHours]
			,M1PrecedingFlv
			,[SalePriceSF]
			,SaleMonth
			--,CASE when [LocationState] in ('CA','NV','AZ') Then 'W' --3
			--		   When [LocationState] in ('NM','TX','LA','AR','OK') Then 'SW' --5
			--		   When [LocationState] in ('WA','OR','ID','UT','MT','WY','CO','ND') Then 'NW' --8
			--		   When [LocationState] in ('MN','SD','NE','KS','IA','MO','IL','WI','MI','IN','OH','KY','WV') Then 'MW' --13
			--		   When [LocationState] in ('PA','NY','VT','ME','NH','MA','CT','RI','NJ','MD','DC','DE') Then 'NE'-- 12
			--		   When [LocationState] in ('MS','TN','AL','GA','SC','VA','NC','FL') Then 'SE' --8
			--		   ELSE 'NOT OK' END AS [Region] 
			,RMIP.[Region]
			,BIC.LocationState
			,Year([M1AppraisalBookPublishDate]) - ModelYear + (Month([M1AppraisalBookPublishDate])-6)/12.0000 AS Age
  
			  ,[SalePriceSF]/M1PrecedingFLV as Y
		  FROM [ras_sas].[BI].[Comparables] BIC
		  inner join [ras_sas].[BI].[RegionMappingInProgressMKT] RMIP
		  on BIC.LocationState = RMIP.[StateProvince]
		  where [Source]='Internet' and RMIP.MarketCode ='USNA'
			 --And [LocationState] in ('MN','SD','NE','KS','IA','MO','IL','WI','MI','IN','OH','KY','WV','CA','NV','AZ',
			--		   'WA','OR','ID','UT','MT','WY','CO','ND','PA','NY','VT','ME','NH','MA','CT',
			--		   'MS','TN','AL','GA','SC','VA','NC','FL','RI','NJ','MD','DC','DE','NM','TX','LA','AR','OK')               
			 AND Categoryid in (6,	14,	15,	29,	313,	315,	316,	360,	362,	451,	453,	2509,	2515,	2614,	2616)
			  AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed
              AND RMIP.[CountryCode]='USA'
			 And [SalePrice]>3  And [M1PrecedingFLV]>3 and [SaleDate]>@SD_Start and [SaleDate]<=@SD_End
			 AND Modelyear between 2008 and 2020           
			 AND [SalePriceSF]/M1PrecedingFLV>0.5 AND [SalePriceSF]/M1PrecedingFLV<2      
			 AND M1PrecedingABCostUSNA is not NULL
       AND Option15 is NULL      
   
               ")


channel<-odbcConnect("production")
CANregion<-sqlQuery(channel,"
SET NOCOUNT ON
SET ANSI_WARNINGS OFF

Declare @StartDate DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @EndDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)

Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)

DROP TABLE IF EXISTS #Dataset
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
    ,[SalePrice]
    ,[CurrencyCode]
    ,[SalePriceUSD]
	  ,SalePriceSF
    ,[CurrentABCostUSNA]
    ,[M1SFUsage]
    ,[M1AppraisalBookPublishDate]
    ,[LocationState]
    ,BIAS.[CountryCode]
    ,[M1PrecedingFlv]
  	,M1PrecedingABCostUSNA
   ,Year([M1AppraisalBookPublishDate]) - ModelYear + (Month([M1AppraisalBookPublishDate])-6)/12.0000 AS Age
    
	INTO #Dataset
	FROM [ras_sas].[BI].[AuctionSales] BIAS
	
    WHERE BIAS.CountryCode = 'CAN' 
    AND Modelyear between 2008 and 2020
    AND SaleDate > @StartDate AND SaleDate<= @EndDate
    AND CategoryId in (6,	15,	29,	315,451,362,360,2509,313,316)
    AND [M1PrecedingFlv] >3
    AND SalePrice>3			
    AND M1PrecedingABCostUSNA is not NULL
					
DROP TABLE IF EXISTS #ETV					
 SELECT [CategoryId]
 ,[SubcategoryId]
 ,MakeId
 ,[ModelId]
,[AppraisalBookPublishDate]
,[ModelYear]
,[FLVSchedulePercentageCanadian]
INTO #ETV
FROM [ras_sas].[BI].[AppraisalBookEquipmentTypeValuesUSNA] 
where Modelyear between 2008 and 2020
AND [AppraisalBookPublishDate] between @StartDate and @EffectiveDate
AND CategoryId in (6,	15,	29,	315,451,362,360,2509,313,316) and  Makeid is not null



SELECT salesTb.*, [FLVSchedulePercentageCanadian],(SalePriceUSD/ISNULL(M1SFUsage,1))/([FLVSchedulePercentageCanadian]*M1PrecedingABCostUSNA/100) as Y
FROM (select #Dataset.*, RMIP.Region from #Dataset
inner join [ras_sas].[BI].[RegionMappingInProgressMKT] RMIP
		  on #Dataset.LocationState = RMIP.[StateProvince]) salesTb			
left join #ETV 
ON #ETV.[SubcategoryId]=salesTb.[SubcategoryId] AND #ETV.ModelId=salesTb.ModelId AND #ETV.Modelyear=salesTb.Modelyear and #ETV.[AppraisalBookPublishDate] = salesTb.[M1AppraisalBookPublishDate]

 ")



############################################################### Import Last Month adjustments #####################################################################
channel<-odbcConnect("production")
lastMonthAdj<-sqlQuery(channel,"
SET NOCOUNT ON
SELECT [CountryCode]
      ,[Region]
      ,isNull([CategoryID],'') as CategoryId 
      ,[CategoryName] as Category
      ,[SubcategoryId] as SubcategoryId
      ,isNull([SubcategoryName],'') as Subcategory
      ,[RegionAdjustment]
      ,[AppraisalBookIssueID]
      ,[AppraisalBookPublishDate]
  FROM [ras_sas].[BI].[AppraisalBookRegionAdjustmentsMKT]
  where [AppraisalBookIssueID]=(Select max([AppraisalBookIssueID]) FROM [ras_sas].[BI].[AppraisalBookRegionAdjustmentsMKT])")



transLastM<-spread(as.data.frame(lastMonthAdj),Region,RegionAdjustment) %>%
  mutate(SubcategoryId = ifelse(is.na(SubcategoryId),'',SubcategoryId)) %>%
  mutate(CodeCS = paste(CountryCode, CategoryId,SubcategoryId,sep = '|')) %>%
  select(CountryCode,CodeCS,E,AK,GL,HI,HL,NE,NW,SE,SW,W) %>%
  rename(lmE=E, lmAK=AK, lmHI=HI,lmGL=GL,lmHL=HL,lmNE=NE, lmNW=NW, lmSE=SE, lmSW=SW, lmW=W)




#######################################################################################################################
##################################################### Build Functions  ################################################
#######################################################################################################################

### build a function to do MoM limitation

MoMlimit_region <- function(last_month,current_month,limit){
  upline = last_month + limit
  btline = last_month - limit
  result = ifelse(is.na(last_month),current_month,pmin(upline,pmax(btline,current_month)))
  return(result)
}



############################################################### Making Adjustment #####################################################################

##################################################### USA ############################################################

### read the input file
inputFeed_USA <- read_excel(Regionfile,sheet='InA')

### join to the data 
Catlevel_Cat<-subset(inputFeed_USA,inputFeed_USA$Level2 =='Category' & inputFeed_USA$SubcategoryId == "NULL")
Catlevel_subcat<-subset(inputFeed_USA,inputFeed_USA$Level2 =='Category' & inputFeed_USA$SubcategoryId != "NULL")
Subcatlevel<-subset(inputFeed_USA,inputFeed_USA$Level2 =='SubcatGroup')

CatCatData <- merge(USAregion,Catlevel_Cat, by=c("CategoryId"),all.x=T) %>% select(ClassificationId,Schedule,MonthsOfData,CategoryName.x, SubcategoryName.x,Y,Region)
CatSubcatData <- merge(USAregion,Catlevel_subcat,  by=c("CategoryId","SubcategoryId"),all.x=T) %>% select(ClassificationId,Schedule,MonthsOfData,CategoryName.x, SubcategoryName.x,Y,Region)
SubcatData<-merge(USAregion,Subcatlevel, by=c("CategoryId","SubcategoryId"),all.x=T) %>% select(ClassificationId,Schedule,MonthsOfData,CategoryName.x, SubcategoryName.x,Y,Region)




GeoUSA<-c('GL','HL', 'NE', 'NW', 'SE', 'SW', 'W')

Data_model_USA <-rbind(CatCatData,CatSubcatData,SubcatData) %>%
  filter(Region %in% GeoUSA)

# Schedule List
summaryUSA<-inputFeed_USA %>%
  filter(Schedule !="") %>%
  group_by(Schedule) %>%
  summarise(n=n()) 
SchedList_USA<-data.frame(summaryUSA[,1])

n<-dim(SchedList_USA)[1]
k<-length(GeoUSA)

outputUSA<-matrix(NA,n,k)
volumeUSA<-matrix(NA,n,k)
SF_USA<-matrix(NA,n,k)

for(i in 1:n){ ## Go through by Schedule
 
  BySchedule<-Data_model_USA %>% filter(Schedule==SchedList_USA[i,1])
  
  fit<-lm(log(Y)~Region-1,data=BySchedule)
  
  for (j in 1:k){
    
    outputUSA[i,j]<-exp(coef(fit)[j])             ## exponent of the coefficient of each region
    
    ByReg<-subset(BySchedule,BySchedule$Region==GeoUSA[j])
    volumeUSA[i,j]<-length(ByReg$Region)
    
  }
  
}


## calculation
multiply_USA<-outputUSA*volumeUSA
avg_USA<-rowSums(multiply_USA)/rowSums(volumeUSA)

for (x in 1:n){
  SF_USA[x,]<-outputUSA[x,]/avg_USA[x]
  
}

SFregion_USA<-SF_USA*mutingFactor+(1-mutingFactor)
rownames(SFregion_USA)<-SchedList_USA[,1]
colnames(SFregion_USA)<-GeoUSA
rownames(volumeUSA)<-SchedList_USA[,1]
colnames(volumeUSA)<-GeoUSA


### Caps

for (j in 1:dim(SFregion_USA)[1]){
  for (i in 1:dim(SFregion_USA)[2]){
    SFregion_USA[j,i] <- min(max(SFregion_USA[j,i],1-adjCap_USA),1+adjCap_USA)
  }
}


#### format the output
output_USA<-rownames_to_column(data.frame(SFregion_USA)) %>%
  rename(Schedule=rowname)
counts_USA<-rownames_to_column(data.frame(volumeUSA)) %>%
  rename(Schedule=rowname)


#### compute the global adjustment by using the input schedule (weighted average)
Global_USA <- apply(output_USA[-1]*counts_USA[-1],2,sum)/apply(counts_USA[-1],2,sum)

matrix_global<-matrix(Global_USA,nrow=1,ncol=7)
rownames(matrix_global) <-"Global"
colnames(matrix_global) <-GeoUSA

GlobalUSA<-rownames_to_column(data.frame(matrix_global)) %>%
  rename(CategoryName=rowname) %>%
  mutate(Schedule='Global',CountryCode="USA",E='',CategoryId=globalId,SubcategoryId="",SubcategoryName="") %>%
  select(Schedule,CategoryId,CategoryName,SubcategoryId,SubcategoryName,CountryCode,NW,NE,GL,HL,SE,SW,W,E) %>%
  rename(Category = CategoryName,Subcategory=SubcategoryName)



Application_USA<-read_excel(Regionfile,sheet='OutA') %>%
  filter(Schedule !="") %>%
  select(Schedule,CategoryId,	CategoryName,SubcategoryId,SubcategoryName)

mergeTb_USA<-merge(Application_USA,output_USA,by=c("Schedule"),all.x=T) %>%
  mutate(CountryCode="USA",E='') %>%
  select(Schedule,CategoryId,CategoryName,SubcategoryId,SubcategoryName,CountryCode,NW,NE,GL,HL,SE,SW,W,E) %>%
  rename(Category = CategoryName,Subcategory=SubcategoryName)


### union the USA final input schedule table 
ExportTb_USA<- rbind(GlobalUSA,mergeTb_USA) 

##################################################### CAN ############################################################

############### Read input mapping table #################
inputFeed_CAN <- read_excel(Regionfile,sheet='InC')


Data_model_CAN <- merge(CANregion,inputFeed_CAN, by=c("CategoryId"),all.x=T) %>%
  filter(Region != 'NOT OK')


Data_exc_outlier<-Data_model_CAN %>%
  group_by(Schedule) %>%
  filter(!is.na(Y)) %>%
  filter(Y <= quantile(Y,0.75) + 2*IQR(Y) & Y>= quantile(Y,0.25) - 2*IQR(Y))



Geo_CAN<-c("E","W")


# Schedule List
summary_CAN<-inputFeed_CAN %>%
  filter(Schedule !="") %>%
  group_by(Schedule) %>%
  summarise(n=n()) 
SchedList_CAN<-data.frame(summary_CAN[,1])

m<-dim(SchedList_CAN)[1]
h<-length(Geo_CAN)

output_CAN<-matrix(NA,m,h)
volume_CAN<-matrix(NA,m,h)
SF_CAN<-matrix(NA,m,h)

for(i in 1:m){ ## Go through by Schedule
  BySchedule<-Data_exc_outlier[Data_exc_outlier$Schedule==SchedList_CAN[i,1],]
  
  fit<-lm(log(Y)~Region-1,data=BySchedule)
  
  for (j in 1:h){
    output_CAN[i,j]<-exp(coef(fit)[j])             ## exponent of the coefficient of each region
    
    ByReg<-subset(BySchedule,BySchedule$Region==Geo_CAN[j])
    volume_CAN[i,j]<-length(ByReg$Region)
    
  }
  
}


## calculation
multiply_CAN<-output_CAN*volume_CAN
avg_CAN<-rowSums(multiply_CAN)/rowSums(volume_CAN)


for (x in 1:m){
  SF_CAN[x,]<-output_CAN[x,]/avg_CAN[x]
  
}

SFregion_CAN<-SF_CAN*mutingFactor+(1-mutingFactor)
rownames(SFregion_CAN)<-SchedList_CAN[,1]
colnames(SFregion_CAN)<-Geo_CAN
rownames(volume_CAN)<-SchedList_CAN[,1]
colnames(volume_CAN)<-Geo_CAN


### Caps

for (j in 1:dim(SFregion_CAN)[1]){
  for (i in 1:dim(SFregion_CAN)[2]){
    SFregion_CAN[j,i] <- min(max(SFregion_CAN[j,i],1-adjCap_CAN),1+adjCap_CAN)
  }
}

output_CAN<-rownames_to_column(data.frame(SFregion_CAN)) %>%
  rename(Schedule=rowname)



#### compute the global adjustment by using the input schedule (weighted average)
Global_CAN <- apply(output_CAN[2:3]*output_CAN[2:3],2,sum)/apply(output_CAN[2:3],2,sum)


matrix_global_CAN<-matrix(Global_CAN,nrow=1,ncol=2)
rownames(matrix_global_CAN) <-"Global"
colnames(matrix_global_CAN) <-Geo_CAN

GlobalCAN<-rownames_to_column(data.frame(matrix_global_CAN)) %>%
  rename(CategoryName=rowname) %>%
  mutate(Schedule='Global',CountryCode="CAN",GL="",HL="",NE="",NW="",SE="",SW="",CategoryId="0",SubcategoryId="",SubcategoryName="") %>%
  select(Schedule,CategoryId,CategoryName,SubcategoryId,SubcategoryName,CountryCode,NW,NE,GL,HL,SE,SW,W,E) %>%
  rename(Category = CategoryName,Subcategory=SubcategoryName)




Application_CAN<-read_excel(Regionfile,sheet='OutC') %>%
  filter(Schedule !="") %>%
  select(Schedule,CategoryId,	CategoryName,SubcategoryId,SubcategoryName)

mergeTb_CAN<-merge(Application_CAN,output_CAN,by=c("Schedule"),all.x=T) %>%
  mutate(CountryCode="CAN",
         GL="",HL="",NE="",NW="",SE="",SW="") %>%
  select(Schedule,CategoryId,CategoryName,SubcategoryId,SubcategoryName,CountryCode,NW,NE,GL,HL,SE,SW,W,E) %>%
  rename(Category = CategoryName,Subcategory=SubcategoryName)


### union the USA final input schedule table 
ExportTb_CAN<-rbind(GlobalCAN,mergeTb_CAN)


################ factor and caps on HI and AK
AK_HI<-USAregion %>% filter(Region %in% c('HI','AK')) %>%
  group_by(Region) %>%
  summarise(n=n(),
            avg = median(Y)) %>%
  mutate(adjustAvg = avg * (1-mutingFactor) + mutingFactor) %>%
  mutate(cap = pmin(pmax(adjustAvg,1-adjCap_HIAK),1+adjCap_HIAK))

AK_HIdf<-data.frame(AK_HI)
AK_Adj <- AK_HIdf[AK_HIdf$Region=='AK',]$cap
HI_Adj <- AK_HIdf[AK_HIdf$Region=='HI',]$cap
############################################################## Output #####################################################################
## combine the USA and CAN values
combindTB<-rbind(ExportTb_USA,ExportTb_CAN) %>%
  mutate(AK = ifelse(CountryCode =='USA', as.numeric(NW) * HI_AK_fact * AK_Adj, ''),
         HI = ifelse(CountryCode =='USA',as.numeric(W) * HI_AK_fact * HI_Adj, '')) %>%
  replace(.=="NULL","") %>%
  mutate(CodeCS = paste(CountryCode, CategoryId,SubcategoryId,sep = '|')) 




## join the current month results to last month adjustments, and cap the movement from month to month

## this table compares last month to this month, share
Join_LM<-merge(combindTB,transLastM,by=c('CountryCode','CodeCS'),all.x=T) 

## limit the movement from last month and explore the final values to upload
MoMlimst <- Join_LM %>%
  mutate(capW = MoMlimit_region(lmW,W,MoM_limit),
         capE = MoMlimit_region(lmE,E,MoM_limit),
         capGL = MoMlimit_region(lmGL,GL,MoM_limit),
         capHL = MoMlimit_region(lmHL,HL,MoM_limit),
         capNE = MoMlimit_region(lmNE,NE,MoM_limit),
         capNW = MoMlimit_region(lmNW,NW,MoM_limit),
         capSE = MoMlimit_region(lmSE,SE,MoM_limit),
         capSW = MoMlimit_region(lmSW,SW,MoM_limit),
         capHI = MoMlimit_region(lmHI,HI,MoM_limit),
         capAK = MoMlimit_region(lmAK,AK,MoM_limit))

### format the upload file 
after_MoM <- MoMlimst %>%
  select(CategoryId,	Category,	SubcategoryId,	Subcategory,	CountryCode,	capGL,capHL,capNE,capNW,capSE,capSW,capW,capHI,capAK,capE) %>%
  rename(GL=capGL,HL = capHL, NE=capNE,NW=capNW,SE=capSE,SW=capSW,W=capW,HI=capHI, AK= capAK,E=capE)


### transfer the table and the upload file is ready 
fileUpload <- gather(after_MoM, RegionCode,RegionAdjuster,GL:E,factor_key=T) %>%
  mutate(MarketCode=MarketCode) %>%
  select(MarketCode, CountryCode, RegionCode, CategoryId, SubcategoryId, RegionAdjuster) %>%
  filter(RegionAdjuster !='') %>%
  arrange(MarketCode, CountryCode, CategoryId, SubcategoryId)



### check if the otput file has right rows
check_out<- fileUpload %>%
  group_by(CountryCode, CategoryId, SubcategoryId) %>%
  summarise(n=n()) %>%
  filter((CountryCode =='USA' & n != 9) |(CountryCode =='CAN' & n !=2))
check_out

## Current month adjustments in schedule level

## manipulating global row
rownames(matrix_global) <-'.USAGlobal'
rownames(matrix_global_CAN) <-'.CANGlobal'
Global_share<-rownames_to_column(data.frame(matrix_global)) %>%
  rename(Schedule=rowname) 

Global_share_CAN<-rownames_to_column(data.frame(matrix_global_CAN)) %>%
  rename(Schedule=rowname)

## combine with cat/subcat level with global
sharepageUSA <-rbind(Global_share,output_USA) %>%
  mutate(CountryCode="USA",E="")

shareplageCAN <- rbind(Global_share_CAN,output_CAN) %>%
  mutate(CountryCode="CAN",
         GL="",HL="",NE="",NW="",SE="",SW="")

## combine Canada and USA, schedule level
sharepage1 <- rbind(shareplageCAN,sharepageUSA) %>%
  select(CountryCode,Schedule,everything()) %>%
  arrange(CountryCode,Schedule) 

## this month vs last month by cat/subcat
sharepage2 <-MoMlimst %>%
  replace(.=='Global','.Global') %>%
  mutate(Category = ifelse(Category =='Global' & CountryCode=='USA', '.USAGlobal',ifelse(Category =='Global' & CountryCode=='CAN', '.CANGlobal',Category))) %>%
  select(CountryCode,Schedule,Category,Subcategory,lmGL,lmHL,lmNE,lmNW,lmSE,lmSW,lmHI,lmAK,lmW,lmE,capGL,capHL,capNE,capNW,capSE,capSW,capHI, capAK, capW,capE) %>%
  rename(GL=capGL,HL = capHL, NE=capNE,NW=capNW,SE=capSE,SW=capSW,W=capW,HI=capHI, AK= capAK,E=capE) %>%
  arrange(CountryCode,Schedule,Category,Subcategory)

## write the file into folder to upload
write.csv(fileUpload,loadFile,row.names = F)
write.csv(sharepage1,paste(Sys.Date(),'MoMSharePage1.csv'),row.names = F)
write.csv(sharepage2,paste(Sys.Date(),'MoMSharePage2.csv'),row.names = F)

