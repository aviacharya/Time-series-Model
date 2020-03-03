library(dplyr)
library(forecast)
library(Metrics)
library(imputeTS)
library(knitr)
library(lubridate)
library(DescTools)
library(sqldf)

monthToPredict <- 6

#FinalSalesDataFrame <- read.csv(url("https://lentunestorage.blob.core.windows.net/lentune-public/Azure/SalesData20200116.csv"),header = TRUE)
FinalSalesDataFrame <- read.csv("Top100stockSales.csv",header = TRUE)

#maping data structure to header in csv file. in the csv file the stock head is i..stock
  FinalSalesDataFrame <- FinalSalesDataFrame %>%
    rename(
      Stock =ï..Stock
    )


FinalSalesDataFrame$Date <- as.Date(FinalSalesDataFrame$Date)
Getmaxdate <- max(FinalSalesDataFrame$Date)
# extract the Year
EndYear <- as.numeric(format(as.Date(Getmaxdate), "%Y"))
#extract the month
EndMonth <- as.numeric(format(as.Date(Getmaxdate), "%m"))

GetStock <- sqldf("SELECT DISTINCT Stock FROM FinalSalesDataFrame")
GetStock <- as.numeric(GetStock$Stock)
StockList <-c(GetStock)

GetBranch <- sqldf("SELECT DISTINCT Branch FROM FinalSalesDataFrame")
GetBranch <- as.numeric(GetBranch$Branch) 
BranchList <-c(GetBranch)

#filter list for testing
StockList <- c(435,113801)
BranchList <- c(2,3)



#Datelist <- c("2019-06-01","2019-07-01","2019-08-01","2019-09-01","2019-10-01","2019-11-01")
Datelist <- c("2019-11-20")
PrintColheaderOnlyOne <- TRUE

for(stockCode in StockList)
{
  for (branchcode in BranchList)
  {
    for (date in Datelist)
    {
     #  stockCode <-121382
      # branchcode <- 2
       #date <- '2019-05-01'
      
      # extract the Year
      EndYear <- as.numeric(format(as.Date(date), "%Y"))
      #extract the month
      EndMonth <- as.numeric(format(as.Date(date), "%m"))
      
      runningStockBranch <- paste("date",date, "Stock",stockCode,"Branch",branchcode, sep=" ")
      print(runningStockBranch)
      #filter the list based on the branch and stock code and arrange do the sorting by date
      FinalSalesDataFramefilter <-FinalSalesDataFrame %>% 
        filter(Branch %in% branchcode,Stock==stockCode) %>%
        arrange(Date)
      
      
      NumberOfRows <-nrow(FinalSalesDataFramefilter)
      NumberOfRows
      if(NumberOfRows > 2)
        
      {
        
        #if there is missing value then we mark it as NA 
        FinalSalesDataFramefilter <-na.omit(FinalSalesDataFramefilter)
        
        
        #-----------------impute missing date---
        sorted.data <- FinalSalesDataFramefilter[order(FinalSalesDataFramefilter$Date),]
        
        data.length <- length(sorted.data$Date)
        data.length
        
        time.min <- as.Date("2012-05-01")
        time.max <- as.Date(date) #we need make this a variable to send in
        time.fiter <- AddMonths(as.Date(date),2) 
        all.dates <- seq(time.min, time.max, by="month")
        
        all.dates
        
        all.dates.frame <- data.frame(list(Date=all.dates))
        
        
        merged.data <- merge(all.dates.frame, sorted.data,all=T)
        
        #convert the data into time series mtrix format
        dat_ts <- ts(merged.data$TotalQuantity, start=c(2012,5), end= c(EndYear,EndMonth), frequency = 12)
        
        # ARIMA MODEL
        
        dat_impute <- na.interpolation(dat_ts)
        
        
        ArimaModel <- auto.arima(dat_impute, d=1, D=1, stepwise = FALSE,approximation = FALSE,
                                 seasonal = TRUE )#residual 22.16303 and RMSE : 21.39462
        
        
        
        forecast <-forecast(ArimaModel,h=monthToPredict,level=c (80,90))
        
        
        PredictForecast <-predict(forecast)
        PredictValue <- PredictForecast$mean
        
        if(Datelist[length(Datelist)] != date){
          PredictValue<-PredictValue[1]
        }
        
        # To calculate actual value within the time frame
        ActualValue <- merged.data[merged.data$Date>as.Date(date) & merged.data$Date < as.Date(time.fiter), c("TotalQuantity")]
        if(Datelist[length(Datelist)] == date){
          ActualValue <-0
        }
        
        #Toandle NA value when printing
        LengthOfActualValue <- length(ActualValue)
        if(LengthOfActualValue == 0) {
          ActualValue <- 0
        }
        
        Stock <- stockCode
        outPutFileName <- "D:/LentuneDataScienceProject/Lentune/Test/MarginResult43.csv"
        count <- 1
        
        ActualDate <- date
        for(pVal in PredictValue){
          PredictDate <- AddMonths(as.Date(date),count) 
         
          PredictDate <-  toString(PredictDate)
          predict <-PredictValue[count]
          predictedValue <-toString(predict)
          if(count > 1){
            ActualValue <- 0
          }
          df <- cbind(PredictDate,branchcode,Stock,ActualValue,predictedValue)
          #make sure only print one header for file
          if(PrintColheaderOnlyOne==TRUE)
          {
            write.table(df,file=outPutFileName, append=TRUE,sep=",",col.names=TRUE,row.names=FALSE)
          }
          else
          {
            write.table(df,file=outPutFileName, append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
          }
          PrintColheaderOnlyOne <-FALSE
          count <- count+1
        }
      }
      
    }
    
  }
  
}







