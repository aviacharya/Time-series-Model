setwd("D:/LentuneDataScienceProject/Lentune")
library(dplyr)
library(zoo)
library(xts)
library(PerformanceAnalytics)
library(corrplot)
library(ggcorrplot)
library(lubridate)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(visdat)
library(ggplot2)

#Reading the Receivable Invoice Line dataset and get glimpse of the data to see every column

RecievableInvoiceLine <- read.csv("ReceivableInvoiceLine.csv",header = TRUE)
nrow(RecievableInvoiceLine)
View(RecievableInvoiceLine)
glimpse(RecievableInvoiceLine)

#Reading the Receivable Invoice dataset and get glimpse of the data to see every column

ReceivableInvoice <- read.csv("ReceivableInvoice.csv",header = TRUE)
nrow(ReceivableInvoice)
View(ReceivableInvoice)
glimpse(ReceivableInvoice)

#Reading the stock dataset and get glimpse of the data to see every column

stock <- read.csv("stock.csv",header = TRUE)
nrow(stock)
View(stock)
glimpse(stock)

#Reading the Account Transaction dataset and get glimpse of the data to see every column

AccountTransaction <- read.csv("AccountTransaction.csv",header = TRUE)
nrow(AccountTransaction)
View(AccountTransaction)
glimpse(AccountTransaction)

#Reading the branch dataset and get glimpse of the data to see every column
Branch <- read.csv("Branch.csv",header = TRUE)
nrow(Branch)
View(Branch)
glimpse(Branch)

#=========Wrangling the dataset==============

#Sqldf package is used for running the sql in R dataframe to join all the dataset

FinalDataFrame <- sqldf("SELECT *
              FROM RecievableInvoiceLine
              inner join stock on stock.oid = RecievableInvoiceLine.Stock
              inner join ReceivableInvoice on ReceivableInvoice.oid = RecievableInvoiceLine.ReceivableInvoice
              inner join AccountTransaction on AccountTransaction.oid = ReceivableInvoice.AccountTransaction
              inner join Branch on Branch.OID = ReceivableInvoice.Branch
              where stock.IsMiscStock='False'")
nrow(FinalDataFrame)
glimpse(FinalDataFrame)



#==========testing specific branch



#View(testSpecificBranch)


#=======end===============

#lubridate package is used to extract data and time from date column


FinalDataFrame$Month <- month(FinalDataFrame$TransactionDate)
FinalDataFrame$Year <- year(FinalDataFrame$TransactionDate)

# selecting the only require column for analysis

FinalDataFrame<- FinalDataFrame%>%
  select(OID,TransactionDate,Quantity,Year,Month,Price,GrossAmount,Code,SellingPrice,Branch,Code..118,Amount,DiscountAmount,DiscountPercent,DiscountedPrice,
         Stock,GrossAmount,ExcludingTaxAmount,TotalTaxAmount,IncludingTaxAmount,TaxAmount,AllocatedAmount,TaxAmount1,TaxAmount2,TaxAmount3,CostPriceMarginReport,IncludingTaxAmount)


View(FinalDataFrame)

# grouping the dataset by stock , branch, month and year

FinalSalesDataFrame <- FinalDataFrame %>%
  group_by(Stock,Code,Branch,Code..118,Month,Year)%>%
  summarise(TotalsaleNumber= sum(Quantity),DiscountPrice =mean(DiscountedPrice),DiscountPercent = mean(DiscountPercent),StockPrice = mean(Price),
            TaxAmount = sum(TaxAmount1), TotalSaleAmount=sum(IncludingTaxAmount))

#renaming the column

FinalSalesDataFrame <- FinalSalesDataFrame %>%
  rename(
    BranchCode=Code..118,
    StockCode=Code
  )

View(FinalSalesDataFrame)
glimpse(FinalSalesDataFrame)

#-------------write final csv-----
#saving the dataframe in csv file.
write.csv(FinalSalesTestDataFrame, file = 'D:/LentuneDataScienceProject/Lentune/FinalSalesTestDataFrame.csv')



#--------------------------------------------------------------------------

#----filter the data with branch code and stock------
BranchFilter <-c(2)
FinalSalesDataFramefilter <-FinalSalesDataFrame %>%
  filter(Branch %in% BranchFilter,Stock==436) 
View(FinalDataFrameGroupByfilter)


#---------missing value----
#plot the chart of missing values of the dataset

vis_dat(FinalSalesDataFramefilter,warn_large_data = FALSE) +
  ggtitle("Missing Value Distribution")

#----correlation plot------------

CorMatrix<-cor(dfMissingValue1)
ggcorrplot(CorMatrix, method = "circle")

