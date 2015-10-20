# =====================================================================
# CSE487/587
# Author: Nischala Manjunath
# Email: nmanjuna@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
#install.packages("forecast")
#install.packages("fpp")


library(forecast)
library(fpp)

# need to read the stocklist, and loop all files
### TO DO

setwd('/gpfs/courses/cse587/spring2015/data/hw2/data')
file.names = list.files(pattern="*.csv")



MAE_arima=c()
MAE_hw=c()
MAE_lm=c()
filelist1=c()
filelist2=c()
filelist3=c()

for( file in 1:length(file.names)) {
  
  filename = file.names[file]
  
  # if file is not empty
  if(file.info(filename)[1]>0) {
    
    nrows = sapply(filename, function(f) nrow(read.csv(f)))
    
    if(sum(nrows)>=754) {
      
      # read one csv file into variable (DO NOT EDIT)
      textData=read.csv(file=filename, header=T)
      
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE1 = matrix(NA,1,length(testData))
      MAE2 = matrix(NA,1,length(testData))
      MAE3 = matrix(NA,1,length(testData))
      
      # apply Linear Regression Model  (DO NOT EDIT) 
      fitData1 = auto.arima(trainData, seasonal=FALSE, lambda=NULL, approximation=TRUE)
      fitData2 = HoltWinters(trainData,gamma=FALSE)
      fitData3 = tslm(trainData ~ trend)
      
      ### TO DO
      
      # apply forecast(DO NOT EDIT)
      forecastData1 = forecast(fitData1, h=length(testData))
      forecastData2 = forecast(fitData2, h=length(testData))
      forecastData3 = forecast(fitData3, h=length(testData))
      
      # print variable and see what is in the result data set
      #print(forecastData1)
      #print(forecastData2)
      #print(forecastData3)
      
      # calculate Mean Absolute Error 
      for(i in 1:length(testData))
      {
        MAE1[1,i] = abs(forecastData1$mean[i] - testData[i])
        MAE2[1,i] = abs(forecastData2$mean[i] - testData[i])
        MAE3[1,i] = abs(forecastData3$mean[i] - testData[i])
      }
      
      # this is the result you need for stock AAPL
      #print(sum(MAE1[1,1:10]))
      #print(sum(MAE2[1,1:10]))
      #print(sum(MAE3[1,1:10]))
      MAE_arima[file]=sum(MAE1[1,1:10])
      filelist1[file]=filename
      
      MAE_hw[file]=sum(MAE2[1,1:10])
      filelist2[file]=filename
      
      MAE_lm[file]=sum(MAE3[1,1:10])
      filelist3[file]=filename
      
    }
  }
}
table1=data.frame(filelist1,MAE_arima)

newtable1 = table1[order(MAE_arima),]

finaltable1=newtable1[1:10,]
print("Arima Model")
print(finaltable1)
#plot the top 10 minimum sum of MAE in Arima
jpeg("/gpfs/courses/cse587/spring2015/students/nmanjuna/hw2/arima.jpg")
plot(finaltable1$MAE_arima, main ="Arima Model", xlab="Stocks", ylab="Sum of MAE",  col = "blue", xaxt='n')
axis(1, at=1:10, finaltable1$filelist1[1:10])
lines(finaltable1$MAE_arima, lw = 2, col = "red")
dev.off()

table2=data.frame(filelist2,MAE_hw)

newtable2 = table2[order(MAE_hw),]

finaltable2=newtable2[1:10,]
print("Holt-Winters Model")
print(finaltable2)
#plot the top 10 minimum sum of MAE in HoltWinters
jpeg("/gpfs/courses/cse587/spring2015/students/nmanjuna/hw2/hw.jpg")
plot(finaltable2$MAE_hw, main ="Holt-Winters Model", xlab="Stocks", ylab="Sum of MAE",  col = "blue", xaxt='n')
axis(1, at=1:10, finaltable2$filelist2[1:10])
lines(finaltable2$MAE_hw, lw = 2, col = "red")
dev.off()

table3=data.frame(filelist3,MAE_lm)

newtable3 = table3[order(MAE_lm),]

finaltable3=newtable3[1:10,]
print("Linear Regression Model")
print(finaltable3)
#plot the top 10 minimum sum of MAE in Linear Regression
jpeg("/gpfs/courses/cse587/spring2015/students/nmanjuna/hw2/lm.jpg")
plot(finaltable3$MAE_lm, main ="Linear Regression Model", xlab="Stocks", ylab="Sum of MAE",  col = "blue", xaxt='n')
axis(1, at=1:10, finaltable3$filelist3[1:10])
lines(finaltable3$MAE_lm, lw = 2, col = "red")
dev.off()


