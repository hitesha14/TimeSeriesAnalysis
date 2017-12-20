## AUTHOR Hitesha Mukherjee##
## Decomposing the Time Series Data into Seasonal , Trend and Residual using STL Decomposition##
## OUTLIER DETECTING USING TS OUTLIERS ON ARIMA###
## We are working on a dataset which has 2 folders and huge number of CSV's hence we are iterating in the for loop and saving both the plots and ouput results from the console into separate files and folders
library(tsoutliers)
library(expsmooth)
library(fma)
library(tseries)
library(forecast)
library(earth)
library(jpeg)
setwd("current working directory")
files<- list.files(path = "listing the files", pattern =".csv")
files
sort(files)

for(i in c(4:length(files)))
{
  dataseries <- read.csv(files[i])
  temp1 <- ts(dataseries[2], start = 1, end = length(dataseries[,2]),na.omit(daily_data$cnt_ma), frequency = 2)
  ## STL Decomposition
  obj1 <- stl(temp1,s.window = "periodic",t.window = 15)
  seasonal   <- obj1$time.series[,1]
  trend <- obj1$time.series[,2]
  random  <- obj1$time.series[,3]
  deseasonal_cnt <- seasadj(obj1) # deseasonalizing Data  
  adf <- adf.test(temp1, alternative = "stationary")
  
  ## ACF AND PACF Tests
  Acf(temp1, main='', plot = TRUE)
  Pacf(temp1, main='', plot = TRUE)
  
  ## Applying Auto ARIMA and extracting the residuals for OUTLIER ANALYSIS
  arm <- auto.arima(deseasonal_cnt, seasonal=FALSE)
  par<- coefs2poly(arm)
  residual <- residuals(arm)
  
  ##For saving entire plots of STL Decomposition to separate folder for a large dataset
  vec = Vectorize(obj1)
  title <- paste(tools::file_path_sans_ext(files[i]), ".jpeg", sep="")
  filepath <- file.path("folder path for storing plots",title)
  jpeg(filepath) 
   plot(vec)
  
  # for capturing the Arima and ADF Test Output from the console and storing it in a file for large dataset
  adftest <- capture.output(adf)
  arima <- capture.output(arm)
  cat("ADFTest", adftest, file="adf-results.txt", sep="/", append=TRUE)
  cat("Arima", arima, file="arima-model-results.txt", sep="/", append=TRUE) 	 	
  sink()
  sink()
  dev.off()
  
  ## For saving entire plots of outliers to separate folder for a large dataset
  title <- paste(tools::file_path_sans_ext(files[i]),"outlier", ".jpeg", sep="")
  filepath <- file.path("folder path for storing outliers",title)
  jpeg(filepath)
  outliers <- locate.outliers(residual,par,cval=3.5,types=c("IO","AO","LS","TC"))
  outlierdetails <-capture.output(outliers)
  
  #for capturing the Different Types of OUTLIER CHARECTERISTIC from the Output console and storing it in a file for large dataset
  cat("OUTLIERS", outlierdetails, file="outliers-results.txt", sep="/", append=TRUE)
  sink()
  plot(outliers)
  dev.off()
}

