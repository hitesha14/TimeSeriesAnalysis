library(tsoutliers)
library(expsmooth)
library(fma)
library(tseries)
library(forecast)
library(earth)
library(jpeg)

setwd("./ydata-labeled-time-series-anomalies-v1_0/A1Benchmark/")
examples <- list.files("./")
print(examples)
length(examples)

labels <- c('Raw', 'Smoothed', 'second time smoothed')
cols <- c('red', 'yellow', 'blue')


for (i in c(4:length(examples))){ # length(examples
  dataseries <- read.csv(examples[i])
  series <- ts(dataseries[2], start = 1, end = length(dataseries[,2]),na.omit(daily_data$cnt_ma), frequency = 1)
  title <- paste(tools::file_path_sans_ext(examples[i]), ".jpeg", sep="")
  filepath <- file.path("Plots",title)
  jpeg(file=filepath)
  plot(series, col = cols[1], main=title)
  dev.off()
  
  title <- paste(tools::file_path_sans_ext(examples[i]),"outlier", ".jpeg", sep="")
  filepath <- file.path("Plots",title)
  jpeg(file=filepath)
  outlier.series <- tsoutliers::tso(series,types = c("AO","LS","TC"),maxit.iloop=10)
  plot(outlier.series, col = cols[1], main=title)
  dev.off()
}
