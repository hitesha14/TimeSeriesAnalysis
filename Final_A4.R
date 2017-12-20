library(tsoutliers)
library(expsmooth)
library(fma)
library(tseries)
library(forecast)
library(earth)
library(jpeg)

make_ts <- function(Tseries = tseries, Tcol = col){
  require(tseries)
  TS <- ts(Tseries[Tcol], start = 1, end = length(Tseries[,Tcol]),na.omit(daily_data$cnt_ma), frequency = 1)
  return(TS)
}

smoothen_series <- function(Tseries = tseries, param = 10){
  smoothedseries <- filter(Tseries, filter=rep(1/param, param),
                           method= 'convolution', sides=2)
  return(smoothedseries)
}

plot_ts <- function(Tseries = tseries, Tcol = col, Tmain = main){
  plot(Tseries, col = Tcol, main= Tmain)
}

setwd("./ydata-labeled-time-series-anomalies-v1_0/A4Benchmark/")
examples <- list.files("./")
print(examples)
length(examples)

labels <- c('Raw', 'Smoothed', 'second time smoothed')
cols <- c('red', 'yellow', 'blue')

for (i in c(2:length(examples))){ # length(examples){
  print(examples[i])
  dataseries <- read.csv(examples[i])
  series <- make_ts(dataseries, 2)
  plot_ts(series, cols[1], examples[i])
  
  title <- paste(tools::file_path_sans_ext(examples[i]), ".jpeg", sep="")
  filepath <- file.path("Plots",title)
  jpeg(file=filepath)
  plot(series, col = cols[1], main=title)
  dev.off()
  
  smoothedseries <- smoothen_series(series, 140)
  lines(smoothedseries, col=cols[2], lwd=2)
  
  secondsmoothed <- smoothen_series(smoothedseries, 140)
  lines(secondsmoothed, col=cols[3], lwd=2)
  legend("bottomright", labels, col=cols, lwd=2)
  
  df <- data.frame(smoothedseries)
  ind <- which(is.na(df))
  for (j in c(1: length(ind))) {
    df$smoothedseries[ind[j]] <- dataseries$value[[ind[j]]]    
  }
  df_series <- cbind(dataseries[1], df)
  mars.fit <-  earth(y = df_series[2], x = df_series[1], degree = 1, nprune = 7, nfold = 10, ncross = 3)
  # plot(mars.fit)
  # plot.earth.models(mars.fit)
  knots <- mars.fit$cuts[mars.fit$selected.terms]
  knots[1] <- dataseries[1,1]
  knots_sort <- sort(knots)
  knots_sort <- unique(knots_sort)
  for (k in c(2:length(knots_sort)-1)){
    sub_df <- subset(dataseries, dataseries$timestamps > knots_sort[k] & dataseries$timestamps < knots_sort[k+1]) 
    sub_series <- ts(sub_df[2], start = 1, end = length(sub_df[,2]),na.omit(daily_data$cnt_ma), frequency = 1)
    
    title <- paste(tools::file_path_sans_ext(examples[i]),"sub", k , ".jpeg", sep="")
    print(title)
    filepath <- file.path("Plots",title)
    jpeg(file=filepath)
    plot(sub_series, main = k)
    dev.off()
    
    title <- paste(tools::file_path_sans_ext(examples[i]),"outlier", k , ".jpeg", sep="")
    print(title)
    filepath <- file.path("Plots",title)
    jpeg(file=filepath)
    
    try(outlier.series <- tsoutliers::tso(sub_series,types = c("AO","LS","TC"),maxit.iloop=100, maxit.oloop = 100) , silent = TRUE)
    try(plot(outlier.series, col = cols[1], main= title), silent = TRUE)
    dev.off()
  }
}
