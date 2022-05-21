require(quantmod)

# Override EMA
EMA <- function (x, n = 10, wilder = FALSE, ratio = NULL) 
{
    x <- try.xts(x, error = as.matrix)
    x <- as.xts(do.call(data.frame,lapply(x, function(x) replace(x, is.infinite(x),0))))
    x <- as.xts(do.call(data.frame,lapply(x, function(x) replace(x, is.nan(x),0))))
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    x.na <- xts:::naCheck(x, n)
    if (missing(n) && !missing(ratio)) 
        n <- trunc(2/ratio - 1)
    if (is.null(ratio)) {
        if (wilder) 
            ratio <- 1/n
        else ratio <- 2/(n + 1)
    }
    ma <- .Call("ema", x, n, ratio, PACKAGE = "TTR")
    reclass(ma, x)
}


MyRMAX <- function(data, n=5){
  window = n
  
  data.length = length(data)
  if (window >= data.length)
    stop("Invalid window size")
  
  max.vector = data * NA # just to keep the indices
  
  for (i in 0:(data.length-window)){
    end.index = data.length-i
    start.index = end.index-window+1
    current.window = data[start.index:end.index]
    max.vector[end.index] = max(current.window)
  }
  
  return(max.vector)
  
}

MyRMIN <- function(data, n=5){
  window = n
  
  data.length = length(data)
  if (window >= data.length)
    stop("Invalid window size")
  
  min.vector = data * NA # just to keep the indices
  
  for (i in 0:(data.length-window)){
    end.index = data.length-i
    start.index = end.index-window+1
    current.window = data[start.index:end.index]
    min.vector[end.index] = max(current.window)
  }
  
  return(min.vector)
  
}

MySD <- function(data, n){

 #sd(as.numeric(data))   
 runSD(as.numeric(data), n)

}

MyMeanDev <- function(data, window=5){
 
  mavg <- SMA(data, window)
  meanDev <- runMAD(data, window, center = mavg, stat = "mean")
  return(meanDev)
 
}

MyHistWin <- function(x, n) histwinext(x, n)

UpFunc <- function(x){
    x = diff(x)
    for (i in 2:length(x))# Don't touch the NA element, starts from 2
    { if (x[i]<0)
      x[i] = 0
    }
    return(x)
}
  
DownFunc <- function(x){
    x = diff(x)
    for (i in 2:length(x))# Don't touch the NA element, starts from 2
    { if (x[i]>0)
      x[i] = 0
    }
    return(-x)
}
