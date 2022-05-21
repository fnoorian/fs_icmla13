library(wavelets)

histwinext = function(dd, w=3, pstr=as.character(substitute(dd))) {	
# an extension of histwin which works for vectors, matrices and dataframes
# collects and embeds w lengthed windows of data as a vector
# INPUTS:
#    dd: data input
#    w:  window length
#    pstr: name of datacolumn
# EXAMPLE
#  a=1:0histwin(
  if ((is.null(dim(dd))) || (ncol(dd) == 1)) { # for one column objects
    # embed the data
    embeded.ts = embed(dd, w)

    # convert back to xts if required
    if (is.xts(dd)) {
      embeded.ts = xts(embeded.ts, index(dd)[w:length(dd)]);
      embeded.ts = na.omit(embeded.ts)
    }
    
    # give each column a name
    colnames(embeded.ts) = paste0(pstr, ".w", 1:NCOL(embeded.ts))

    # return the object
    return(embeded.ts);
  }
  else { # for multi column objects

    # call histwin each separately
    nc = ncol(dd)
    hw = lapply(1:nc, function(i) histwin(dd[,i], w=w, pstr=paste0(pstr, '.', colnames(dd[,i]))))
 
    # combine them
    hw.combined = NULL
    for (i in 1:nc) {
      hw.combined = cbind(hw.combined, hw[[i]])
    }

    return(hw.combined)
  }
}


################################################################################
mape <- function(obs, pred) mean((abs((obs-pred)/obs)))*100
rmse <- function(obs, pred) sqrt(mean((obs-pred)^2))
mse <- function(obs, pred) mean((obs-pred)^2)
mae <- function(obs, pred) mean((abs(obs-pred)))
nmse <- function(obs, pred) mean((obs-pred)^2)/var(as.numeric(obs))
################################################################################
calendar_matrix = function(dates, holidays) {

    holidays = as.timeDate(holidays)

    IsHoliday = function(date){
     
      if (length(holidays[holidays == date]) > 0)
        return(TRUE)
      return(FALSE)
      
    }

    wday.matrix = xts(matrix(0, length(dates), 8), dates)
    for (i in 1:length(dates)) {
        dt = dates[i]
        wday.matrix[i, wday(dt)] = 1
        wday.matrix[i, 8] = IsHoliday(dt)
    }

    colnames(wday.matrix) = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "h")

    return (wday.matrix)
}

################################################################################
GetMRA = function(ts, start.index = 2){
  
  ret.vals = sapply(start.index:length(ts), function(i){
    mra.ts <- mra(as.numeric(ts)[1:i], n.levels=3, boundary="periodic", method="modwt")
    cbind(last(mra.ts@D$D1), last(mra.ts@D$D2), last(mra.ts@D$D3), 
          last(mra.ts@S$S1),last(mra.ts@S$S2), last(mra.ts@S$S3))
  })

  #mra.ts= mra(as.ts(ts),n.levels=3, boundary="periodic", method="modwt")  
  #ret.vals = cbind(mra.ts@D$D1, mra.ts@D$D2, mra.ts@D$D3, mra.ts@S$S1, mra.ts@S$S2,mra.ts@S$S3)  

  xts(t(ret.vals), order.by=index(ts[start.index:length(ts)]))
}