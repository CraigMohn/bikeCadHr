##  note that lag_one/lead_one pad the new entry with the first/last value,
##      which is different than lag_n/lead_n(,1)
##    this gives flexibility with differences, but be careful!

lag_one <- function(vec) {
  return(c(vec[1],vec[-length(vec)]))
}
lead_one <- function(vec) {
  return(c(vec[-1],vec[length(vec)]))
}
lag_n <- function(vec,n) {
  if (n < length(vec)) {
    return(c(rep(NA,n),vec[1:(length(vec)-n)]))
  }
  else {
    return(vec<-NA)
  }
}
lead_n <- function(vec,n) {
  if (n < length(vec)) {
    return(c(vec[-n:-1],rep(NA,n)))
  }
  else {
    return(vec<-NA)
  }
}
smoothEpanechnikov <- function(t,x,segment,nneighbors=5,bw=5) {
  #  ignore the 3/4 scale factor, since we are using the kernel for weighting numerator and denominator
  if (missing(segment)) segment <- rep(1,length(x))
  xpresent <- as.numeric(!is.na(x))
  num <- xpresent*x
  den <- xpresent
  for (i in 1:nneighbors) {
    twt <- 1.0-((lead_n(t,i)-t)/bw)*((lead_n(t,i)-t)/bw)
    twt[is.na(twt)] <- 0
    y <- lead_n(x,i)
    y[segment != lead_n(segment,i)] <- NA
    num[!is.na(y)&(twt>0)] <- num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*y[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <- den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]
    twt <- 1.0-((lag_n(t,i)-t)/bw)*((lag_n(t,i)-t)/bw)
    twt[is.na(twt)] <- 0
    y <- lag_n(x,i)
    y[segment != lag_n(segment,i)] <- NA
    num[!is.na(y)&(twt>0)] <- num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*x[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <- den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]
  }
  return(num/den)  # if only NAs in neighbors, return NA
}
smoothTriangular <- function(t,x,segment,nneighbors=5,bw=5) {
  #  ignore the scale factor, since we are using the kernel for weighting numerator and denominator
  if (missing(segment)) segment <- rep(1,length(x))
  xpresent <- as.numeric(!is.na(x))
  num <- xpresent*x
  den <- xpresent
  for (i in 1:nneighbors) {
    twt <- 1.0-((lead_n(t,i)-t)/bw)
    twt[is.na(twt)] <- 0
    y <- lead_n(x,i)
    y[segment != lead_n(segment,i)] <- NA
    num[!is.na(y)&(twt>0)] <- num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*y[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <- den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]
    twt <- 1.0-((lag_n(t,i)-t)/bw)*((lag_n(t,i)-t)/bw)
    twt[is.na(twt)] <- 0
    y <- lag_n(x,i)
    y[segment != lag_n(segment,i)] <- NA
    num[!is.na(y)&(twt>0)] <- num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*x[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <- den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]
  }
  return(num/den)  # if only NAs in neighbors, return NA
}
dateTimeStr <- function(intDate,intTime) {
  return(paste0(stringr::str_pad(intDate,8,pad="0"),stringr::str_pad(intTime,6,pad="0")))
}

