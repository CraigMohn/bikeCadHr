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
stripDupTrackRecords <- function(track,fixDistance=FALSE) {
  #  first thing, strip out observations with no distance (no speed sensor and no gps)
  rettrack <- track[!is.na(track$distance.m),]
  #  drop any observation where the timestamp is the same as the preceding
  #  this should be rare
  firstInRun <- rettrack$timestamp.s != dplyr::lag(rettrack$timestamp.s)
  firstInRun[1] <- TRUE
  ndropped <- sum(!firstInRun)
  if (ndropped > 0) {
    print(paste0("dropping ",ndropped," obs with duplicate timestamps"))
  }
  rettrack <- rettrack[firstInRun,]
  if (is.unsorted(rettrack$timestamp.s,strictly=TRUE))
    stop(paste0("timestamps are not strictly increasing!"))
  if (is.unsorted(rettrack$distance.m,strictly=FALSE)) {
    if (fixDistance) {
      deltaDist <- c(0.0,diff(rettrack$distance.m))
      nonMono <- deltaDist < 0.0
      b4NonMono <-c(nonMono[-1],FALSE)
      cat(" fixing ",sum(nonMono)," non-monotonicities in distance.m\n")
      if (any(rettrack$segment[nonMono]==rettrack$segment[b4NonMono]))
        stop("nonmonotonicity within a segment")
      offset <- -cumsum(pmin(deltaDist,0.0))
      fixdist <- -pmin(deltaDist,0.0)
      fixdist[nonMono] <- fixdist[nonMono] +
        raster::pointDistance(cbind(rettrack$position_lon.dd[nonMono],
                                    rettrack$position_lat.dd[nonMono]),
                              cbind(rettrack$position_lon.dd[b4NonMono],
                                    rettrack$position_lat.dd[b4NonMono]),
                              lonlat=TRUE)
      rettrack$distance.m <- rettrack$distance.m + cumsum(fixdist)
    } else {
      stop(paste0("distance is not nondecreasing!"))
    }
  }
  return(rettrack)
}
smoothDataSegments <- function(yvec,xvar,segment,
                               bw,nneighbors=10,kernel="epanechnikov",
                               replaceNAs=TRUE) {
  if (!is.vector(xvar)) stop("smoothDataSegments needs xvar as vector")
  if (missing(segment)) segment <- rep(1,length(xvar))
  if (length(xvar)!=length(segment))
    stop("smoothDataSegments needs segment and xvar same length")

  xret <- vector("numeric",length(xvar))
  xret <- NA  #  if anything goes wrong return garbage
  for (seg in unique(segment)) {
    inseg <- segment == seg
    if (sum(inseg)>1) {
      xret[inseg] <- smoothData(yvec=yvec[inseg],xvar=xvar[inseg],
                                bw=bw,nneighbors=nneighbors,
                                kernel=kernel,replaceNAs=replaceNAs)
    } else {
      xret[inseg] <- xvar[inseg]
    }
  }
  return(xret)

}
smoothData <- function(yvec,xvar,bw,nneighbors=10,
                       kernel="epanechnikov",replaceNAs=TRUE) {
  if (!is.vector(xvar)) stop("smoothData needs xvar as vector")
  if (!is.vector(yvec)) stop("smoothData needs yvec as vector")
  if (length(xvar)==0) stop("smoothData needs some points")
  if (length(yvec)!=length(xvar))
    stop("smoothData needs xvar and yvec same length")
  #  ignore the scale factor, since we are using the kernel for weighting
  #            numerator and denominator
  #  note this function fills in missing values with approximation unless
  #   replaceNAs is FALSE
  if (!(is.vector(yvec) & is.vector(xvar) & length(yvec)==length(xvar)))
    stop("need 2 vectors of same length in smoothData")
  ypresent <- as.numeric(!is.na(yvec))
  num <- ypresent*yvec
  den <- ypresent
  xvar <- as.numeric(xvar)
  for (i in 1:nneighbors) {
    if (kernel == "triangular") {
      twt <- 1.0-((lead_n(xvar,i)-xvar)/bw)
    } else if (kernel == "epanechnikov") {
      twt <- 1.0-((lead_n(xvar,i)-xvar)/bw)*((lead_n(xvar,i)-xvar)/bw)
    } else {
      stop("invalid kernel")
    }
    twt[is.na(twt)] <- 0
    y <- lead_n(yvec,i)
    num[!is.na(y)&(twt>0)] <-
      num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*y[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <-
      den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]

    if (kernel == "triangular") {
      twt <- 1.0-((lag_n(xvar,i)-xvar)/bw)
    } else if (kernel == "epanechnikov") {
      twt <- 1.0-((lag_n(xvar,i)-xvar)/bw)*((lag_n(xvar,i)-xvar)/bw)
    } else {
      stop("invalid kernel")
    }
    twt[is.na(twt)] <- 0
    y <- lag_n(yvec,i)
    num[!is.na(y)&(twt>0)] <-
      num[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]*y[!is.na(y)&(twt>0)]
    den[!is.na(y)&(twt>0)] <-
      den[!is.na(y)&(twt>0)] + twt[!is.na(y)&(twt>0)]
  }
  retvec <- num/den # if only NAs in neighbors, return NA
  if (!replaceNAs) retvec[is.na(yvec)] <- NA
  return(retvec)
}
dateTimeStr <- function(intDate,intTime) {
  return(paste0(stringr::str_pad(intDate,8,pad="0"),
                stringr::str_pad(intTime,6,pad="0")))
}
#  this was lifted from stack overflow - credit author
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z:i,(i+2):w)] <= x[i+1])) return(i+1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

