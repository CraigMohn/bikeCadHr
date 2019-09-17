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

