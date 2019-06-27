#' generate heartrate statistics for a track
#'
#' \code{statsHeartRate}  processes a gps track file to summarize the hr data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param recovery_hr data frame or tibble with gps recovery hr data
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsHeartRate <- function(trackdf,recovery_hr,...) {
  if (any(!is.na(trackdf$heart_rate.bpm))) {
    firstHr <- min(which(!is.na(trackdf$heart_rate.bpm)))
    if (as.numeric(difftime(trackdf$timestamp.s[1],
                            trackdf$timestamp.s[firstHr],units="secs")) < 60) {
      hrAtStart <- as.numeric(trackdf$heart_rate.bpm[firstHr])
    }
    else {
      hrAtStart <- NA
    }
    lastHr <- max(which(!is.na(trackdf$heart_rate.bpm)))
    if (as.numeric(difftime(trackdf$timestamp.s[nrow(trackdf)],
                            trackdf$timestamp.s[lastHr],units="secs")) < 10) {
      hrAtStop <- as.numeric(trackdf$heart_rate.bpm[lastHr])
    }
    else {
      hrAtStop <- NA
    }
    hrMax <- max(trackdf$heart_rate.bpm,na.rm=TRUE)
    hrMin <- min(trackdf$heart_rate.bpm,na.rm=TRUE)
    hrMean <- mean(trackdf$heart_rate.bpm,na.rm=TRUE)
  } else {
    hrAtStart <- NA
    hrAtStop <- NA
    hrMax <- NA
    hrMin <- NA
    hrMean <- NA
  }
  if (!is.na(hrMean)) {
    hrMoment2 <- mean( (trackdf$heart_rate.bpm - hrMean)*
                         (trackdf$heart_rate.bpm - hrMean),
                       na.rm=TRUE)
  } else {
    hrMoment2 <- NA
  }

  if (!is.null(nrow(recovery_hr))) {
    if (nrow(recovery_hr)>0) {
      hrRecovery <- as.numeric(recovery_hr[1,"heart_rate.postride"])
    } else {
      hrRecovery <- NA
    }
  } else {
    hrRecovery <- NA
  }
  return(list(hrMax=hrMax,
              hrMin=hrMin,
              hrMean=hrMean,
              hrMoment2=hrMoment2,
              hrAtStart=hrAtStart,
              hrAtStop=hrAtStop,
              hrRecovery=hrRecovery))
}

#' clean up HR data for a track
#'
#' \code{repairHR}  processes a gps track file to correct HR data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param fixHR repair excessive HR values by setting them to NA
#' @param HRMax max credible HR value, larger values are errors set to NA
#' @param loud display actions taken
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return dataframe with HR data repaired
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsHeartRate}}
#'
#' @export
repairHR <- function(trackdf,fixHR=TRUE,
                     HRMax=220,
                     loud=FALSE,...) {

  ## too large HR values
  HRTooHigh <- trackdf$heart_rate.bpm > HRMax
  HRTooHigh[is.na(HRTooHigh)] <- FALSE
  if (sum(HRTooHigh) > 0) {
    if (loud) {
      cat("   there are ",sum(HRTooHigh)," too-large HR values\n")
      cat("     ",paste(sort(unique(trackdf$heart_rate.bpm[HRTooHigh])),sep=","),"\n")
    }
    if (fixHR) {
      if (loud) cat("         setting them to NA\n")
      trackdf$heart_rate.uncorrected <- trackdf$heart_rate.bpm
      trackdf$heart_rate.bpm[HRTooHigh] <- NA
    }
  }
  return(list(trackdf=trackdf,nHRTooHigh=sum(HRTooHigh)))
}

