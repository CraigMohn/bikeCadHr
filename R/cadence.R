#' summarize cadence statistics for a track
#'
#' \code{statsCadence}  processes a gps track file to correct and
#'   summarize cadence data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param sessionpedalstrokes number of pedalstrokes in session records of
#'    fit file
#' @param loud print useful trace information
#' @param cadTrimBegSecs number of seconds after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndSecs number of seconds before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimBegMeters number of meters after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndMeters number of meters before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
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
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsCadence <- function(trackdf,sessionpedalstrokes=NA,
                         loud=FALSE,
                         cadTrimBegSecs=15,cadTrimBegMeters=10,
                         cadTrimEndSecs=20,cadTrimEndMeters=15,...) {

  pedaling <- !is.na(trackdf$cadence.rpm) & trackdf$cadence.rpm > 0
  segtimes <- tibble::data_frame(timestamp=cumsum(trackdf$deltatime),
                                 distance.m=trackdf$distance.m,
                                 segment=trackdf$segment)    %>%
    dplyr::group_by(segment) %>%
    dplyr::mutate(segbegtime=min(timestamp),
                  segendtime=max(timestamp),
                  segbegdist=min(distance.m),
                  segenddist=max(distance.m)) %>%
    dplyr::arrange(timestamp)

  innersegment <- segtimes$timestamp >= segtimes$segbegtime+cadTrimBegSecs &
    segtimes$timestamp <= segtimes$segendtime-cadTrimEndSecs &
    segtimes$distance.m >= segtimes$segbegdist+cadTrimBegMeters &
    segtimes$distance.m <= segtimes$segenddist-cadTrimBegMeters

  cadenceNumerator <- sum(trackdf$deltatimestart[pedaling]*
                            trackdf$cadence.rpm[pedaling])
  cadenceNumeratorInner <- sum(trackdf$deltatimestart[pedaling&innersegment]*
                                 trackdf$cadence.rpm[pedaling&innersegment])

  avgcadenceNoZerosSum <- cadenceNumerator / pedalTime(trackdf,...)
  avgcadenceWithZerosSum <- cadenceNumerator / rollingTime(trackdf,...)
  avgcadenceMidsegment <- cadenceNumeratorInner /
    sum(trackdf$deltatime[pedaling & innersegment])
  if (is.na(sessionpedalstrokes)) {
    avgcadenceNoZerosSession <- NA
    avgcadenceWithZerosSession <- NA
  } else {
    if (loud) {
      cat("pedalstrokes - session = ",sessionpedalstrokes," , summed = ",
                   cadenceNumerator/60,"   summed exceeds session by  ",
                   ((cadenceNumerator/(60*sessionpedalstrokes))-1),"\n")
    }
    avgcadenceNoZerosSession <- sessionpedalstrokes / (pedalTime(trackdf,...)/60)
    avgcadenceWithZerosSession <- sessionpedalstrokes / (rollingTime(trackdf,...)/60)
    if (abs(avgcadenceNoZerosSession-avgcadenceNoZerosSum) >= 0.1 & loud)
      print(paste0("  cadence nozeros (session,sum) = ",avgcadenceNoZerosSession," , ",avgcadenceNoZerosSum))
    if (abs(avgcadenceWithZerosSession-avgcadenceWithZerosSum) >= 0.1 & loud)
      print(paste0("  cadence withzeros (session,sum) = ",avgcadenceWithZerosSession," , ",avgcadenceWithZerosSum))
  }

  return(list(avgcadenceNoZerosSum=avgcadenceNoZerosSum,
              avgcadenceWithZerosSum=avgcadenceWithZerosSum,
              avgcadenceNoZerosSession=avgcadenceNoZerosSession,
              avgcadenceWithZerosSession=avgcadenceWithZerosSession,
              avgcadenceMidsegment=avgcadenceMidsegment,
              summedPedalStrokes=cadenceNumerator/60))
}


#' clean up cadence data for a track
#'
#' \code{repairCadence}  processes a gps track file to correct and
#'   summarize cadence data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param fixCadence repair cadence errors
#' @param cadMax max credible cadence value, larger values are errors
#' @param cadMin min nonzero cadence value, smaller values set to 0
#' @param cadCorrectTooHigh repair excessive cadence values
#'    using triangular-kernel-weighted average of the nearest nonmissing values
#'    in the same segment
#' @param cadStuckMin threshold cadence value for removing repeated low values,
#'    useful for some sensor/GPS combinations.  0 means no checking for this.
#' @param cadStuckRep minimum length of runs of low cadence values to remove,
#'    useful for some sensor/GPS combinations
#' @param cadStuckSpdDelta proportionate change from initial speed to remove
#'    useful for some sensor/GPS combinations
#' @param cadCorrectStopped repair cadence by setting cadence to 0 when speed
#'    is zero.  This is what would be appropriate if magnet parked near sensor
#'    was generating spurious clicks or if moving pedal while at stoplight
#' @param cadCorrectNA repair cadence missing values using
#'    triangular-kernel-weighted average of the nearest nonmissing values in
#'    the same segment
#' @param cadCorrectWindowSec window size for kernel smoothing of cadence
#' @param loud display actions taken
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return dataframe with cadence data repaired
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsCadence}}
#'
#' @export
repairCadence <- function(trackdf,fixCadence=TRUE,
                          cadMax=160,cadMin=0,cadCorrectTooHigh=TRUE,
                          cadStuckMin=0,cadStuckRep=4,cadStuckSpdDelta=0.07,
                          cadCorrectStopped=TRUE,cadCorrectNA=FALSE,
                          cadCorrectWindowSec=7,loud=FALSE,...) {
  trackdf$cadence.uncorrected <- trackdf$cadence.rpm
  cadChanged <- FALSE
  ## too large cadence values
  cadTooHigh <- trackdf$cadence.rpm > cadMax
  cadTooHigh[is.na(cadTooHigh)] <- FALSE
  if (sum(cadTooHigh) > 0) {
    if (loud) {
      if (fixCadence) cat("  fixing")
      cat("  ",sum(cadTooHigh)," too-large cadence values\n")
      cat("   ",paste(sort(unique(trackdf$cadence.rpm[cadTooHigh])),sep=","),"\n")
    }
    if (fixCadence) {
      if (cadCorrectTooHigh) {
        trackdf$cadence.rpm[cadTooHigh] <- NA
        cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                              xvar=cumsum(trackdf$deltatime),
                                              segment=trackdf$segment,
                                              bw=cadCorrectWindowSec,
                                              nneighbors=cadCorrectWindowSec,
                                              kernel="triangular",
                                              replaceNAs=TRUE)
        trackdf$cadence.rpm[cadTooHigh] <-
          cadenceSmoothed[cadTooHigh]
      } else {
        trackdf$cadence.rpm[cadTooHigh] <- NA
      }
      cadChanged <- TRUE
    }
  }
  nCadTooHigh <- sum(cadTooHigh)

  ## too small cadence values
  cadTooLow <- trackdf$cadence.rpm > 0 & trackdf$cadence.rpm < cadMin
  cadTooLow[is.na(cadTooLow)] <- FALSE
  if (sum(cadTooLow) > 0) {
    if (loud) {
      if (fixCadence) cat("  fixing")
      cat("  ",sum(cadTooLow)," too-small cadence values\n")
      cat("   ",paste(sort(unique(trackdf$cadence.rpm[cadTooLow])),
                       sep=","),"\n")
    }
    if (fixCadence) {
      trackdf$cadence.rpm[cadTooLow] <- 0
      cadChanged <- TRUE
    }
  }
  nCadTooLow <- sum(cadTooLow)

  ######   Cadence stuck on low value
  nCadStuck <- 0
  if (cadStuckMin > 0) {
    cadrle <- rle(trackdf$cadence.rpm)
    cadrlestops <- cumsum(cadrle[["lengths"]])
    cadrlestarts <- c(0,cadrlestops[-length(cadrlestops)]) + 1
    zeroinrun <- mapply(zerosinrange,
                        begin=cadrlestarts+1,
                        end=cadrlestops,
                        MoreArgs = list(vec=trackdf$speed.m.s))
    stuckrun <-
      #  stuck cadence is nonmissing and positive
      !is.na(cadrle[["values"]]) &
      cadrle[["values"]] > 0 &
      #  stuck value is less than threshold
      cadrle[["values"]] <= cadStuckMin &
      #  run of stuck values is long enough
      cadrle[["lengths"]] >= cadStuckRep &
      #  run hits a stop or next cadence is zero
      #( (trackdf$speed.m.s[cadrlestops] == 0) |
      ( zeroinrun |
          (c(cadrle[["values"]][-1]==0,TRUE) &
             !is.na(c(cadrle[["values"]][-1],TRUE))) )
    if (max(stuckrun) > 0) {
      #  zap cadence values when speed deviates from first in run
      stuckstarts <- cadrlestarts[stuckrun]
      stuckstops <- cadrlestops[stuckrun]
      ssspdrle <- cadrle
      ssspdrle[["values"]] <- trackdf$speed.m.s[cadrlestarts]
      ssspdrle[["values"]][!stuckrun] <- 0.0
      stuckstartspeed <- inverse.rle(ssspdrle)
      speedfact <- abs(stuckstartspeed - trackdf$speed.m.s)/stuckstartspeed
      speedfact[stuckstartspeed==0.0] <- 0
      zapcadence <- speedfact > cadStuckSpdDelta
      if (max(zapcadence) > 0) {
        if (loud) {
          if (fixCadence) cat("  zeroing")
          cat("  ",sum(zapcadence)," apparently stuck cadence values \n")
          temp <- trackdf[zapcadence,c("timestamp.s","speed.m.s","cadence.rpm",
                                       "distance.m")]
          print(temp,n=70,na.print="NA")
        }
        if (fixCadence) {
          trackdf$cadence.rpm[zapcadence] <- 0
          cadChanged <- TRUE
        }
        nCadStuck <- sum(zapcadence)
      }
    }
  }

  ######    cadence > 0 but not moving -
  cadzero <-  trackdf$cadence.rpm>0 & !is.na(trackdf$cadence.rpm) &
    trackdf$speed.m.s==0 &
    c(0,trackdf$speed.m.s[-nrow(trackdf)]) == 0
  if (sum(cadzero)>0) {
    if (loud) {
      if (cadCorrectStopped & fixCadence) cat("  zeroing")
      cat("  ",sum(cadzero)," positive cadence values while speed is 0\n")
      if (sum(cadzero)>0) {
        temp <- trackdf[cadzero,c("timestamp.s","speed.m.s","cadence.rpm",
                                  "distance.m")]
        print(temp,n=70,na.print="NA")
      }
    }
    if (cadCorrectStopped & fixCadence) {
      trackdf$cadence.rpm[cadzero] <- 0
      cadChanged <- TRUE
    }
  }
  nCadStoppedPos <- sum(cadzero)

  ######   Missing cadence values
  if (cadCorrectNA & fixCadence) {
    cadenceNA <- is.na(trackdf$cadence.rpm)
    if (sum(cadenceNA) > 0) {
      if (loud) cat("  fixing ",sum(cadenceNA)," missing cadence values")
      cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                            xvar=cumsum(trackdf$deltatime),
                                            segment=trackdf$segment,
                                            bw=cadCorrectWindowSec,
                                            nneighbors=cadCorrectWindowSec,
                                            kernel="triangular",
                                            replaceNAs=TRUE)
      trackdf$cadence.rpm[cadenceNA] <- cadenceSmoothed[cadenceNA]
      cadChanged <- TRUE
    }
  }
  if (!cadChanged) trackdf$cadence.uncorrected <- NULL
  return(list(trackdf=trackdf,nCadTooHigh=nCadTooHigh,nCadTooLow=nCadTooLow,
              nCadStoppedPos=nCadStoppedPos,nCadStuck=nCadStuck))
}
zerosinrange <- function(vec,begin,end) {
  if ((begin > end) | (begin < 1) | (end > length(vec))) {
    FALSE
  } else {
    t <- vec[begin:end] == rep(0,end-begin+1)
    t[is.na(t)] <- FALSE
    max(t) == 1
  }
}
