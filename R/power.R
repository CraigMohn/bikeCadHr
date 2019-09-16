#' generate power statistics for a track
#'
#' \code{statsPower}  processes a gps track file to summarize the power data
#'
#' @param trackdf data frame or tibble with gps track data
#'   powerCalibrateTime <= 0
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{excludeCalibrate}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsPower <- function(trackdf,...) {

  avgPowers <- powerAvgs(trackdf,...)
  return(list(avgpowerNoZeros=avgPowers[["avgpowerNoZeros"]],
              avgpowerWithZeros=avgPowers[["avgpowerWithZeros"]]))
}
powerAvgs <- function(trackdf,...) {
  powerpos <- !is.na(trackdf$power.watts) & trackdf$power.watts > 0
  powerNum <- sum(trackdf$power.watts[powerpos]*
                    trackdf$deltatimestart[powerpos]  )
  if (sum(powerpos)>0) {
    avgpowerNoZeros <- powerNum / totalTime(trackdf,powerpos)
    avgpowerWithZeros <- powerNum / rollingTime(trackdf,...)

  } else {
    avgpowerNoZeros <- NA
    avgpowerWithZeros <- NA
  }
  return(list(avgpowerNoZeros=avgpowerNoZeros,
              avgpowerWithZeros=avgpowerWithZeros))
}

#' clean up power data for a track
#'
#' \code{repairPower}  processes a gps track file to correct and
#'   summarize power data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param loud display actions taken
#' @param ... parameters for \code{\link{excludeCalibrate}},
#'    \code{\link{repairHR}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return dataframe with power data repaired
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsCadence}}
#'
#' @export
repairPower <- function(trackdf,loud=FALSE,...) {

  powerfixed <- excludeCalibrate(trackdf$deltatime,
                                 trackdf$power.watts,...)
  if (powerfixed[["calSeqs"]] > 0){
    trackdf$power.uncorrected <- trackdf$power.watts
    trackdf$power.watts < -powerfixed[["power"]]
  }
  return(list(trackdf=trackdf,nCalibrateSequences=powerfixed[["calSeqs"]]))
}
#' exclude powerpod calibration sequence
#'
#' \code{excludeCalibrate}  processes a gps track file to summarize the power data
#'
#' @param deltatime timestamp vector
#' @param watts power vector
#' @param powerCalibrateAuto automatically find calibration signals if
#' @param powerCalibrateTime number of seconds to ignore in avgpower PostCal
#' @param afterlast zap all power readings before last calibration
#' @param maxCalDelta maximum stepsize in increasing sequence
#' @param maxCalWatts power value at end of calibration
#' @param maxminCalWatts largest start value for increasing sequence
#' @param minmaxCalWatts smallest end value for increasing sequence
#' @param maxCalBurstSecs window to look for huge values after sequence
#' @param minBurstSize maximum realistic power reading for burst elimination
#' @param loud print information about corrections
#' @param ... arguments to other functions
#'
#' @return a vector containing cleeaned power
#'
#' @seealso \code{\link{read_ride}},\code{\link{excludeCalibrate}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
excludeCalibrate <- function(deltatime,watts,afterlast=TRUE,
                             powerCalibrateAuto=TRUE,powerCalibrateTime=NA,
                             maxCalDelta=3,maxCalWatts=100,
                             maxminCalWatts=3,minmaxCalWatts=98,
                             maxCalBurstSecs=5,minBurstSize=2000,
                             loud=FALSE,...) {
  outwatts <- watts
  numSeqs <- 0
  if (sum(!is.na(watts)) > 0) {
    clock <- cumsum(deltatime)
    #  work on zeros removed and NAs removed - times will be the same
    idxpos <- !is.na(watts) & (watts > 0)
    pwatts <- watts[idxpos]
    pclock <- clock[idxpos]
    laggedpWatts <- c(0,pwatts[-length(pwatts)])
    leadpWatts <- c(pwatts[-1],NA)
    inRun <- (pwatts <= maxCalWatts) &
      (((pwatts <= leadpWatts) & (pwatts + maxCalDelta >= leadpWatts)) |
         ((pwatts >= minmaxCalWatts) & (pwatts >= laggedpWatts)))
    inRun[is.na(inRun)] <- TRUE
    runs <- rle(inRun)
    allstops <- cumsum(runs[["lengths"]])
    allstarts <- c(0,allstops[-length(allstops)]) + 1
    runStops <- allstops[runs[["values"]]]
    runStarts <- allstarts[runs[["values"]]]
    if (length(runStarts) > 0) {
      runMin <- pwatts[runStarts]
      runMax <- pwatts[runStops]
      goodRun <- runMin <= maxminCalWatts &
        runMax >= minmaxCalWatts
      runStarts <- runStarts[goodRun]
      runStops <- runStops[goodRun]
    }
    numSeqs <- length(runStarts)
    if (length(runStarts)>0) {
      if (loud) cat("***##*** calibration","\n")
      changed <- TRUE
      if (length(runStarts)>1 ) {
        cat("****** multiple calibration sequences found ******","\n")
      }
      if (loud) cat("  beginning time = ",pclock[runStarts],"\n")
      if (loud) cat("  ending time    = ",pclock[runStops],"\n")
      if (afterlast) {
        endtime <- pclock[runStops[length(runStops)]]
        outwatts[clock <= endtime] <- NA
        burst <- clock <= (endtime + maxCalBurstSecs) &
          !is.na(outwatts) & outwatts >= minBurstSize
        outwatts[burst] <- NA
        if (sum(burst)>0)
          if (loud) cat("  dropping ",sum(burst)," post-cal burst values\n")
      } else {
        for (i in 1:length(runStarts)) {
          outwatts[clock >= pclock[runStarts[i]] &
                     clock <= pclock[runStops[i]]] <- NA
          burst <- clock >= (pclock[runStops[i]]) &
            clock <= (pclock[runStops[i]] + maxCalBurstSecs) &
            !is.na(outwatts) & outwatts >= minBurstSize
          outwatts[burst] <- NA
          if (sum(burst)>0)
            if (loud) cat("  dropping ",sum(burst)," post-cal burst values\n")
        }
      }
    }
  }
  return(list(power=outwatts,calSeqs=numSeqs))
}

