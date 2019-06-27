#' generate power statistics for a track
#'
#' \code{statsPower}  processes a gps track file to summarize the power data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param powerCalibrateTime number of seconds to ignore in avgpowerPostCal
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
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
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
statsPower <- function(trackdf,powerCalibrateTime=0,...) {
  powerpos <- !is.na(trackdf$power.watts) & trackdf$power.watts > 0
  powerNum <- sum(trackdf$power.watts[powerpos]*
                    trackdf$deltatime[powerpos]  )
  if (sum(powerpos)>0) {
    avgpowerNoZeros <- powerNum / totalTime(trackdf,powerpos)
    avgpowerWithZeros <- powerNum / rollingTime(trackdf,...)

    calibrate <- cumsum(trackdf$deltatime)<powerCalibrateTime
    if (sum(!calibrate)>sum(calibrate)) {
      powerNumpost <- sum(trackdf$power.watts[powerpos & !calibrate]*
                            trackdf$deltatime[powerpos & !calibrate]  )
      avgpowerPostCalNoZeros <-powerNumpost / totalTime(trackdf,
                                                        include=powerpos & !calibrate,
                                                        ...)
      avgpowerPostCalRolling <-powerNumpost / rollingTime(trackdf,
                                                          include=!calibrate,
                                                          ...)
    } else {
      avgpowerPostCalNoZeros <- NA
      avgpowerPostCalRolling <- NA
    }
  } else {
    avgpowerNoZeros <- NA
    avgpowerWithZeros <- NA
    avgpowerPostCalNoZeros <- NA
    avgpowerPostCalRolling <- NA
  }
  return(list(avgpowerNoZeros=avgpowerNoZeros,
              avgpowerWithZeros=avgpowerWithZeros,
              avgpowerPostCalNoZeros=avgpowerPostCalNoZeros,
              avgpowerPostCalRolling=avgpowerPostCalRolling))
}
