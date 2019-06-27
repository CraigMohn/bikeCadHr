##' return the total time for a track
#'
#' \code{totalTime}  return the total time in a track
#'
#' @param trackdf data frame or tibble with gps track data
#' @param include a vector specifying which points on the track tyop include
#' @param ... parameters for other functions
#'
#' @return a vector of length one containing the elapsed time on the track for which
#'    include is TRUE
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{rollingTime}},
#'    \code{\link{pedalTime}}
#'
#' @export
totalTime <- function(trackdf,include,...) {
  if (missing(include)) include <- TRUE
  return(sum(trackdf$deltatime[include]))
}
#' return moving time for a track
#'
#' \code{rollingTime}  return the total time in a track
#'
#' @param trackdf data frame or tibble with gps track data
#' @param stopSpeed a numeric value below which the bike is considered stopped
#' @param include a vector specifying which points on the track to include
#' @param ... parameters for other functions
#'
#' @return a vector of length one containing the elapsed time on the track for which
#'    include is TRUE
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{totalTime}},
#'    \code{\link{pedalTime}}
#'
#' @export
rollingTime <- function(trackdf,stopSpeed=0.0,include,...) {
  if (missing(include)) include <- TRUE
  return(sum(trackdf$deltatime[include & isMoving(trackdf,stopSpeed)]))
}
#' return pedaling time for a track
#'
#' \code{pedalTime}  return the total time pedaling in a track
#'
#' @param trackdf data frame or tibble with gps track data
#' @param minCadence a numeric value below which the cadence is ignored
#' @param cadRequireRolling if TRUE, cadence value ignored if bike not moving
#' @param stopSpeed a numeric value below which the bike is considered stopped
#' @param cadMinMove a numeric value specifying the minimum duration of motion
#'    to be included in pedaled time
#' @param cadStartMaxDelay a numeric value specifying the number of seconds
#'    after beginning motion that pedaling is assumed for time (but not pedal
#'    stroke accumulation).  This accounts for sensor startup lag.
#' @param include a vector specifying which points on the track to include
#' @param ... parameters for other functions
#'
#' @return a vector of length one containing the elapsed time on the track for which
#'    include is TRUE
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{totalTime}},
#'    \code{\link{rollingTime}}
#'
#' @export
pedalTime <- function(trackdf,
                      minCadence=0,
                      cadRequireRolling=TRUE,
                      stopSpeed=0.0,cadMinMove=3,
                      cadStartMaxDelay=3,
                      include,...) {
  if (missing(include)) include <- TRUE
  #  pedaling if moving and cadence is positive or when starting from a stop
  pedaling <- !is.na(trackdf$cadence.rpm) & trackdf$cadence.rpm > minCadence

  rolling <- isMovingLonger(trackdf,stopSpeed,cadMinMove)

  if (!is.na(cadStartMaxDelay) & cadStartMaxDelay > 0) {
    pedaling <- pedaling |
      beginMoving(rolling,
                  timestamp=trackdf$timestamp.s,
                  cadStartMaxDelay)
  }
  if (cadRequireRolling) {
    pedaling <- pedaling & rolling
  }
  return(sum(trackdf$deltatime[include & pedaling]))
}
isMoving <- function(trackdf,stopSpeed) {
  return(!is.na(trackdf$speed.m.s) & (trackdf$speed.m.s > stopSpeed))
}
isMovingLonger <- function(trackdf,stopSpeed,cadMinMove) {
  moving <- rle(isMoving(trackdf,stopSpeed))
  if (!is.na(cadMinMove) & cadMinMove>0) {
    stops <- cumsum(moving[["lengths"]])
    starts <- 1 + c(0,stops[-length(stops)])
    elapsed <- trackdf$timestamp.s[stops] - trackdf$timestamp.s[starts]
    moving[["values"]] <- moving[["values"]] & (elapsed >= cadMinMove)
  }
  return(inverse.rle(moving))
}
beginMoving <- function(rolling,timestamp,cadStartMaxDelay) {
  begmoves <- rep(FALSE,length(rolling))
  r <- rle(rolling)
  beginruns <- 1 + c(0,cumsum(r[["lengths"]])[-length(r[["lengths"]])])
  beginroll <- beginruns[r[["values"]]]
  if (length(beginroll)>0 & cadStartMaxDelay >= 0) {
    begtimes <- timestamp[beginroll]
    for (t in begtimes) {
      begmoves[(timestamp - t >= 0) &
                 (timestamp - t <= cadStartMaxDelay )] <- TRUE
    }
  }
  return(begmoves)
}
