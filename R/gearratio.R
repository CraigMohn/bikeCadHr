#' generate gear ratio statistics for a track
#'
#' \code{statsGearing}  processes a gps track file to summarize the gear ratio
#'   implied by the ratio of cadence to speed
#'
#' @param trackdf data frame or tibble with gps track data
#' @param grLow1 lowest gear speed/cadence upper bound
#' @param grLow2 alternate lowest gear speed/cadence upper bound
#' @param minGRCadence ignore where cadence is below this number
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
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
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsGearing <- function(trackdf,
                         grLow1=0.044,grLow2,
                         minGRCadence=40,...) {
  ## ratio of speed to cadence
  pctLowGear1 <- NA
  pctLowGear2 <- NA
  pedaling <- (!is.na(trackdf$cadence.rpm)) & (trackdf$cadence.rpm >= minGRCadence)
  if (sum(pedaling)>100) {
    gearratio <- trackdf$speed.m.s/trackdf$cadence.rpm
    gearratio[is.na(gearratio)|is.infinite(gearratio)] <- 10000
    inlowgear <- ( gearratio < grLow1 )
    pctLowGear1 <- sum(trackdf$deltatime[pedaling & inlowgear]) /
      sum(trackdf$deltatime[pedaling])
    if (!missing(grLow2)) {
      inlowgear <- ( gearratio < grLow2 )
      pctLowGear2 <- sum(trackdf$deltatime[pedaling & inlowgear]) /
        sum(trackdf$deltatime[pedaling])
    } else {
      grLow2 <- NA
    }
  }
  return(list(lowGear1=grLow1,
              lowGear2=grLow2,
              pctLowGear1=pctLowGear1,
              pctLowGear2=pctLowGear2))

}
