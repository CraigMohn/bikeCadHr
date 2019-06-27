#' generate grade statistics for a track
#'
#' \code{statsGrade}  processes a gps track file to summarize grade data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param gradeSmoothWindowNN number of observations on each side used for
#'    calculating kernel smoothed elevation for the grade calculation
#' @param gradeSmoothWindowMeters bandwidth (meters) for kernel smoothing of
#'    elevation in the grade calculation
#' @param flatLimit if the grade does not exceed this, it is neither scending
#'    nor descending
#' @param gradeMinDistance if the distance (meters) between points is less than
#'    this,the distance will not be counted aas ascending or descending and the
#'    grade will not be calculated
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
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
#'    \code{\link{statsGearing}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsGrade <- function(trackdf,
                       gradeSmoothWindowMeters=7,
                       gradeSmoothWindowNN=10,
                       flatLimit=0.01,
                       gradeMinDistance=0.50,...) {

  deltaelev <- trackdf$altitude.m - lag_one(trackdf$altitude.m)
  elevSmoothed <- smoothDataSegments(yvec=trackdf$altitude.m,
                                     xvar=trackdf$distance.m,
                                     segment=trackdf$segment,
                                     bw=gradeSmoothWindowMeters,
                                     nneighbors=gradeSmoothWindowNN,
                                     kernel="epanechnikov",
                                     replaceNAs=TRUE)
  deltaelevSmoothed <- elevSmoothed - lag_one(elevSmoothed)
  trackdeltadistance <- trackdf$distance.m - lag_one(trackdf$distance.m)

  trackgrade <- deltaelevSmoothed/trackdeltadistance
  ascending <- trackgrade > flatLimit
  descending <- trackgrade < -flatLimit
  trackgrade[trackdeltadistance<gradeMinDistance] <- NA
  ascending[trackdeltadistance<gradeMinDistance] <- FALSE
  descending[trackdeltadistance<gradeMinDistance] <- FALSE

  ascent  <- sum(deltaelevSmoothed[deltaelevSmoothed>0])
  descent <- sum(deltaelevSmoothed[deltaelevSmoothed<0])
  #distanceAscending  <- sum(trackdeltadistance[deltaelev >  0.25])
  #distanceDescending <- sum(trackdeltadistance[deltaelev < -0.25])
  distanceAscending  <- sum(trackdeltadistance[ascending])
  distanceDescending <- sum(trackdeltadistance[descending])

  return(list(ascent=ascent,descent=descent,
              distanceAscending=distanceAscending,
              distanceDescending=distanceDescending))
}
