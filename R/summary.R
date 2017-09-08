#' summary data for track files read by bikeCadHr functions
#'
#' @details Data fields in the summary:
#'
#' \itemize{
#'  \item{date - the date of the ride}
#'  \item{start.time - time of the first track point after discarding false starts}
#'  \item{start.hour - time that track recording started, rounded to the quarter hour.
#'      This is useful for merging with other data sources with slightly misaligned clocks}
#'  \item{nwaypoints - number of waypoints in the track}
#'  \item{numsegs - number of segments in the track}
#'  \item{distance - the distance traveled.  for fit files this uses the
#'      wheel distance sensor if present. Otherwise it uses gps position changes.}
#'  \item{total.time - time from first start to final stop}
#'  \item{rolling.time - time while moving}
#'  \item{pedal.time - time with a nonzero, nonmissing cadence measure}
#'  \item{startline.time - time between starting track data recording and beginning motion}
#'  \item{pedal.strokes - estimate of pedal strokes based on stream of cadence values}
#'  \item{avgcadence.nozeros - average cadence excluding zeros}
#'  \item{avgcadence.withzeros - average cadence including zeros}
#'  \item{avgcadence.midsegment - average cadence excluding time before and after stops}
#'  \item{hr.at.stop - fit file only, heart rate at end of ride}
#'  \item{hr.recovery - fit file only, and only if you waited 2 minutes after hitting stop
#'     before you stop recording or reset counters, heart rate after 2 minute interval}
#'  \item{speed.rolling.m.s - average speed while moving in meters per second}
#'  \item{speed.all.m.s - average speed including stationary time}
#'  \item{speed.max.m.s - maximum speed}
#'  \item{ascent - vertical ascent estimated from smoothed elevations}
#'  \item{descent - vertical descent estimated from smoothed elevations}
#'  \item{distance.ascending - distance where smoothed climb exceeds 0.25 meters between samples}
#'  \item{distance.descending- distance where smoothed drop exceeds 0.25 meters between samples}
#'  \item{grade.ascending.steepest - not reliable - self-explanatory}
#'  \item{grade.descending.steepest - not reliable - self-explanatory}
#'  \item{pct.trkpts.hr - share of trackpoints with nonmissing heartrate data}
#'  \item{pct.trkpts.cad - share of trackpoints with nonmissing cadence data}
#'  \item{pct.low.gear - share of trackpoints ridden in the lowest gear}
#'  \item{low.gear2 - alternate value of speed/cadence ratio specified in gr.bound2}
#'  \item{pct.low.gear2 - share of trackpoints ridden in gear below gr.bound2}
#'  \item{low.gear - value of speed/cadence ratio corresponding to the lowest gear}
#'  \item{ride.loop - TRUE if ride endpoint is within 133 meters of startpoint}
#'  \item{stops.subminute - count of stops lasting less than 1 minute}
#'  \item{stops.1to10minutes - count of stops lasting between 1 and 10 minutes}
#'  \item{stops.10to30minutes - count of stops lasting between 10 and 30 minutes}
#'  \item{stops.long - count of stops longer than 30 minutes}
#'  \item{session.distance - fit file only - distance as calculated by GPS}
#'  \item{session.elapsed.time - fit file only - time as calculated by GPS}
#'  \item{session.timer.time - fit file only - time as calculated by GPS}
#'  \item{session.pedal.strokes - fit file only - pedal strokes as calculated by GPS}
#'  \item{session.total.calories - fit file only - kCals burned as calculated by GPS}
#'  \item{session.avg.speed - fit file only - speed as calculated by GPS}
#'  \item{session.max.speed - fit file only - speed as calculated by GPS}
#'  \item{session.total.ascent - fit file only - climb as calculated by GPS}
#'  \item{session.total.descent - fit file only - descent as calculated by GPS}
#'  \item{session.avg.cadence - fit file only - cadence as calculated by GPS, depending
#'     on options set at time of ride}
#'  \item{session.avg.hr -  - fit file only - average heart rate}
#'  \item{session.max.hr - fit file only - max heart rate}
#'  \item{sourcefile - basename of the file containing the source data}
#'  \item{processed.time - time that the file was read into the summary}
#'  \item{startbutton.date - integer containing human readable date stamp for ride,
#'     used to match summary and detailed track data, derived from the source
#'     filename (garmin by default names activity records based on the time that
#'     the start button is pressed).  The parameters used to parse the filename
#'     for the date are set using options().  If the name is not in a format that
#'     can be read, the first timestamp in the track data will be used.
#'     Data format is YYYYMMDD}
#'  \item{startbutton.time  - integer containing human readable time stamp for ride,
#'     used to match summary and detailed track data, derived from the source
#'     filename (garmin by default names activity records based on the time that
#'     the start button is pressed).  The parameters used to parse the filename
#'     for the date are set using options().  If the name is not in a format that
#'     can be read, the first timestamp in the track data will be used.
#'     Data format is  HHMMSS, where HH is 00-23, MM and SS are 00-59}
#'  }
#'
#'  \strong{(all distances are in meters, times are in seconds)}
#'
#'  \emph{Note that .gpx files use distance and speed calculated from GPS location
#'  coordinates, while .fit files will base these quantities on data from the
#'  speed/cadence sensor if present.  Expect, for example, distance to be shorter
#'  for a summary of data in a .gpx file than for data from the same ride and same
#'  gps extracted from the corresponding .fit file.  Even if all calculations are
#'  exact, and the gps has perfect accuracy, coordinate-based distances and speed
#'  will understate distance because corners get "cut off" by the sampling.
#'
#'  Speed and rolling time measures do not align perfectly between computers and/or
#'  GPS units becuase of differences in autopause begin/end delays, an effect which
#'  is more pronounced with more stops.  GPS data are measurements of events, and
#'  as such reflect sampling variation which is probably larger than you expect}
#'
#' @name summary data frame contents
#'
NULL
