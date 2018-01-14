#' read multiple gps data files
#'
#' \code{read_ridefiles} reads a vector of gps .fit and/or .gpx track files
#'  and put into summary and detail data tibbles for modeling or graphical
#'  summary.  A wrapper for \code{\link{read_ride}}.
#'
#'
#' @param ridefilevec a vector of filenames to process
#' @param cores number of cores (default is #CPUs - 1)
#' @param loud display summary of re/segmenting actions
#' @param ... parameters passed for track cleaning
#'
#' @return a list of two data tibbles:  \eqn{summaries} and \eqn{tracks}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
read_ridefiles <- function(ridefilevec,cores=4,loud=loud,...)  {
  nfiles <- length(ridefilevec)
  if(missing(cores)) cores <- parallel::detectCores()
  if ((nfiles > 10) & !is.na(cores) & (cores>1)) {
    doParallel::registerDoParallel(cores)
    `%dopar%` <- foreach::`%dopar%`
    cfun <- function(a,b) list(summary=dplyr::bind_rows(a[["summary"]],b[["summary"]]),
                               trackpoints=dplyr::bind_rows(a[["trackpoints"]],b[["trackpoints"]]))
    ridelist <- foreach (x = ridefilevec,.combine=`cfun`,
                        .packages=c("bikeCadHr")) %dopar% {
      read_ride(x,loud=loud,...)
    }
    doParallel::stopImplicitCluster()
    return(list(summaries=dplyr::arrange(ridelist[["summary"]],date,start.hour),
                tracks=dplyr::arrange(ridelist[["trackpoints"]],startbutton.date,timestamp.s)))
  } else {
    outdf <- NULL
    outtracks <- NULL
    for (x in ridefilevec) {
      ride <- read_ride(x,loud=loud,...)
      obsdf <- ride[["summary"]]
      obstrack <- ride[["trackpoints"]]
      if (is.null(outdf)) {
        outdf <- obsdf
      } else {
        outdf <- dplyr::bind_rows(outdf,obsdf)
      }
      if (is.null(outtracks)) {
        outtracks <- obstrack
      } else {
        outtracks <- dplyr::bind_rows(outtracks,obstrack)
      }
    }
    return(list(summaries=dplyr::arrange(outdf,date,start.hour),
                tracks=dplyr::arrange(outtracks,startbutton.date,timestamp.s)))
  }
}
#' read and clean a gps data file
#'
#' \code{read_rideold}  processes a gps .fit and/or .gpx track file to create
#'   summary and detail data tibbles for modeling or graphical summary.  Track
#'   segments are adjusted to remove many false autostop-start and startline
#'   delay sequences.  Several measures of cadence are calculated (including
#'   zeros, excluding zeros, and midsegment),spurious and missing cadence values
#'   are repaired, and summary stats are calculated for analysis.
#'
#' @param ridefile a filenames (.fit or .gpx extension) to process
#' @param tz string contaiing timezone for converting from UTC
#' @param subtrack.initial.idle.agg.secs stops/restarts before this time has
#'    elapsed are discarded and the ride starts at the last start prior to this
#' @param subtrack.initial.idle.agg.meters stops/restarts within this distance
#'    of the initial start are discarded as above
#' @param min.segment.break.time (seconds) segments separated by less time
#'    than this are joined
#' @param max.interval.time (seconds) tracks are split between datapoints
#'    separated by more time than this
#' @param min.segment.obs segments smaller than this are joined to closest
#'    neighboring segment
#' @param min.segment.dist (meters) segments shorter than this are joined
#' @param min.segment.time (seconds) segments shorter than this are joined
#' @param short.segment.merge.timegap.max (seconds) short segments are joined
#'    if interval between closest is no longer than this time
#' @param short.seg.delete delete short segments if no other segment
#'    within the max tim gap above
#' @param short.seg.cadence.delete.na.threshold delete short segments even
#'    if \eqn{short.seg.delete}=FALSE if share of cadence NAs in segment exceeds this
#' @param cadence.trim.beg.secs number of seconds after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadence.trim.end.secs number of seconds before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadence.max max credible cadence value, larger values are errors
#' @param cadence.correct.errors repair excessive cadence values
#'    using triangular-kernel-weighted average of the nearest nonmissing values
#'    in the same segment
#' @param cadence.correct.nas repair cadence missing values using
#'    triangular-kernel-weighted average of the nearest nonmissing values in
#'    the same segment
#' @param cadence.correct.window window size for kernel smoothing of cadence
#' @param gr.bound gear ratio (speed divided by cadence) cutoff for classifying
#'    as lowest gear.  default calculated based on parameters
#' @param gr.bound2 alternate cutoff for classifying
#'    as lowest gear
#' @param gr.low lowest gear ratio (speed divided by cadence)
#' @param gr.high highest gear ratio (speed divided by cadence)
#' @param gr.peak.m window for identifying peaks in gear ratio density
#' @param gr.nlowgear number of gears lumped into pct.low.gear statistic
#' @param gr.kernel kernel to use in gear ratio density estimation
#' @param gr.adjust multiplier for default density() bandwidth
#' @param gr.density.threshold threshold to keep density peak, as a
#'    share of the largest peak density
#' @param grade.smooth.window number of observations on each side used for
#'    calculating kernel smoothed elevation for the grade calculation
#' @param grade.smooth.bw bandwidth (meters distance) for kernel smoothing of elevation
#'    in the grade calculation
#' @param grade.smooth.percentiles percentiles of elevations to use
#'    as steepest grades
#' @param cadence.time.shift.start do not use, alter assumptions about gps sample recording
#' @param last.interval.end.notrigger do not use, alter assumptions about gps sample recording
#' @param fixDistance repair nonmonotonicities in distance which are on segment breaks -
#'    this occurs when power is lost or on some device lockups
#' @param ... parameters for \code{\link{processSegments}}
#'
#' @return a list of three data tibbles:  \eqn{summary} and \eqn{trackpoints} and
#'    \eqn{session}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ridefiles}}
#'
#' @export
read_rideold <- function(ridefile,tz="America/Los_Angeles",
          subtrack.initial.idle.agg.secs=20,subtrack.initial.idle.agg.meters=15,
          min.segment.break.time=3,max.interval.time=30,
          min.segment.obs=2,min.segment.dist=10,min.segment.time=5,
          short.segment.merge.timegap.max=4,
          short.seg.delete=FALSE,short.seg.cadence.delete.na.threshold=0.5,
          fixDistance=FALSE,
          cadence.time.shift.start=0,last.interval.end.notrigger=FALSE,
          cadence.trim.beg.secs=30,cadence.trim.end.secs=30,
          cadence.max=160,cadence.correct.window=7,
          cadence.correct.errors=TRUE,cadence.correct.nas=FALSE,
          gr.bound=NULL,gr.bound2=NULL,gr.low=0.0378,gr.high=0.1855,gr.peak.m=5,gr.nlowgear=1,
          gr.kernel="epanechnikov",gr.adjust=0.02,gr.density.threshold=0.05,
          grade.smooth.window=7,grade.smooth.bw=10,
          grade.smooth.percentiles=c(0.005,.98),...)  {

  cat("reading: ",ridefile,"\n")
  if (missing(tz)) {
    tz <- Sys.timezone()
  }
  if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".fit") {
    temp <- read_fittrack(ridefile)
    time.fn.string <- basename(ridefile)
    fit.fn.time.parse <- getOption("bCadHr.fit.fn.time.parse")
    fit.fn.lead <- getOption("bCadHr.fit.fn.lead")
    fit.fn.trail <- getOption("bCadHr.fit.fn.trail")
    if (nchar(fit.fn.lead)>0) time.fn.string <-
        substr(time.fn.string,regexpr(fit.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(fit.fn.trail)>0) time.fn.string <-
        substr(time.fn.string,1,regexpr(fit.fn.trail,time.fn.string))
    time.turned.on <- strptime(time.fn.string,fit.fn.time.parse,tz=tz)
  } else if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".gpx") {
    temp <- read_gpxtrack(ridefile)
    time.fn.string <- basename(ridefile)
    gpx.fn.time.parse <- getOption("bCadHr.gpx.fn.time.parse")
    gpx.fn.lead <- getOption("bCadHr.gpx.fn.lead")
    gpx.fn.trail <- getOption("bCadHr.gpx.fn.trail")
    if (nchar(gpx.fn.lead)>0) time.fn.string <-
      substr(time.fn.string,regexpr(gpx.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(gpx.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(gpx.fn.trail,time.fn.string))
    time.turned.on <- strptime(time.fn.string,gpx.fn.time.parse,tz=tz)
  } else {
    stop("unknown file extension")
  }
  trackdata <- temp[["track"]]
  recovery_hr <- temp[["recovery_hr"]]
  session <- temp[["session"]]

  attr(trackdata$timestamp.s,"tzone") <- tz
  if (is.na(time.turned.on)){
    #  if filename was not successfully turned into a start-button time, use first
    #   timestamp value recorded in the track.  Likely but not certain to match between
    #   different versions of the same ride
    time.turned.on <- trackdata$timestamp.s[1]
  }
  startbutton.date <- as.integer(lubridate::mday(time.turned.on)+
                             100*lubridate::month(time.turned.on)+
                           10000*lubridate::year(time.turned.on))
  startbutton.time <- as.integer(lubridate::second(time.turned.on)+
                             100*lubridate::minute(time.turned.on)+
                           10000*lubridate::hour(time.turned.on))
  trackdata$startbutton.date <- startbutton.date
  trackdata$startbutton.time <- startbutton.time
  ###   minimal data validity checks after dumping unusable obs
  trackdata <- stripDupTrackRecords(trackdata,fixDistance=fixDistance)
  if (is.unsorted(trackdata$segment,strictly=FALSE))
    stop(paste0(ridefile," yields segment ids that are not nondecreasing!"))
  if (is.unsorted(trackdata$distance.m,strictly=FALSE))
    stop(paste0(ridefile," yields distances that are not nondecreasing!"))
  if (is.unsorted(trackdata$timestamp.s,strictly=TRUE))
    stop(paste0(ridefile," yields timestamps that are not strictly increasing!"))
  ttrackdata <- processSegments(trackdf=trackdata,loud=TRUE)

  # split really, really long intervals between points
  first.seg <- trackdata$segment != lag_one(trackdata$segment)
  long.interval <-  as.numeric(difftime(trackdata$timestamp.s,lag_one(trackdata$timestamp.s),units="secs")) >
                        max.interval.time &
                    !first.seg
  if (sum(long.interval)>0) {
    trackdata$segment <- trackdata$segment + cumsum(long.interval)
  }

  #  summary stats based on all usable nontrivial data
  ride.date <- as.Date(format(trackdata$timestamp.s[1],tz=tz))
  start.time <- as.character(trackdata$timestamp.s[1])
  start.hour <- as.numeric(lubridate::hour(format(time.turned.on,tz=tz))
           + 0.25*(round((60*lubridate::minute(format(time.turned.on,tz=tz))+
                          lubridate::second(format(time.turned.on,tz=tz)))/(60*15))))
  nwaypoints <- nrow(trackdata)  #  this will be recalculated, but is appropriate for these summary stats
  pct.trkpts.hr <- sum(!is.na(trackdata$heart_rate.bpm))/nwaypoints
  pct.trkpts.cad <- sum(!is.na(trackdata$cadence.rpm))/nwaypoints

  ##   collapse (usually spurious) initial start/stops before ride really begins, and accumulate startline time and startline distance offset
  track.start <- (as.numeric(difftime(trackdata$timestamp.s,trackdata$timestamp.s[1],units="secs")) < subtrack.initial.idle.agg.secs) |
     		 (trackdata$distance.m < subtrack.initial.idle.agg.meters)
  startline.time <- 0.0
  startline.distance <- 0.0
  if (sum(track.start) > 0) {
    drop.obs <- trackdata$segment < max(trackdata$segment[track.start])
    if (sum(drop.obs) > 0) {
      first.seg <- trackdata$segment != lag_one(trackdata$segment)
      first.seg[1] <- TRUE  #  there should be exactly one obs in the "startline set" of close points which is both not dropped and first-in-seg
      startline.time <- as.numeric(difftime(trackdata$timestamp.s[first.seg&track.start&!drop.obs],trackdata$timestamp.s[1],units="secs"))
      startline.distance <- trackdata$distance.m[first.seg&track.start&!drop.obs]
      trackdata$segment[!track.start] <- trackdata$segment[!track.start] + 1 - max(trackdata$segment[track.start])
      trackdata$segment[track.start] <- 1
      trackdata <- trackdata[!drop.obs,]
    }
  }
  ##   join segments where separating breaks are less than minimum time
  trackdeltatime <- as.numeric(difftime(trackdata$timestamp.s,lag_one(trackdata$timestamp.s),units="secs"))
  trackdeltanexttime <- as.numeric(difftime(trackdata$timestamp.s,lead_one(trackdata$timestamp.s),units="secs"))
  trackdeltadist <- as.numeric(trackdata$distance.m,lag_one(trackdata$distance.m))
  first.seg <- trackdata$segment != lag_one(trackdata$segment)   # this does not flag the first start, which is fine
  first.nomin <- first.seg & (trackdata$segment != 1) & (trackdeltatime < min.segment.break.time)
  if (sum(first.nomin) > 0) {
    newsegnum <- trackdata$segment - cumsum(first.nomin)
    trackdata$segment <- newsegnum
    #  patch missing cadence in midblock later
  }
  ##  segments with too few samples, too short in time or distance
  ##
  first.seg <- trackdata$segment != lag_one(trackdata$segment)   # this does not flag the first start
  first.seg[1] <- TRUE
  last.seg <- trackdata$segment != lead_one(trackdata$segment)   # this does not flag the last stop
  last.seg[nrow(trackdata)] <- TRUE
  first.short <- first.seg & (trackdata$segment != lead_n(trackdata$segment,min.segment.obs))
  first.short[is.na(first.short)] <- FALSE
  seg.start.m <- trackdata$distance.m[first.seg]
  seg.stop.m <- trackdata$distance.m[last.seg]
  seg.length.m <- seg.stop.m - seg.start.m
  seg.start.s <- trackdata$timestamp.s[first.seg]
  seg.stop.s <- trackdata$timestamp.s[last.seg]
  seg.length.s <- as.numeric(difftime(seg.stop.s,seg.start.s,units="secs"))
  gap.prior <- trackdeltatime[first.seg]
  gap.prior[1] <- Inf
  gap.post <- trackdeltanexttime[last.seg]
  gap.post[1] <- Inf
  segs.na.share <- sapply(1:length(seg.start.m),
                          function(x) sum(is.na(trackdata$cadence.rpm)&trackdata$segment==x)/sum(trackdata$segment==x))
  segs.too.short <- seg.length.m < min.segment.dist | seg.length.s < min.segment.time | first.short[first.seg]
  segs.merge.prior  <- segs.too.short & (gap.prior < gap.post) & (gap.prior < short.segment.merge.timegap.max)
  segs.merge.post   <- segs.too.short & (gap.prior >= gap.post) & (gap.post  < short.segment.merge.timegap.max)
  segs.merge.delete <- segs.too.short & !(segs.merge.prior | segs.merge.post) &
                            (short.seg.delete | segs.na.share > short.seg.cadence.delete.na.threshold)

  if (length(c(segs.merge.prior,segs.merge.post,segs.merge.delete))>0) {
     first.merged.dropped <- first.seg & trackdata$segment %in% c(segs.merge.prior,segs.merge.post,segs.merge.delete)
     newsegnum <- trackdata$segment - cumsum(first.merged.dropped) + as.numeric(trackdata$segment %in% segs.merge.post)
     dropobs <- trackdata$segment %in% segs.merge.delete
     trackdata$segment <- newsegnum
     trackdata <- trackdata[!dropobs,]

  }

  ### set first,last and middle flags after all segment clean-up is done, and set up final quantities for calulations ######
  nwaypoints <- nrow(trackdata)
  nsegments <- max(trackdata$segment)
  first.seg <- trackdata$segment != lag_one(trackdata$segment)
  first.seg[1] <- TRUE
  last.seg <- trackdata$segment != lead_one(trackdata$segment)
  last.seg[nwaypoints] <- TRUE
  middle.of.seg <- !(first.seg | last.seg)
  start.times <- trackdata$timestamp.s[first.seg]
  stop.times <- trackdata$timestamp.s[last.seg]
  trackdeltatime <- as.numeric(difftime(trackdata$timestamp.s,lag_one(trackdata$timestamp.s),units="secs"))
  trackdeltaelev <- trackdata$altitude.m - lag_one(trackdata$altitude.m)
  trackdeltadistance <- trackdata$distance.m - lag_one(trackdata$distance.m)
  time.from.start <- cumsum(trackdeltatime)
  elev.smoothed <- smoothDataSegments(yvec=trackdata$altitude.m,
                                      xvar=trackdata$distance.m,
                                      segment=trackdata$segment,
                                      bw=grade.smooth.bw,
                                      nneighbors=grade.smooth.window,
                                      kernel="epanechnikov")
  deltaelev.smoothed <- elev.smoothed - lag_one(elev.smoothed)
  trackgrade <- deltaelev.smoothed/trackdeltadistance
  trackgrade[trackdeltadistance<1] <- NA
  trackgrade.tails <- stats::quantile(trackgrade,probs=grade.smooth.percentiles,na.rm=TRUE)

  ## ratio of speed to cadence
  trackdata$gearratio <- trackdata$speed.m.s/trackdata$cadence.rpm

  ## patch cadence errors and then NAs
  cadence.error <- trackdata$cadence.rpm > cadence.max
  cadence.error[is.na(cadence.error)] <- FALSE
  if (sum(cadence.error,na.rm=TRUE) > 0) {
    if (cadence.correct.errors) {
      trackdata$cadence.rpm[cadence.error] <- NA
      cadence.smoothed <- smoothDataSegments(yvec=trackdata$cadence.rpm,
                                             xvar=time.from.start,
                                             segment=trackdata$segment,
                                             bw=cadence.correct.window,
                                             nneighbors=cadence.correct.window,
                                             kernel="triangular")
      trackdata$cadence.rpm[cadence.error] <- cadence.smoothed[cadence.error]
    } else {
      trackdata$cadence.rpm[cadence.error] <- NA
    }
  }
  if (cadence.correct.nas) {
    cadence.na <- is.na(trackdata$cadence.rpm)
    if (sum(cadence.na,na.rm=TRUE) > 0) {
      cadence.smoothed <- smoothDataSegments(yvec=trackdata$cadence.rpm,
                                             xvar=time.from.start,
                                             segment=trackdata$segment,
                                             bw=cadence.correct.window,
                                             nneighbors=cadence.correct.window,
                                             kernel="triangular")
      trackdata$cadence.rpm[cadence.na] <- cadence.smoothed[cadence.na]
    }
  }
  cadence.pos.index <- !is.na(trackdata$cadence.rpm) & trackdata$cadence.rpm>0
  trimmed.cadence <- rep(NA,nwaypoints)
  if (sum(cadence.pos.index) > 0) {
    for (segment in 1:nsegments) {
      innersegment <-
        (trackdata$timestamp.s >= start.times[segment]+lubridate::seconds(cadence.trim.beg.secs)) &
        (trackdata$timestamp.s <   stop.times[segment]-lubridate::seconds(cadence.trim.end.secs))
      innersegment[is.na(innersegment)] <- 0
      if (sum(innersegment)>0)
          trimmed.cadence[innersegment] <- trackdata$cadence.rpm[innersegment]
    }
  }

  ## times for constructing weights on cadences and summarizing the ride
  idletime <- rep(0,nwaypoints)
  idletime[first.seg] <- trackdeltatime[first.seg] - cadence.time.shift.start
  idletime[last.seg] <- cadence.time.shift.start

  rollingtime <- rep(0,nwaypoints)
  rollingtime[first.seg] <- cadence.time.shift.start
  rollingtime[middle.of.seg] <- trackdeltatime[middle.of.seg]
  #  use last interval if recording is triggered by stop, half if stop recorded at scheduled time
  if (last.interval.end.notrigger) {
    rollingtime[last.seg] <- trackdeltatime[last.seg]/2
  } else {
    rollingtime[last.seg] <- trackdeltatime[last.seg] - cadence.time.shift.start
  }

  pedaltime <- rep(0,nwaypoints)
  pedaltime[first.seg&cadence.pos.index] <- cadence.time.shift.start
  pedaltime[middle.of.seg&cadence.pos.index] <- trackdeltatime[middle.of.seg&cadence.pos.index]
  #  use last interval if recording is triggered by stop, half if stop recorded at scheduled time
  if (last.interval.end.notrigger) {
    pedaltime[last.seg&cadence.pos.index] <- trackdeltatime[last.seg&cadence.pos.index]/2
  } else {
    pedaltime[last.seg&cadence.pos.index] <- trackdeltatime[last.seg&cadence.pos.index] - cadence.time.shift.start
  }
  pedaltime[pedaltime<0.0] <- 0.0

  trackpedalstrokes <- (pedaltime/60)*trackdata$cadence.rpm
  trackpedalstrokes[pedaltime==0] <- 0.0

  nwaypoints <- nwaypoints
  numsegs <- nsegments
  startline.time <- startline.time
  total.time <- as.numeric(difftime(trackdata$timestamp.s[nwaypoints],trackdata$timestamp.s[1],units="secs"))
  rolling.time <- sum(rollingtime)
  pedal.time <- sum(pedaltime)
  ped.strks <- sum(trackpedalstrokes[!is.na(trackdata$cadence.rpm)])
  ped.strks.na <- ped.strks
  ped.strks.na[ped.strks.na==0] <- NA
  pedal.strokes <- ped.strks
  distance <- trackdata$distance.m[nwaypoints]-startline.distance
  avgcadence.nozeros <- 60*ped.strks.na/sum(pedaltime)
  avgcadence.withzeros <- 60*ped.strks.na/sum(rollingtime[!is.na(trackdata$cadence.rpm)])
  avgcadence.midsegment <- mean(trimmed.cadence,na.rm=TRUE)
  hr.at.stop <- NA
  if (any(!is.na(trackdata$heart_rate.bpm))) {
    last.hr <- max(which(!is.na(trackdata$heart_rate.bpm)))
    if (as.numeric(difftime(trackdata$timestamp.s[nwaypoints],trackdata$timestamp.s[last.hr],units="secs")) < 10) {
      hr.at.stop <- as.numeric(trackdata$heart_rate.bpm[last.hr])
    }
  }
  if (!is.null(nrow(recovery_hr))) {
    if (nrow(recovery_hr)>0) {
      hr.recovery <- as.numeric(recovery_hr[1,"heart_rate.postride"])
    } else {
      hr.recovery <- NA
    }
  } else {
    hr.recovery <- NA
  }
  speed.rolling.m.s <- distance/rolling.time
  speed.all.m.s <- distance/total.time
  speed.max.m.s <- max(trackdata$speed.m.s,na.rm=TRUE)
  ascent  <- sum(trackdeltaelev[trackdeltaelev>0])
  descent <- sum(trackdeltaelev[trackdeltaelev<0])
  distance.ascending  <- sum(trackdeltadistance[trackdeltaelev >  0.25])
  distance.descending <- sum(trackdeltadistance[trackdeltaelev < -0.25])
  grade.ascending.steepest <- stats::median(trackgrade[!is.na(trackgrade)&(trackgrade>=trackgrade.tails[2])])
  grade.descending.steepest <- stats::median(trackgrade[!is.na(trackgrade)&(trackgrade<=trackgrade.tails[1])])

  break.time <- trackdeltatime[first.seg]
  stops.subminute <- length(break.time[break.time<=60 & break.time>2])
  stops.1to10minutes <- length(break.time[break.time<=600 & break.time>60])
  stops.10to30minutes <- length(break.time[break.time<=1800 & break.time>600])
  stops.long <- length(break.time[break.time>1800])

  session.distance <- ifelse("total_distance" %in% colnames(session),
                             session$total_distance[1],NA)
  session.elapsed.time <- ifelse("total_elapsed_time" %in% colnames(session),
                                 session$total_elapsed_time[1],NA)
  session.timer.time <- ifelse("total_timer_time" %in% colnames(session),
                               session$total_timer_time[1],NA)
  session.pedal.strokes <- ifelse("total_cycles" %in% colnames(session),
                                  session$total_cycles[1],NA)
  session.total.calories <- ifelse("total_calories" %in% colnames(session),
                                   session$total_calories[1],NA)
  session.avg.speed <- ifelse("avg_speed" %in% colnames(session),
                              session$avg_speed[1],NA)
  session.max.speed <- ifelse("max_speed" %in% colnames(session),
                              session$max_speed[1],NA)
  session.total.ascent <- ifelse("total_ascent" %in% colnames(session),
                                 session$total_ascent[1],NA)
  session.total.descent <- ifelse("total_descent" %in% colnames(session),
                                  session$total_descent[1],NA)
  session.avg.cadence <- ifelse("avg_cadence" %in% colnames(session),
                                session$avg_cadence[1],NA)
  session.avg.hr <- ifelse("avg_heart_rate" %in% colnames(session),
                           session$avg_heart_rate[1],NA)
  session.max.hr <- ifelse("max_heart_rate" %in% colnames(session),
                           session$max_heart_rate[1],NA)
  session.avg.power <- ifelse("avg_power" %in% colnames(session),
                           session$avg_power[1],NA)
  session.max.power <- ifelse("max_power" %in% colnames(session),
                           session$max_power[1],NA)
  sourcefile <- basename(ridefile)
  processed.time <- Sys.time()

  if (!is.na(trackdata$position_lon.dd[1])){
    begEndGap <- raster::pointDistance(cbind(trackdata$position_lon.dd[1],
                                             trackdata$position_lat.dd[1]),
                                       cbind(trackdata$position_lon.dd[nrow(trackdata)],
                                             trackdata$position_lat.dd[nrow(trackdata)]),
                                       lonlat=TRUE)
    if (begEndGap>100) {
      cat("  *non-loop - distance between start and end = ",begEndGap,"m \n")
      cat("   start = ",trackdata$position_lon.dd[1],"  ",
                   trackdata$position_lat.dd[1],"  \n")
      cat("   stop  = ",trackdata$position_lon.dd[nrow(trackdata)],"  ",
                   trackdata$position_lat.dd[nrow(trackdata)],"\n")
      ride.loop <- FALSE
    } else {
      ride.loop <- TRUE
    }
  } else {
    begEndGap <- NA
    ride.loop <- NA
  }
  deltaElev <- trackdata$altitude.m[nrow(trackdata)] -
               trackdata$altitude.m[1]
  gr <- trackdata$gearratio[!is.na(trackdata$gearratio) &
                              trackdata$gearratio >= gr.low &
                              trackdata$gearratio < gr.high]
  if (length(gr) > 0.50*nrow(trackdata)) {
    if (is.null(gr.bound)) {
      m <- min(gr.peak.m,round(nrow(gr)/600))
      grdens <- stats::density(gr,kernel=gr.kernel,adjust=gr.adjust)
      plot(grdens)
      peakspots <- find_peaks(grdens$y,m=m)
      maxpdens <- max(grdens$y[peakspots])
      peakspots <- peakspots[grdens$y[peakspots] > gr.density.threshold*maxpdens]
      peaks <- grdens$x[peakspots]
      pprt = utils::head(formatC(peaks, digits = 3, format = "f"),16)
      cat("  gears ",pprt,"\n")
      if (length(peaks) >= gr.nlowgear+1) {
        low.gear <- peaks[gr.nlowgear]
        lowgear <- as.numeric( trackdata$gearratio <
                                 (peaks[gr.nlowgear] + peaks[gr.nlowgear+1])/2 )
        pct.low.gear <- sum(lowgear[!is.na(lowgear)])/length(trackdata$gearratio)
      }  else  {
        pct.low.gear <- NA
        low.gear <- NA
      }
      pct.low.gear2 <- NA
      low.gear2 <- NA
    } else {
      low.gear <- gr.bound
      lowgear <- as.numeric( trackdata$gearratio < low.gear )
      pct.low.gear <- sum(lowgear[!is.na(lowgear)])/length(trackdata$gearratio)
      if (!is.null(gr.bound2)) {
        low.gear2 <- gr.bound2
        lowgear <- as.numeric( trackdata$gearratio < low.gear2 )
        pct.low.gear2 <- sum(lowgear[!is.na(lowgear)])/length(trackdata$gearratio)

      }
    }
  }  else {
    pct.low.gear <- NA
    low.gear <- NA
    pct.low.gear2 <- NA
    low.gear2 <- NA
  }

  track.cleaned <- tibble::data_frame(date=ride.date,start.time,start.hour,
      nwaypoints,numsegs,begEndGap,deltaElev,
      distance,total.time,rolling.time,pedal.time,startline.time,pedal.strokes,
      avgcadence.nozeros,avgcadence.withzeros,avgcadence.midsegment,
      hr.at.stop,hr.recovery,speed.rolling.m.s,speed.all.m.s,speed.max.m.s,
      ascent,descent,distance.ascending,distance.descending,
      grade.ascending.steepest,grade.descending.steepest,
      pct.trkpts.hr,pct.trkpts.cad,ride.loop,
      pct.low.gear,low.gear,pct.low.gear2,low.gear2,
      stops.subminute,stops.1to10minutes,stops.10to30minutes,stops.long,
      session.distance,session.elapsed.time,session.timer.time,
      session.pedal.strokes,session.avg.power,session.max.power,
      session.total.calories,session.avg.speed,session.max.speed,
      session.total.ascent,session.total.descent,
      session.avg.cadence,session.avg.hr,session.max.hr,
      sourcefile,processed.time,startbutton.date,startbutton.time)
  return(list(summary=track.cleaned,trackpoints=trackdata,session=session))
}


#' read and clean a gps data file
#'
#' \code{read_ride}  processes a gps .fit and/or .gpx track file to create
#'   summary and detail data tibbles for modeling or graphical summary.  Track
#'   segments are adjusted to remove many false autostop-start and startline
#'   delay sequences.  Several measures of cadence are calculated (including
#'   zeros, excluding zeros, and midsegment),spurious and missing cadence values
#'   are repaired, and summary stats are calculated for analysis.
#'
#' @param ridefile a filenames (.fit or .gpx extension) to process
#' @param tz string contaiing timezone for converting from UTC
#' @param fixDistance repair nonmonotonicities in distance which are on
#'    segment breaks - this occurs when power is lost or on some device lockups
#' @param loud print information about re/segmenting track data
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list of three data tibbles:  \eqn{summary} and \eqn{trackpoints}
#'    and \eqn{session}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ridefiles}},
#'    \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
read_ride <- function(ridefile,tz="America/Los_Angeles",
                      fixDistance=FALSE,loud=FALSE,...)  {

  cat("reading: ",ridefile,"\n")
  if (missing(tz)) {
    tz <- Sys.timezone()
  }
  if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".fit") {
    temp <- read_fittrack(ridefile)
    time.fn.string <- basename(ridefile)
    fit.fn.time.parse <- getOption("bCadHr.fit.fn.time.parse")
    fit.fn.lead <- getOption("bCadHr.fit.fn.lead")
    fit.fn.trail <- getOption("bCadHr.fit.fn.trail")
    if (nchar(fit.fn.lead)>0) time.fn.string <-
      substr(time.fn.string,
             regexpr(fit.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(fit.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(fit.fn.trail,time.fn.string))
    time.turned.on <- strptime(time.fn.string,fit.fn.time.parse,tz=tz)
  } else if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".gpx") {
    temp <- read_gpxtrack(ridefile)
    time.fn.string <- basename(ridefile)
    gpx.fn.time.parse <- getOption("bCadHr.gpx.fn.time.parse")
    gpx.fn.lead <- getOption("bCadHr.gpx.fn.lead")
    gpx.fn.trail <- getOption("bCadHr.gpx.fn.trail")
    if (nchar(gpx.fn.lead)>0) time.fn.string <-
      substr(time.fn.string,
             regexpr(gpx.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(gpx.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(gpx.fn.trail,time.fn.string))
    time.turned.on <- strptime(time.fn.string,gpx.fn.time.parse,tz=tz)
  } else {
    stop("unknown file extension")
  }
  trackdata <- temp[["track"]]
  recovery_hr <- temp[["recovery_hr"]]
  session <- temp[["session"]]

  attr(trackdata$timestamp.s,"tzone") <- tz
  if (is.na(time.turned.on)){
    #  if filename not successfully turned into a start-button time, use 1st
    #   timestamp value recorded in the track.  Likely but not certain to match
    #   between different versions of the same ride
    time.turned.on <- trackdata$timestamp.s[1]
  }
  startbuttonDate <- as.integer(lubridate::mday(time.turned.on)+
                                   100*lubridate::month(time.turned.on)+
                                   10000*lubridate::year(time.turned.on))
  startbuttonTime <- as.integer(lubridate::second(time.turned.on)+
                                   100*lubridate::minute(time.turned.on)+
                                   10000*lubridate::hour(time.turned.on))
  trackdata$startbutton.date <- startbuttonDate
  trackdata$startbutton.time <- startbuttonTime
  ###   minimal data validity checks after dumping unusable obs
  trackdata <- stripDupTrackRecords(trackdata,fixDistance=fixDistance)
  if (is.unsorted(trackdata$segment,strictly=FALSE))
    stop(paste0(ridefile," yields segment ids that are not nondecreasing!"))
  if (is.unsorted(trackdata$distance.m,strictly=FALSE))
    stop(paste0(ridefile," yields distances that are not nondecreasing!"))
  if (is.unsorted(trackdata$timestamp.s,strictly=TRUE))
    stop(paste0(ridefile," yields timestamps that are
                            not strictly increasing!"))

  #  split/revise track into segments separated by non-negligile stops
  trackdata <- processSegments(trackdf=trackdata,loud=loud)
  cadStats <- statsCadence(trackdf=trackdata,...)
  powStats <- statsPower(trackdf=trackdata,...)
  hrStats <- statsHeartRate(trackdf=trackdata,recovery_hr=recovery_hr,...)
  gearStats <- statsGearing(trackdf=trackdata,...)
  climbStats <- statsGrade(trackdf=trackdata,...)
  sessionStats <- statsSession(session)
  stopsStats <- statsStops(trackdf=trackdata,...)

  if (!is.na(trackdata$position_lon.dd[1])){
    begEndGap <- raster::pointDistance(cbind(trackdata$position_lon.dd[1],
                                             trackdata$position_lat.dd[1]),
                                       cbind(trackdata$position_lon.dd[nrow(trackdata)],
                                             trackdata$position_lat.dd[nrow(trackdata)]),
                                       lonlat=TRUE)
    if (begEndGap>100) {
      cat("  *non-loop - distance between start and end = ",begEndGap,"m \n")
      cat("   start = ",trackdata$position_lon.dd[1],"  ",
          trackdata$position_lat.dd[1],"  \n")
      cat("   stop  = ",trackdata$position_lon.dd[nrow(trackdata)],"  ",
          trackdata$position_lat.dd[nrow(trackdata)],"\n")
      rideLoop <- FALSE
    } else {
      rideLoop <- TRUE
    }
  } else {
    begEndGap <- NA
    rideLoop <- NA
  }

  rideDate <- as.Date(format(trackdata$timestamp.s[1],tz=tz))
  startTime <- as.character(trackdata$timestamp.s[1])
  startHour <- as.numeric(lubridate::hour(format(time.turned.on,tz=tz))
           + 0.25*(round((60*lubridate::minute(format(time.turned.on,tz=tz))+
                             lubridate::second(format(time.turned.on,tz=tz)))/
                           (60*15))))
   track.cleaned <-
    tibble::data_frame(date = rideDate,
                       start.time = startTime,
                       start.hour = startHour,
                       nwaypoints = nrow(trackdata),
                       numsegs = max(trackdata$segment),
                       pct.trkpts.hr = sum(!is.na(trackdata$heart_rate.bpm))/
                                         nrow(trackdata),
                       pct.trkpts.cad = sum(!is.na(trackdata$cadence.rpm))/
                                          nrow(trackdata),
                       begEndGap = begEndGap,
                       deltaElev = trackdata$altitude.m[nrow(trackdata)] -
                                   trackdata$altitude.m[1],
                       ride.loop = rideLoop,
                       distance = trackdata$distance.m[nrow(trackdata)],
                       total.time = totalTime(trackdata),
                       rolling.time = rollingTime(trackdata),
                       pedal.time = pedalTime(trackdata),
                       speed.rolling.m.s = trackdata$distance.m[nrow(trackdata)]/
                                              rollingTime(trackdata),
                       speed.all.m.s = trackdata$distance.m[nrow(trackdata)]/
                                              totalTime(trackdata),
                       speed.max.m.s = max(trackdata$speed.m.s,na.rm=TRUE),
                       avgcadence.nozeros = cadStats[["avgcadenceNoZeros"]],
                       avgcadence.withzeros = cadStats[["avgcadenceWithZeros"]],
                       avgcadence.midsegment = cadStats[["avgcadenceMidsegment"]],
                       ascent = climbStats[["ascent"]],
                       descent = climbStats[["descent"]],
                       distance.ascending = climbStats[["distanceAscending"]],
                       distance.descending = climbStats[["distanceDescending"]],
                       pct.low.gear = gearStats[["pctLowGear1"]],
                       low.gear = gearStats[["lowGear1"]],
                       pct.low.gear2 = gearStats[["pctLowGear2"]],
                       low.gear2 = gearStats[["lowGear2"]],
                       hr.at.stop = hrStats[["hrAtStop"]],
                       hr.recovery = hrStats[["hrRecovery"]],
                       startline.time = stopsStats[["startlineTime"]],
                       stops.subminute = stopsStats[["stopsSubMinute"]],
                       stops.1to10minutes = stopsStats[["stops1to10Minutes"]],
                       stops.10to30minutes = stopsStats[["stops10to30Minutes"]],
                       stops.long = stopsStats[["stopsLong"]],
                       session.distance = sessionStats[["sessionDistance"]],
                       session.elapsed.time = sessionStats[["sessionElapsedTime"]],
                       session.timer.time = sessionStats[["sessionTimerTime"]],
                       session.pedal.strokes = sessionStats[["sessionPedalStrokes"]],
                       session.total.calories = sessionStats[["sessionTotalCalories"]],
                       session.avg.speed = sessionStats[["sessionAvgSpeed"]],
                       session.max.speed = sessionStats[["sessionMaxSpeed"]],
                       session.total.ascent = sessionStats[["sessionTotalAscent"]],
                       session.total.descent = sessionStats[["sessionTotalDescent"]],
                       session.avg.cadence = sessionStats[["sessionAvgCadence"]],
                       session.avg.hr = sessionStats[["sessionAvgHr"]],
                       session.max.hr = sessionStats[["sessionMaxHr"]],
                       session.avg.power = sessionStats[["sessionAvgPower"]],
                       session.max.power = sessionStats[["sessionMaxPower"]],
                       sourcefile = basename(ridefile),
                       processed.time = Sys.time(),
                       startbutton.date=startbuttonDate,
                       startbutton.time=startbuttonTime)
  return(list(summary=track.cleaned,trackpoints=trackdata,session=session))
}
#' clean up and add start/stop segments to a track tibble, add flag for stopped
#'
#' \code{processSegments}  processes a gps track file to correct or add
#'    start/stop segment information like that generated by gps autopause.
#'    The sequence of processing is: eliminate stops that are too short,
#'    split segments where time between observations is too large, split
#'    segments where stopped for longer than threshold, then merge segments
#'    which are too small.  Finally, all segment breaks too close to the
#'    start line are removed
#'
#' @param trackdf data frame or tibble with gps track data
#' @param segBreakTimeMin (seconds) segments separated by less time
#'    than this are joined
#' @param nonsegTimeGapMax (seconds) segemnts are split between datapoints
#'    separated by more time than this
#' @param segSplitTimeStop segments are split at points where
#' @param segMinObs segments smaller than this are joined to closest
#'    neighboring segment
#' @param segMinMeters segments shorter than this are joined with predecessor
#' @param segMinSecs (seconds) segments shorter than this are joined
#' @param segInitIdleAggSecs stops/restarts before this time has
#'    elapsed are discarded and the ride starts at the last start prior to this
#' @param segInitIdleAggMeters stops/restarts within this distance
#'    of the initial start are discarded as above
#' @param ignoreSegInfo wipe existing segment information and use
#'    \code{nonsegTimeGapMax} and \code{segSplitTimeStop} to
#'    reconstruct segments before merging short ones
#' @param loud display summary of actions
#' @param ... parameters for
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a tibble containing track data with start/stop segment data
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
processSegments <- function(trackdf,
                            segInitIdleAggSecs=60,
                            segInitIdleAggMeters=20,
                            segBreakTimeMin=3,
                            nonsegTimeGapMax=30,
                            segSplitTimeStop=5,
                            segMinObs=3,segMinMeters=10,segMinSecs=7,
                            ignoreSegInfo=FALSE,loud=FALSE,...) {

  #  demand some small amount of consistency
  segSplitTimeStop <- max(segSplitTimeStop,segBreakTimeMin)

  #   no observations will be deleted or added, so set up some useful stuff
  #
  deltatime <- as.numeric(difftime(trackdf$timestamp.s,
                                   lag_one(trackdf$timestamp.s),units="secs"),
                          units="secs")
  timesecs <- cumsum(deltatime)
  stopped <- trackdf$speed.m.s==0
  stopped[length(stopped)] <-TRUE
  starting <- !stopped & lag_one(stopped)
  starting[1] <- TRUE

  ###   now process segment and stop data
  #  wipe existing segment data from gps (auto)start/stop if requested
  #  otherwise make it a sequence of consecutive nondecreasing integers
  if (loud) {
    newseg <- trackdf$segment != lag_one(trackdf$segment)
    newseg[1] <- TRUE
    cat("starting out with ",1+sum(trackdf$segment!=lag_one(trackdf$segment)),
        " segments\n")
  }
  if (ignoreSegInfo) {
    if (loud) cat("wiping out existing segment data\n")
    trackdf$segment <- 1
  }
  newseg <- trackdf$segment != lag_one(trackdf$segment)
  newseg[1] <- TRUE
  # split really, really long intervals between points
  needsplit <- (deltatime > nonsegTimeGapMax) & !newseg
  if (sum(needsplit) > 0) {
    if (loud) {
      cat("splitting ",sum(needsplit)," long intervals in same segment\n")
      # print(trackdf$timestamp.s[needsplit])
    }
    newseg <- newseg | needsplit
  }
  # join segment breaks which are too short
  needjoin <- (deltatime < segBreakTimeMin) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," short breaks between segments\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }
  # split if time since last motion is too large
  lastmovedtime <- cummax(ifelse(stopped&!newseg,
                                 0,
                                 timesecs))
  stoptoolong <- c(FALSE,diff(lastmovedtime) >= segSplitTimeStop) &
                 !stopped &
                 lag_one(stopped)

  needsplit <- stoptoolong & !newseg
  if (sum(needsplit) > 0) {
    if (loud) {
      cat("splitting ",sum(needsplit)," segments with long stops\n")
      #print(trackdf$timestamp.s[needsplit])
    }
    newseg <- newseg | needsplit
  }

  # collapse too-short segments together  join them with previous segment
  #     (stops and start/stops occur at end of segment)
  tempdf <-  trackdf %>% dplyr::select(timestamp.s,distance.m)
  newseg[1] <- TRUE  # do this before regenerate segment variable
  tempdf$segment <- cumsum(newseg)
  #  stack the last obs on bottom of newseg for diff'ing
  segobs <- newseg
  segobs[length(segobs)] <- TRUE
  segdf <- tempdf[segobs,]
  segmeters <- diff(segdf$distance.m)
  segsecs <- difftime(segdf$timestamp.s,
                      lag_one(segdf$timestamp.s),units="secs")[-1]
  segdf <- segdf[-nrow(segdf),]
  segdf$segmeters <- segmeters
  segdf$segsecs <- segsecs

  tempdf <- tempdf %>%
    dplyr::group_by(segment) %>%
    dplyr::mutate(nobsinseg=n()) %>%
    dplyr::left_join(segdf,by="timestamp.s")
  tempdf[!newseg,c("segmeters","segsecs")] <- 0
  needjoin <- (tempdf$nobsinseg < segMinObs) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments with fewer than ",
          segMinObs," obs\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }
  needjoin <- (tempdf$segmeters < segMinMeters) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments shorter than ",
          segMinMeters," meters\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }
  needjoin <- (tempdf$segsecs < segMinSecs) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments shorter than ",
          segMinSecs," seconds\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }

  ###  now allow the first segment to include starts/stop issues at beginning
  insidefirstseg <- (cumsum(deltatime) <= segInitIdleAggSecs)  |
    (trackdf$distance.m <= segInitIdleAggMeters)
  needjoin <- insidefirstseg & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments at start\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }

  #  these are only modifications to the track data tibble
  newseg[1] <- TRUE
  trackdf$segment <- cumsum(newseg)
  trackdf$stopped <- stopped
  trackdf$deltatime <- deltatime

  return(trackdf)
}
#' clean up and summarize cadence statistics for a track
#'
#' \code{statsCadence}  processes a gps track file to correct and
#'   summarize cadence data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param cadTrimBegSecs number of seconds after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndSecs number of seconds before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimBegMeters number of meters after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndMeters number of meters before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadMax max credible cadence value, larger values are errors
#' @param cadCorrectTooHigh repair excessive cadence values
#'    using triangular-kernel-weighted average of the nearest nonmissing values
#'    in the same segment
#' @param cadCorrectNA repair cadence missing values using
#'    triangular-kernel-weighted average of the nearest nonmissing values in
#'    the same segment
#' @param cadCorrectWindowSec window size for kernel smoothing of cadence
#' @param ... parameters for \code{\link{processSegments}},
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
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsCadence <- function(trackdf,
                         cadTrimBegSecs=15,cadTrimBegMeters=10,
                         cadTrimEndSecs=20,cadTrimEndMeters=15,
                         cadMax=160,cadCorrectTooHigh=TRUE,
                         cadCorrectNA=FALSE,
                         cadCorrectWindowSec=7,...) {

  ## patch cadence errors and then NAs
  cadenceTooHigh <- trackdf$cadence.rpm > cadMax
  cadenceTooHigh[is.na(cadenceTooHigh)] <- FALSE
  if (sum(cadenceTooHigh) > 0) {
    if (cadCorrectTooHigh) {
      trackdf$cadence.rpm[cadenceTooHigh] <- NA
      cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                           xvar=cumsum(trackdf$deltatime),
                                           segment=trackdf$segment,
                                           bw=cadCorrectWindowSec,
                                           nneighbors=cadCorrectWindowSec,
                                           kernel="triangular")
      trackdf$cadence.rpm[cadCorrectTooHigh] <-
        cadenceSmoothed[cadCorrectTooHigh]
    } else {
      trackdf$cadence.rpm[cadCorrectTooHigh] <- NA
    }
  }
  if (cadCorrectNA) {
    cadenceNA <- is.na(trackdf$cadence.rpm)
    if (sum(cadenceNA) > 0) {
      cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                            xvar=cumsum(trackdf$deltatime),
                                            segment=trackdf$segment,
                                            bw=cadCorrectWindowSec,
                                            nneighbors=cadCorrectWindowSec,
                                            kernel="triangular")
      trackdf$cadence.rpm[cadenceNA] <- cadenceSmoothed[cadenceNA]
    }
  }
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

  cadenceNumerator <- sum(trackdf$deltatime[pedaling]*
                            trackdf$cadence.rpm[pedaling])
  cadenceNumeratorInner <- sum(trackdf$deltatime[pedaling&innersegment]*
                                 trackdf$cadence.rpm[pedaling&innersegment])

  avgcadenceNoZeros <- cadenceNumerator / pedalTime(trackdf)
  avgcadenceWithZeros <- cadenceNumerator / rollingTime(trackdf)
  avgcadenceMidsegment <- cadenceNumeratorInner /
    sum(trackdf$deltatime[pedaling & innersegment])

  return(list(avgcadenceNoZeros=avgcadenceNoZeros,
              avgcadenceWithZeros=avgcadenceWithZeros,
              avgcadenceMidsegment=avgcadenceMidsegment))
}
#' generate power statistics for a track
#'
#' \code{statsPower}  processes a gps track file to summarize the power data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param ... parameters for \code{\link{processSegments}},
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
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsPower <- function(trackdf,...) {
  powerpos <- !is.na(trackdf$power.watts) & trackdf$power.watts > 0
  powerNum <- sum(trackdf$power.watts[powerpos]*
                    trackdf$deltatime[powerpos]  )
  avgpowerNoZeros <- powerNum / sum(trackdf$deltatime[powerpos])
  avgpowerWithZeros <- powerNum / rollingTime(trackdf)

  return(list(avgpowerNoZeros=avgpowerNoZeros,
              avgpowerWithZeros=avgpowerWithZeros))
}
#' generate heartrate statistics for a track
#'
#' \code{statsHeartRate}  processes a gps track file to summarize the hr data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param recovery_hr data frame or tibble with gps recovery hr data
#' @param ... parameters for \code{\link{processSegments}},
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
    lastHr <- max(which(!is.na(trackdf$heart_rate.bpm)))
    if (as.numeric(difftime(trackdf$timestamp.s[nrow(trackdf)],
                            trackdf$timestamp.s[lastHr],units="secs")) < 10) {
      hrAtStop <- as.numeric(trackdf$heart_rate.bpm[lastHr])
    }
    else {
      hrAtStop <- NA
    }
  } else {
    hrAtStop <- NA
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
  return(list(hrAtStop=hrAtStop,
              hrRecovery=hrRecovery))
}
#' generate gear ratio statistics for a track
#'
#' \code{statsGearing}  processes a gps track file to summarize the gear ratio
#'   implied by the ratio of cadence to speed
#'
#' @param trackdf data frame or tibble with gps track data
#' @param grLow1 lowest gear speed/cadence upper bound
#' @param grLow2 alternate lowest gear speed/cadence upper bound
#' @param ... parameters for \code{\link{processSegments}},
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
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsGearing <- function(trackdf,
                         grLow1=0.044,grLow2,...) {
  ## ratio of speed to cadence
  grLow2 <- NA
  pctLowGear1 <- NA
  pctLowGear2 <- NA
  pedaling <- (!is.na(trackdf$cadence.rpm)) & (trackdf$cadence.rpm > 0)
  if (sum(pedaling)>100) {
    gearratio <- trackdf$speed.m.s/trackdf$cadence.rpm
    gearratio[is.na(gearratio)|is.infinite(gearratio)] <- 10000
    inlowgear <- ( gearratio < grLow1 )
    pctLowGear1 <- sum(trackdf$deltatime[pedaling & inlowgear]*
                       gearratio[pedaling & inlowgear]) /
                             pedalTime(trackdf)
    if (!missing(grLow2)) {
      inlowgear <- ( gearratio < grLow2 )
      pctLowGear2 <- sum(trackdf$deltatime[pedaling & inlowgear]*
                         gearratio[pedaling & inlowgear]) /
                             pedalTime(trackdf)
    }
  }
  return(list(lowGear1=grLow1,
              lowGear2=grLow2,
              pctLowGear1=pctLowGear1,
              pctLowGear2=pctLowGear2))

}
#' generate grade statistics for a track
#'
#' \code{statsGrade}  processes a gps track file to summarize grade data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param gradeSmoothWindowNN number of observations on each side used for
#'    calculating kernel smoothed elevation for the grade calculation
#' @param gradeSmoothWindowMeters bandwidth (meters) for kernel smoothing of
#'    elevation in the grade calculation
#' @param ... parameters for \code{\link{processSegments}},
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
                       gradeSmoothWindowNN=10,...) {

  deltaelev <- trackdf$altitude.m - lag_one(trackdf$altitude.m)
  elevSmoothed <- smoothDataSegments(yvec=trackdf$altitude.m,
                                     xvar=trackdf$distance.m,
                                     segment=trackdf$segment,
                                     bw=gradeSmoothWindowMeters,
                                     nneighbors=gradeSmoothWindowNN,
                                     kernel="epanechnikov")
  deltaelevSmoothed <- elevSmoothed - lag_one(elevSmoothed)
  trackdeltadistance <- trackdf$distance.m - lag_one(trackdf$distance.m)

  trackgrade <- deltaelevSmoothed/trackdeltadistance
  trackgrade[trackdeltadistance<1] <- NA

  ascent  <- sum(deltaelevSmoothed[deltaelevSmoothed>0])
  descent <- sum(deltaelevSmoothed[deltaelevSmoothed<0])
  distanceAscending  <- sum(trackdeltadistance[deltaelev >  0.25])
  distanceDescending <- sum(trackdeltadistance[deltaelev < -0.25])

  return(list(ascent=ascent,descent=descent,
              distanceAscending=distanceAscending,
              distanceDescending=distanceDescending))
}
#' collect gps session statistics for a track
#'
#' \code{statsSession}  processes a gps session file
#'
#' @param session data frame or tibble with gps session data
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsStops}}
#'
#' @export
statsSession <- function(session) {
  if (is.null(session)) {
    sessionDistance <- NA
    sessionElapsedTime <- NA
    sessionTimerTime <- NA
    sessionPedalStrokes <- NA
    sessionTotalCalories <- NA
    sessionAvgSpeed <- NA
    sessionMaxSpeed <- NA
    sessionTotalAscent <- NA
    sessionTotalDescent <- NA
    sessionAvgCadence <- NA
    sessionAvgHr <- NA
    sessionMaxHr <- NA
    sessionAvgPower <- NA
    sessionMaxPower <- NA
  } else {
    sessionDistance <- ifelse("total_distance" %in% colnames(session),
                              session$total_distance[1],NA)
    sessionElapsedTime <- ifelse("total_elapsed_time" %in% colnames(session),
                                 session$total_elapsed_time[1],NA)
    sessionTimerTime <- ifelse("total_timer_time" %in% colnames(session),
                               session$total_timer_time[1],NA)
    sessionPedalStrokes <- ifelse("total_cycles" %in% colnames(session),
                                  session$total_cycles[1],NA)
    sessionTotalCalories <- ifelse("total_calories" %in% colnames(session),
                                   session$total_calories[1],NA)
    sessionAvgSpeed <- ifelse("avg_speed" %in% colnames(session),
                              session$avg_speed[1],NA)
    sessionMaxSpeed <- ifelse("max_speed" %in% colnames(session),
                              session$max_speed[1],NA)
    sessionTotalAscent <- ifelse("total_ascent" %in% colnames(session),
                                 session$total_ascent[1],NA)
    sessionTotalDescent <- ifelse("total_descent" %in% colnames(session),
                                  session$total_descent[1],NA)
    sessionAvgCadence <- ifelse("avg_cadence" %in% colnames(session),
                                session$avg_cadence[1],NA)
    sessionAvgHr <- ifelse("avg_heart_rate" %in% colnames(session),
                           session$avg_heart_rate[1],NA)
    sessionMaxHr <- ifelse("max_heart_rate" %in% colnames(session),
                           session$max_heart_rate[1],NA)
    sessionAvgPower <- ifelse("avg_power" %in% colnames(session),
                              session$avg_power[1],NA)
    sessionMaxPower <- ifelse("max_power" %in% colnames(session),
                              session$max_power[1],NA)
  }
  return(list(
    sessionDistance = sessionDistance,
    sessionElapsedTime = sessionElapsedTime,
    sessionTimerTime = sessionTimerTime,
    sessionPedalStrokes = sessionPedalStrokes,
    sessionTotalCalories = sessionTotalCalories,
    sessionAvgSpeed = sessionAvgSpeed,
    sessionMaxSpeed = sessionMaxSpeed,
    sessionTotalAscent = sessionTotalAscent,
    sessionTotalDescent = sessionTotalDescent,
    sessionAvgCadence = sessionAvgCadence,
    sessionAvgHr = sessionAvgHr,
    sessionMaxHr = sessionMaxHr,
    sessionAvgPower = sessionAvgPower,
    sessionMaxPower = sessionMaxPower))
}
#' generate stops statistics for a track
#'
#' \code{statsStops}  processes a gps track file to summarize grade data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param segInitIdleAggSecs stops/restarts before this time has
#'    elapsed are discarded and the ride starts at the last start prior to this
#' @param segInitIdleAggMeters stops/restarts within this distance
#'    of the initial start are discarded as above
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}}
#'
#' @export
statsStops <- function(trackdf,
                       segInitIdleAggSecs=60,
                       segInitIdleAggMeters=20,
                       ...) {

  segstart <- trackdf$segment != lag_one(trackdf$segment)
  segstart[1] <- TRUE

  timesecs <- cumsum(trackdf$deltatime)
#  moving <- trackdf$speed.m.s > 0
  moving <- !trackdf$stopped
  moving[!((lag_one(lag_one(moving))&lag_one(moving))|
           (lead_one(lead_one(moving))&lead_one(moving)))] <- FALSE
  movingtime <- timesecs*as.numeric(moving)
  lastmovetime <- lead_one(cummax(movingtime))
  stoplength <- (timesecs[segstart] - lastmovetime[segstart])[-1]

  startlinewait <- timesecs <= segInitIdleAggSecs &
    trackdf$distance.m <= segInitIdleAggMeters &
    trackdf$speed.m.s > 0
  startlineTime <- sum(trackdf$deltatime[startlinewait])
  stopsSubMinute <- sum(stoplength > 0 & stoplength < 60)
  stops1to10Minutes <- sum(stoplength >= 60 & stoplength < 600)
  stops10to30Minutes <- sum(stoplength >= 600 & stoplength < 1800)
  stopsLong <- sum(stoplength >= 1800)
  return(list(startlineTime=startlineTime,
              stopsSubMinute=stopsSubMinute,
              stops1to10Minutes=stops1to10Minutes,
              stops10to30Minutes=stops10to30Minutes,
              stopsLong=stopsLong))
}
totalTime <- function(trackdf) {
  return(sum(trackdf$deltatime))
}
rollingTime <- function(trackdf) {
  return(sum(trackdf$deltatime[trackdf$speed.m.s > 0]))
}
pedalTime <- function(trackdf) {
  pedaling <- !is.na(trackdf$cadence.rpm) & trackdf$cadence.rpm > 0
  return(sum(trackdf$deltatime[pedaling]))
}

