#' add/replace ride data in all tbl pairs
#'
#' \code{update_gps_variables} Add/replace ride data in .fit, .gpx and combined
#'  tbl pairs (summaries,tracks)
#'
#' Update existing (summaries,tracks) pairs for .fit and .gpx files structured
#'   with new files at the top level, and files already included in the
#'   pairs of tbls summarizing the rides are in subdirectories below them.
#'   there are 3 file pairs created:
#'     (fitsummary,fittracks), (gpxsummary,gpxtracks) and (gpssummary,gpstracks)
#'     the last pair contains data from both joined, with preference given to
#'     .fit-sourced data
#'
#'
#' @param outdir string containing location of output data pair files
#' @param fitrootdir string containing location of root directory for gps .fit files
#' @param gpxrootdir string containing location of root directory for gps .gpx files
#' @param merge.files list of vectors (order matters within vectors) of .fit
#'    and .gpx files at the top level to be joined
#' @param fitexcludes list of substrings within a file name which
#'    will cause the file to be ignored.  occasionally .fit files will be unreadable
#'    by these routines and excluding them will be necessary
#' @param gpxexcludes list of substrings within a file
#'    name which will cause the file to be ignored.  occasionally .fit files will
#'    be unreadable by the routines in this package
#' @param rebuild.all.fit if TRUE, will read all files in subdirectories
#'    of \eqn{fitrootdir}, but not in the directory itself
#' @param rebuild.all.gpx if TRUE, will read all files in subdirectories
#'    of \eqn{gpxrootdir}, but not in the directory itself
#' @param prefer.gpx vector of strings identifying times to use a .gpx file
#'    rather than the corresponding .fit file
#' @param drawprofile create a .png file in the output directory profiling
#'    the ride
#' @param drawprofile.both create a .png file in the output directory profiling
#'    the ride for both the .fit files and .gpx files
#' @param elevationChar character to use when printing the elevation profile
#' @param drawmap create a .tiff file in the output directory mapping
#'    the ride with speed-varying color
#' @param drawmap.both create a .tiff file in the output directory mapping
#'    the ride with speed-varying color for both the .fit files and the .gox files
#' @param cores numer of cores (default is #CPUs - 1)
#' @param ... parameters passed for track cleaning
#'
#' @return a tbl contining the combined summaries and tracks, with preference
#'   given to data from .fit files over the corresponding .gpx file, with specified
#'   exceptions
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
update_gps_variables <- function(outdir,fitrootdir,gpxrootdir,merge.files=list(c(NA,NA)),
                  fitexcludes=c("bad","short"),gpxexcludes=c("bad","short","nosegs"),prefer.gpx=c(""),
                  rebuild.all.fit=FALSE,rebuild.all.gpx=FALSE,
                  drawprofile=TRUE,drawprofile.both=FALSE,elevationChar="|",
                  drawmap=TRUE,drawmap.both=FALSE,cores=4,...) {

  num_drawn <- 0
  num_mapped <- 0
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(fitrootdir)
  if(missing(cores)) cores <- parallel::detectCores()
  if (is.na(cores)) cores <- 1

  if (rebuild.all.fit) {
    sink(file=paste0(outdir,"/gps_readfitfiles.txt"),split=TRUE)
    fl <- list.files(pattern=".fit",recursive=TRUE)
    if (!(fitexcludes[1]=="")) {
      for (x in fitexcludes) {
        fl <- fl[grep(x,fl,invert=TRUE)]
      }
    }
    fitlist <- read_ridefiles(fl,cores=cores,...)
    fitsummary <- fitlist[["summaries"]]
    fittracks <- dplyr::arrange(fitlist[["tracks"]],startbutton.date,timestamp.s)
    newfitfiles <- basename(fl)
    if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
      temp <- join_ridefiles(merge.files,fitsummary,fittracks)
      if (temp[[1]]) {
        fitsummary <- temp[[3]]
        fittracks <- temp[[4]]
        newfitfiles <- setdiff(newfitfiles,temp[[2]])
      }
    }
    sink()
  } else {
    newfitfiles <- list.files(pattern=".fit",recursive=FALSE)
    load(paste0(outdir,"/fitsummary.rda"))
    load(paste0(outdir,"/fittracks.rda"))
    if (!(fitexcludes[1]=="")) {
      for (x in fitexcludes) {
        newfitfiles <- newfitfiles[grep(x,newfitfiles,invert=TRUE)]
      }
    }
    if (length(newfitfiles)>0) {
      newfitlist <- read_ridefiles(newfitfiles,cores=cores,...)
      newfitresults <- newfitlist[["summaries"]]
      newfittracks <- newfitlist[["tracks"]]
      newfiles <- newfitresults$sourcefile
      if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
        temp <- join_ridefiles(merge.files,newfitresults,newfittracks)
        if (temp[[1]]) {
          newfitresults <- temp[[3]]
          newfittracks <- temp[[4]]
          newfiles <- setdiff(newfiles,temp[[2]])
        }
      }
      datetimestr.sum <- dateTimeStr(newfitresults$startbutton.date,newfitresults$startbutton.time)
      datetimestr.track <- dateTimeStr(fittracks$startbutton.date,fittracks$startbutton.time)
      fitsummary <- dplyr::arrange(dplyr::bind_rows(newfitresults,fitsummary[!fitsummary$sourcefile %in% newfitfiles,]),date,start.hour)
      fittracks <- dplyr::arrange(dplyr::bind_rows(newfittracks,fittracks[!(datetimestr.track %in% datetimestr.sum),]),startbutton.date,timestamp.s)
      for (ridefn in basename(newfiles)) {
        idate <- fitsummary[fitsummary$sourcefile==ridefn,]$startbutton.date
        itime <- fitsummary[fitsummary$sourcefile==ridefn,]$startbutton.time
        if (drawprofile) {
          plot_profile(fittracks[fittracks$startbutton.date==idate&
                                 fittracks$startbutton.time==itime,],
                       fitsummary[fitsummary$sourcefile==ridefn,],
                       elevationShape=elevationChar,
                       savefn=paste0(outdir,"/",ridefn,"profile.pdf"))
          num_drawn <- 1
        }
        if (drawmap) {
          map_rides(fittracks[fittracks$startbutton.date==idate&
                    fittracks$startbutton.time==itime,],
                    draw.speed=TRUE,minTiles=200,
                    outfile=paste0(outdir,"/",ridefn,"map.jpg"),mapsize=c(3840,2400),
                    speed.color="speedcolors",maptype="maptoolkit-topo")
          num_mapped <- 1
        }
      }
    }
  }
  if (length(newfitfiles)>0) save(fitsummary,file=paste0(outdir,"/fitsummary.rda"))
  if (length(newfitfiles)>0) save(fittracks,file=paste0(outdir,"/fittracks.rda"))

  setwd(gpxrootdir)
  if (rebuild.all.gpx) {
    sink(file=paste0(outdir,"/gps_readgpxfiles.txt"),split=TRUE)
    fl <- list.files(pattern=".gpx",recursive=TRUE)
    if (!(gpxexcludes[1]=="")) {
      for (x in gpxexcludes) {
        fl <- fl[grep(x,fl,invert=TRUE)]
      }
    }
    gpxlist <- read_ridefiles(fl,cores=cores,...)
    gpxsummary <- gpxlist[["summaries"]]
    gpxtracks <- dplyr::arrange(gpxlist[["tracks"]],timestamp.s)
    newgpxfiles <- basename(fl)
    if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
      temp <- join_ridefiles(merge.files,gpxsummary,gpxtracks)
      if (temp[[1]]) {
        gpxsummary <- temp[[3]]
        gpxtracks <- temp[[4]]
        newgpxfiles <- setdiff(newgpxfiles,temp[[2]])
      }
    }
    sink()
  } else {
    newgpxfiles <- list.files(pattern=".gpx",recursive=FALSE)
    load(paste0(outdir,"/gpxsummary.rda"))
    load(paste0(outdir,"/gpxtracks.rda"))
    if (!(gpxexcludes[1]=="")) {
      for (x in gpxexcludes) {
      newgpxfiles <- newgpxfiles[grep(x,newgpxfiles,invert=TRUE)]
       }
    }
    if (length(newgpxfiles)>0) {
      newgpxlist <- read_ridefiles(newgpxfiles,cores=cores,...)
      newgpxresults <- newgpxlist[["summaries"]]
      newgpxtracks <- newgpxlist[["tracks"]]
      newfiles <- newgpxresults$sourcefile
      if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
        temp <- join_ridefiles(merge.files,newgpxresults,newgpxtracks)
        if (temp[[1]]) {
          newgpxresults <- temp[[3]]
          newgpxtracks <- temp[[4]]
          newfiles <- setdiff(newfiles,temp[[2]])
        }
      }
      datetimestr.sum <- dateTimeStr(newgpxresults$startbutton.date,newgpxresults$startbutton.time)
      datetimestr.track <- dateTimeStr(gpxtracks$startbutton.date,gpxtracks$startbutton.time)
      gpxsummary <- dplyr::arrange(dplyr::bind_rows(newgpxresults,gpxsummary[!gpxsummary$sourcefile %in% newgpxfiles,]),date,start.hour)
      gpxtracks <- dplyr::arrange(dplyr::bind_rows(newgpxtracks,gpxtracks[!(datetimestr.track %in% datetimestr.sum),]),startbutton.date,timestamp.s)
      for (ridefn in basename(newfiles)) {
        idate <- gpxsummary[gpxsummary$sourcefile==ridefn,]$startbutton.date
        itime <- gpxsummary[gpxsummary$sourcefile==ridefn,]$startbutton.time
        if (drawprofile &(num_drawn==0 | drawprofile.both))
          rideprofile <- plot_elev_profile_plus(gpxtracks[gpxtracks$startbutton.date==idate&
                                                gpxtracks$startbutton.time==itime,],
                                                gpxsummary[gpxsummary$sourcefile==ridefn,],
                                                elevation.shape=elevationChar,
                                                savefn=paste0(outdir,"/",ridefn,"profile.png"))
        if (drawmap &(num_mapped==0 | drawmap.both))
          map_rides(gpxtracks[gpxtracks$startbutton.date==idate&
                              gpxtracks$startbutton.time==itime,],
                  outfile=paste0(outdir,"/",ridefn,"map.jpg"),mapsize=c(1920,1200),
                  draw.speed=TRUE,speed.color="magma")
      }
    }
  }
  if (length(newgpxfiles)>0) save(gpxsummary,file=paste0(outdir,"/gpxsummary.rda"))
  if (length(newgpxfiles)>0) save(gpxtracks,file=paste0(outdir,"/gpxtracks.rda"))

  if (length(newgpxfiles)+length(newfitfiles)> 0) {
    if (length(newfitfiles)==0) load(paste0(outdir,"/fitsummary.rda"))
    if (length(newfitfiles)==0) load(paste0(outdir,"/fittracks.rda"))
    if (length(newgpxfiles)==0) load(paste0(outdir,"/gpxsummary.rda"))
    if (length(newgpxfiles)==0) load(paste0(outdir,"/gpxtracks.rda"))
    mergeddata <- combine_track_stores(primary_sums=fitsummary,primary_tracks=fittracks,
                                       secondary_sums=gpxsummary,secondary_tracks=gpxtracks,
                                       prefer_secondary=prefer.gpx)
    gpssummary <- mergeddata[[1]]
    gpstracks <- mergeddata[[2]]
    save(gpssummary,file=paste0(outdir,"/gpssummary.rda"))
    save(gpstracks,file=paste0(outdir,"/gpstracks.rda"))
  } else {
    load(paste0(outdir,"/gpssummary.rda"))
  }
  return(gpssummary)
}
#' join multiple ride segments which come from separate files
#'
#' \code{join_ridefiles} Join two or more ride segments which come from
#'   separate files, since binary .fit files are not amenable to easy
#'   editing.  The merged data is in the updated entries in the
#'   (summary,track) data pair.  Tee joined .fit files cannot be
#'   exported.
#'
#' @param joinlist list of vectors (order matters within vectors) of .fit
#'    or .gpx filenames, each vector a part of a track to be joined in order
#' @param summaries tbl of ride summaries created from gpx and/or fit files
#' @param tracks tbl of ride tracks created from gpx and/or fit files
#'
#' @return a list of two tbls containing the summaries and tracks with the
#'    specified sets of tracks merged
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
join_ridefiles <- function(joinlist,summaries,tracks) {
  #  joinlist - a list of vectors of sourcefile names to join
  #  summaries - tbl of ridesummaries created from gpx and/or fit files
  #  tracks - tbl of tracks consisting of timestamp.s,position_lat.dd,position_lon.dd,starttime,segment plus anything else
  values.from.first <- c("date","start.time","start.hour","sourcefile","processed.time",
                         "startbutton.date","startbutton.time","low.gear","low.gear2")
  values.to.add <- c("nwaypoints","numsegs","distance","total.time","rolling.time","pedal.time","pedal.strokes",
                     "ascent","descent","distance.ascending","distance.descending","stops.subminute","stops.1to10minutes",
                     "stops.10to30minutes","stops.long","session.distance","session.elapsed.time","session.timer.time",
                     "session.pedal.strokes","session.total.calories","session.total.ascent","session.total.descent",
                     "session.distance")
  values.special <- c("startline.time","avgcadence.nozeros","avgcadence.withzeros",
                      "speed.rolling.m.s","speed.all.m.s","speed.max.m.s","grade.ascending.steepest","grade.descending.steepest",
                      "session.max.speed","session.max.hr","ride.loop")
  values.weighted.mean.time <- c("avgcadence.midsegment","pct.trkpts.hr","pct.trkpts.cad",
                                 "session.avg.speed","session.avg.cadence",
                                 "session.avg.hr","pct.low.gear","pct.low.gear2")
  values.from.last <- c("hr.at.stop","hr.recovery")

  dropped.files <- "zzz"
  changed <- FALSE
  for (ridevec in joinlist) {
    if (length(ridevec) < 2) stop("cannot join fewer than 2 tracks")
    rides <- dplyr::arrange(summaries[summaries$sourcefile %in% ridevec,],start.time)
    if (nrow(rides)>0) {
      changed <- TRUE
      cat("\njoining ride files: ")
      cat("\n",ridevec,"\n")
      dropped.files <- c(dropped.files,ridevec[-1])
      summary.joined <- rides[1,]
      for (varname in values.from.last) {
        summary.joined[1,varname] <- rides[nrow(rides),varname]
      }
      for (varname in values.to.add){
        summary.joined[1,varname] <- sum(rides[,varname],na.rm=T)
      }
      for (varname in values.weighted.mean.time){
        summary.joined[1,varname] <- stats::weighted.mean(unlist(rides[,varname]),unlist(rides[,"rolling.time"]),na.rm=TRUE)
      }
      #startline time comes from first (already put there by copy), others are added into breaks
      summary.joined[1,"stops.subminute"] <- summary.joined[1,"stops.subminute"] +
            sum( rides[-1,"startline.time"] < 60 )
      summary.joined[1,"stops.1to10minutes"] <- summary.joined[1,"stops.1to10minutes"] +
            sum( rides[-1,"startline.time"] >= 60 & rides[-1,"startline.time"] < 600 )
      summary.joined[1,"stops.10to30minutes"] <- summary.joined[1,"stops.10to30minutes"] +
            sum( rides[-1,"startline.time"] >= 600 & rides[-1,"startline.time"] < 1800 )
      summary.joined[1,"stops.long"] <- summary.joined[1,"stops.1to10minutes"] +
            sum( rides[-1,"startline.time"] >= 1800 )
      summary.joined$avgcadence.nozeros <- 60*summary.joined$pedal.strokes/summary.joined$pedal.time
      summary.joined$avgcadence.withzeros <- 60*summary.joined$pedal.strokes/summary.joined$rolling.time
      summary.joined$speed.rolling.m.s <- summary.joined$distance/summary.joined$rolling.time
      summary.joined$speed.all.m.s <- summary.joined$distance/summary.joined$total.time
      summary.joined$speed.max.m.s <- max(rides$speed.max.m.s,na.rm=T)
      summary.joined$grade.ascending.steepest <- max(rides$grade.ascending.steepest,na.rm=T)
      summary.joined$grade.descending.steepest <- min(rides$grade.descending.steepest,na.rm=T)
      summary.joined$session.max.speed <- max(rides$session.max.speed,na.rm=T)
      summary.joined$session.max.hr <- max(rides$session.max.hr,na.rm=T)
      summaries <- dplyr::bind_rows(summaries[!summaries$sourcefile %in% ridevec,],summary.joined[1,]) %>%
                                dplyr::arrange(start.time)
      datetimestr.sum <- dateTimeStr(rides$startbutton.date,rides$startbutton.time)
      track <- dplyr::arrange(tracks[dateTimeStr(tracks$startbutton.date,tracks$startbutton.time) %in% datetimestr.sum,],
                        startbutton.date,timestamp.s)
      newfile <- track$startbutton.time != lag_one(track$startbutton.time) | track$startbutton.date != lag_one(track$startbutton.date)
      newseg <- (track$segment != lag_one(track$segment)) | newfile
      priordistance <- rep(0,nrow(track))
      priordistance[newfile] <- lag_one(track$distance.m)[newfile]
      track$segment <- 1 + cumsum(newseg)
      track$distance.m <- track$distance.m + cumsum(priordistance)
      if (summary.joined$numsegs[1] != max(track$segment)) {
        print(max(track$segment))
        print(summary.joined)
        stop("incorrect segment count after join")
      }
      track$startbutton.date <- track$startbutton.date[1]
      track$startbutton.time <- track$startbutton.time[1]
      if (!is.na(track$position_lon.dd[1])){
        beg.end.gap <- raster::pointDistance(cbind(track$position_lon.dd[1],track$position_lat.dd[1]),
                                             cbind(track$position_lon.dd[nrow(track)],track$position_lat.dd[nrow(track)]),
                                             lonlat=TRUE)
        if (beg.end.gap>133) {
          print(paste0("non-loop after merge- distance between start and end = ",beg.end.gap))
          print(paste0("start = ",track$position_lon.dd[1],"  ",track$position_lat.dd[1]))
          print(paste0("stop  = ",track$position_lon.dd[nrow(track)],"  ",track$position_lat.dd[nrow(track)]))
          ride.loop <- FALSE
        } else {
          ride.loop <- TRUE
        }
        } else {
        ride.loop <- NA
      }
      tracks <- dplyr::bind_rows(tracks[!(dateTimeStr(tracks$startbutton.date,tracks$startbutton.time) %in% datetimestr.sum),],track) %>%
        dplyr::arrange(startbutton.date,timestamp.s)
    }
  }
  return(list(changed,dropped.files[-1],summaries,tracks))
}
#' combine two (summary,track) tbl pairs
#'
#' \code{combine_track_stores} Combine two (summary,track) data pairs.  When both
#'   sets have data for the same date and time, the first is used unless overidden.
#'
#' Combine two (summary,track) tbl pairs,using data
#'   the first of the pairs for rides starting at the same time on the same day,
#'   with exceptions specifiable.  Typically this is to prefer data from a .fit
#'   file over data from a .gpx file corresponding to the same ride.
#'
#' @param primary_sums  tbl of ride summaries created from gpx and/or fit files
#' @param primary_tracks  tbl of ride tracks created from gpx and/or fit files
#' @param secondary_sums  tbl of ride summaries created from gpx and/or fit files
#' @param secondary_tracks  tbl of ride tracks created from gpx and/or fit files
#' @param prefer_secondary vector of strings specifying times to use data from
#'    secondary pair even if present in primary.  format is "YYYYMMDDHHMMSS",
#'    where YYY is the 4 digit year, MM is the 2 digit month, DD is the 2 digit
#'    day of month, HH is the 2 digit (0-23) hour of day, MM is 2 digit minutes
#'    and SS is 2 digit seconds
#'
#' @return a list of two tbls containing the combined summaries and tracks
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
combine_track_stores <- function(primary_sums,primary_tracks,secondary_sums,secondary_tracks,
                                   prefer_secondary=""){
    #every entry in track tbls should have a corresponding summary entry
    pri.on.times <- unique(dateTimeStr(primary_sums$startbutton.date,primary_sums$startbutton.time))
    sec.on.times <- unique(dateTimeStr(secondary_sums$startbutton.date,secondary_sums$startbutton.time))
    #  specify rides to keep based on time startbutton turned on (from filename so gpx and fit should match)
    if (nrow(primary_tracks[!dateTimeStr(primary_tracks$startbutton.date,primary_tracks$startbutton.time) %in% pri.on.times,1])>0) {
      cat("\n** track dataframe track not in primary summary, starting time ",setdiff(unique(dateTimeStr(primary_tracks$startbutton.date,primary_tracks$startbutton.time)),pri.on.times))
    }
    if (nrow(secondary_tracks[!dateTimeStr(secondary_tracks$startbutton.date,secondary_tracks$startbutton.time) %in% sec.on.times,1])>0) {
      cat("\n** track dataframe track not in secondary summary, starting time ",setdiff(unique(dateTimeStr(secondary_tracks$startbutton.date,secondary_tracks$startbutton.time)),sec.on.times))
    }
    frompri <- pri.on.times
    if (!missing(prefer_secondary)) frompri <- setdiff(frompri,intersect(sec.on.times,prefer_secondary))
    fromsec <- setdiff(sec.on.times,frompri)
    summaries <- dplyr::arrange(dplyr::bind_rows(primary_sums[dateTimeStr(primary_sums$startbutton.date,primary_sums$startbutton.time) %in% frompri,],
                                   secondary_sums[dateTimeStr(secondary_sums$startbutton.date,secondary_sums$startbutton.time) %in% fromsec,]),start.time)
    sourcefiles <- unique(summaries$sourcefile)
    tracks <- dplyr::arrange(dplyr::bind_rows(primary_tracks[dateTimeStr(primary_tracks$startbutton.date,primary_tracks$startbutton.time) %in% frompri,],
                                secondary_tracks[dateTimeStr(secondary_tracks$startbutton.date,secondary_tracks$startbutton.time) %in% fromsec,]),startbutton.date,timestamp.s)
    return(list(summaries,tracks))
}
