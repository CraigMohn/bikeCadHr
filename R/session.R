#' collect gps session statistics for a track
#'
#' \code{statsSession}  processes a gps session file
#'
#' @param session data frame or tibble with gps session data
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

#' clean up sensor dropout data for a track
#'
#' \code{repairSensorDropOut}  processes a gps track file for data dropouts
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param loud display actions taken
#' @param fixAllSensorDrop logical, if TRUE fix case of all sensor dropouts
#' @param fixSpeedSensorDrop logical, if TRUE fix case of speed sensor dropouts
#' @param sensorDropSmooth fill in values
#' @param lookBackward number of seconds to look before dropout when fixing
#' @param lookForward number of seconds to look after dropout when fixing
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return dataframe with data repaired
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
repairSensorDropOut <- function(trackdf,
                                fixAllSensorDrop=TRUE,
                                fixSpeedSensorDrop=TRUE,
                                sensorDropSmooth=TRUE,
                                lookBackward=3,
                                lookForward=5,
                                loud=FALSE,...) {

  flagdrop <- function(varname) {
    if (varname %in% names(trackdf)) {
      return(is.na(trackdf[,varname]))
    } else {
      return(TRUE)
    }
  }
  repairdrops <- function(trackdf,varname,NAOK,zeroOK,allDrop) {
    dropsrle <- rle(allDrop)
    dropids <- dropsrle[["values"]]
    ndropouts <- sum(dropids)
    dropids[dropids] <- cumsum(dropids)[dropids]
    dropsrle[["values"]] <- dropids

    allDrop <- inverse.rle(dropsrle)

    if (ndropouts > 0) {
      for (d in 1:ndropouts) {
        dd=allDrop==d
        trackdf <- repairdrop(trackdf,d,varname,NAOK,zeroOK,allDrop=dd)
      }
    }
    return(trackdf)
  }
  repairdrop <- function(trackdf,d,varname,NAOK,zeroOK,allDrop) {
    begdrow <- max(which.max(allDrop) - 1,1)
    enddrow <- min(length(allDrop) + 1 - which.max(rev(allDrop)) + 1,
                   length(allDrop))
    begtime <- max(trackdf$timestamp.s[1],
                   trackdf$timestamp.s[begdrow] - lookBackward)
    endtime <- min(trackdf$timestamp.s[nrow(allDrop)],
                   trackdf$timestamp.s[enddrow] + lookForward)
    begdrow <- max(begdrow-1,1)
    enddrow <- min(enddrow+1,length(allDrop))
    begrow <- which.max(trackdf$timestamp.s >= begtime)
    endrow <- length(allDrop) + 1 - which.max(rev(trackdf$timestamp.s <= endtime))
    if (NAOK) {
      ytemp <- trackdf[begdrow:enddrow,varname]
      if (!zeroOK) ytemp[ytemp==0] <- NA
      if (sum(!is.na(trackdf[begdrow:enddrow,varname])) >= 2) {
        yreplace <- approx(x=trackdf[begdrow:enddrow,"timestamp.s"],
                           y=ytemp,
                           xout=trackdf[begdrow:enddrow,"timestamp.s"],
                           method="linear")[[2]]
        trackdf[begdrow:enddrow,varname] <- yreplace
      }
    } else {
      ytemp <- trackdf[begrow:endrow,varname]
      if (!zeroOK) ytemp[ytemp==0] <- NA
      if (sum(!is.na(ytemp)) >= 2) {
        yreplace <- approx(x=trackdf[begrow:endrow,"timestamp.s"],
                           y=ytemp,
                           xout=trackdf[begrow:endrow,"timestamp.s"],
                           method="linear")[[2]]
        trackdf[begrow:endrow,varname] <- yreplace
      }
    }
    return(trackdf)
  }

  spdDrop <- flagdrop("speed.m.s")
  cadDrop <- flagdrop("cadence.rpm")
  hrDrop <-  flagdrop("heart_rate.bpm")
  powDrop <- flagdrop("power.watts" )

  allDrop <- spdDrop & cadDrop & hrDrop & powDrop
  if (sum(allDrop) > 0) {
    if (loud) {
      cat("   there are ",sum(allDrop)," records with no external sensor data\n")
      cat("     ",paste(sort(unique(trackdf$timestamp.s[allDrop])),sep=","),"\n")
    }
    if (fixAllSensorDrop) {
      if (sensorDropSmooth) {
        trackdf <- repairdrops(trackdf,"speed.m.s",NAOK=FALSE,
                               zeroOK=TRUE,allDrop)
        trackdf <- repairdrops(trackdf,"cadence.rpm",NAOK=FALSE,
                               zeroOK=TRUE,allDrop)
        trackdf <- repairdrops(trackdf,"heart_rate.bpm",NAOK=FALSE,
                               zeroOK=FALSE,allDrop)
        trackdf <- repairdrops(trackdf,"power.watts",NAOK=FALSE,
                               zeroOK=FALSE,allDrop)
      } else {
        if (loud) cat("    removing them\n")
        trackdf <- trackdf[!allDrop,]
      }
    }
  }
  if (sum(spdDrop) > 0) {
    if (loud) {
      cat("  ** there are ",sum(allDrop)," records with no speed data\n")
      cat("  ",paste(sort(unique(trackdf$timestamp.s[spdDrop])),sep=","),"\n")
    }
    if (fixSpeedSensorDrop) {
      trackdf$speed.m.s[spdDrop] <- 0.0
      if (loud) cat("      setting them to zero\n")
    }

  }
  if (loud & sum(cadDrop) > 0 & sum(!cadDrop) > 0)
    cat("  ** there are ",sum(cadDrop)," records with no cadence data\n")
  if (loud & sum(hrDrop) > 0 & sum(!hrDrop) > 0)
    cat("  ** there are ",sum(hrDrop)," records with no heartrate data\n")
  if (loud & sum(powDrop) > 0 & sum(!powDrop) > 0)
    cat("  ** there are ",sum(powDrop)," records with no power data\n")

  return(trackdf)
}
