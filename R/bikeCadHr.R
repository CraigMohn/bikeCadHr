#' bikeCadHr: A package for analyzing gps cycling cadence
#'  and heartrate data
#'
#' The bikeCadHr package provides three categories of functions:
#'   1) read gps .fit and .gpx files,
#'   2) organize the gps data into dataframes for later use, and
#'   3) draw maps and generate an infographic relating cadence, heartrate,
#'   speed and elevation
#'
#'  This package was motivated because my models of cycling effort and
#'  speed find cadence to be an important predictor.  Garmin fit files record
#'  a measure of the cadence with each sample, and contain an overall summary in
#'  the session summary section.  In addition to creating several measures of
#'  average cadence, this package creates summary variables counting and categorizing
#'  the number of stops on the ride.
#'
#'  The package also allows for the creation of maps of a ride or set of rides.
#'  The map area will be slightly larger than the track of the ride, unless you supply
#'  a named list of map lat/long boundaries. If a specific map is not specified from
#'  the list, it will choose the first in the list that covers the track.  The
#'  map is created using the OpenStreetMap package, and there are many choices for
#'  the map format.  Respect the terms of use and do not automate the generation
#'  of maps.
#'
#'  The package will also create a plot of the elevation profile of a ride, with
#'  speed encoded as the color of the profile line, and color bars below showing
#'  heart rate and cadence range over the ride.  This may be easier to quickly
#'  interpret than the superposed wiggly lines that are usually displayed in
#'  cycling performance apps.
#'
#'  Currently there is no support for power meters, although the data should be read
#'  from fit files and passed upstream unchanged.
#'
#'  Very short .fit files (typically for rides less than 30 seconds) sometimes fail to
#'  be read in.  It is possible to specify a vector of files to exclude to avoid this
#'  situation, and by default .fit filenames containing the strings "short" or "bad" are
#'  ignored, as are .gpx filenames containing those substrings.  The string "nosegs" will
#'  also (by default) cause a .gpx file to be skipped.
#'
#'
#'
#' @section import gps data:
#'   \link{read_ridefiles}, \link{read_ride}
#'
#' @section organize imported gps data:
#'   \link{update_gps_variables}, \link{join_ridefiles}, \link{combine_track_stores}
#'
#' @section map and visualization:
#'   \link{map_rides}
#'
#'
#' @import magrittr tibble dplyr stringr
#' @import maptrack3d
#' @import maptrack2d
#' @import rideProfile
#' @import rideReadGPS
#' @importFrom stats approx median quantile setNames weighted.mean
#' @importFrom raster pointDistance
#'
#' @name bikeCadHr
NULL

###  make the R checker happy
tedious <- utils::globalVariables(c("alphachar","alphahour","distlegend",
                                    "prtchar","prthour","start.hour","start.time",
                                    "startbutton.date","stoplabels","timestamp.s",
                                    "distance.m","segment","timestamp",
                                    "x","xtext.stop","y","xend","xcol",
                                    "verticalMultiplier","color","hjust","label",
                                    "yend","group","timeBeg","timeEnd","pauseSize",
                                    "timelaststop","segbegtime","segendtime",
                                    "movingrun","maxdist","startofstop","sosNA",
                                    "joinseg","subsegment"))


