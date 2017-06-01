.onLoad <- function(libname, pkgname) {
  op <- options()
  op.bikeCadHr <- list(
    bCadHr.path = "~/R-dev",
    bCadHr.install.args = "",
    bCadHr.name = "Craig Mohn",
    bCadHr.desc.author = '"Craig MOhn <craig.mohn.projects@gmail.com> [aut, cre]"',
    bCadHr.desc.license = "GPL 3",
    bCadHr.desc.suggests = NULL,
    bCadHr.desc = list(),
    bCadHr.fit.fn.time.parse = "%Y-%m-%d-%H-%M-%S",
    bCadHr.fit.fn.lead = "",
    bCadHr.fit.fn.trail = "",
    bCadHr.gpx.fn.time.parse = "%m_%d_%Y %I_%M_%S %p",
    bCadHr.gpx.fn.lead = "",
    bCadHr.gpx.fn.trail = "_history"
  )
  toset <- !(names(op.bikeCadHr) %in% names(op))
  if(any(toset)) options(op.bikeCadHr[toset])

  invisible()
}
