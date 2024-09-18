
#' @title Set no area spray coverage
#' @description The null model for area spray coverage
#' @inheritParams AreaSprayCoverage
#' @return [list]
#' @export
AreaSprayCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up no area spray
#' @inheritParams setup_area_spray_coverage
#' @return an **`xds`** object
#' @export
setup_area_spray_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$area_spray$coverage <- coverage
  return(pars)
}
