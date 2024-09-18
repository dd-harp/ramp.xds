
#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams BedNetCoverage
#' @return [list]
#' @export
BedNetCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no bednet_coverage"
#' @inheritParams setup_bednet_coverage
#' @return an **`xds`** object
#' @export
setup_bednet_coverage.none <- function(name, pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$bednets$coverage <- coverage
  return(pars)
}
