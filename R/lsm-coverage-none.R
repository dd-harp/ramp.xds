
#' @title Set no lsm_coverage
#' @description The null model for lsm_coverage
#' @inheritParams LSMCoverage
#' @return [list]
#' @export
LSMCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no lsm_coverage"
#' @inheritParams setup_lsm_coverage
#' @return an **`xds`** object
#' @export
setup_lsm_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$lsm$coverage <- coverage
  return(pars)
}
