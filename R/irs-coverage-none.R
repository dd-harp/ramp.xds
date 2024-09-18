
#' @title Set no irs_coverage
#' @description The null model for irs_coverage
#' @inheritParams IRSCoverage
#' @return [list]
#' @export
IRSCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no irs_coverage"
#' @inheritParams setup_irs_coverage
#' @return an **`xds`** object
#' @export
setup_irs_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$irs$coverage <- coverage
  return(pars)
}
