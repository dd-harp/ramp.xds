#' @title Set no lsm_coverage
#' @description The null model for lsm_coverage
#' @inheritParams LSMCoverage
#' @return [list]
#' @export
LSMCoverage.func <- function(t, pars) {with(pars$lsm_coverage,{
  pars$vars$lsm_coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_lsm_coverage
#' @export
setup_lsm_coverage.func = function(name, pars, opts=list()){
  setup_lsm_coverage_func(pars, opts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean the mean lsm_coverage
#' @param F_season the seasonal signal in lsm coverage
#' @param F_trend a temporal trend in lsm coverage
#' @return an **`xds`** object
#' @export
setup_lsm_coverage_func = function(pars, opts=list(),
                                      mean=0,
                                      F_season=F_flat,
                                      F_trend=F_flat){
  pars = dynamic_vector_control(pars)
  coverage <- list()
  class(coverage) <- 'func'
  coverage$mean <- mean
  coverage$F_season <- F_season
  coverage$F_trend <- F_trend
  pars$lsm$coverage <- coverage
  return(pars)
}
