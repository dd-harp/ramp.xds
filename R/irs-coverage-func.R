#' @title Set no irs_coverage
#' @description The null model for irs_coverage
#' @inheritParams IRSCoverage
#' @return [list]
#' @export
IRSCoverage.func <- function(t, pars) {with(pars$irs_coverage,{
  pars$vars$irs_coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_coverage
#' @export
setup_irs_coverage.func = function(name, pars, opts=list()){
  setup_irs_coverage_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean the mean irs_coverage
#' @param F_season the seasonal signal in irs coverage
#' @param F_trend a temporal trend in irs coverage
#' @return an **`xds`** object
#' @export
setup_irs_coverage_func = function(pars, opts=list(),
                                      mean=0,
                                      F_season=F_flat,
                                      F_trend=F_flat){
  pars = dynamic_vector_control(pars)
  coverage <- list()
  class(coverage) <- 'func'
  coverage$mean <- mean
  coverage$F_season <- F_season
  coverage$F_trend <- F_trend
  pars$irs$coverage <- coverage
  return(pars)
}
