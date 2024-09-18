#' @title Set up sugar bait coverage function
#' @description Set up sugar bait coverage function
#' @inheritParams SugarBaitCoverage
#' @return [list]
#' @export
SugarBaitCoverage.func <- function(t, pars) {with(pars$sugar_bait_coverage,{
  pars$vars$sugar_bait_coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
})}

#' @title Set up dynamic sugar bait coverage function
#' @description Set up a dynamic sugar bait
#' coverage function
#' @inheritParams setup_sugar_bait_coverage
#' @export
setup_sugar_bait_coverage.func = function(name, pars, opts=list()){
  setup_sugar_bait_coverage_func(pars, opts())
}

#' @title Set up dynamic sugar bait coverage function
#' @description Set up dynamic sugar bait coverage function
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean the mean sugar_bait_coverage
#' @param F_season the seasonal signal in sugar_bait coverage
#' @param F_trend a temporal trend in sugar_bait coverage
#' @return an **`xds`** object
#' @export
setup_sugar_bait_coverage_func = function(pars, opts=list(),
                                      mean=0,
                                      F_season=F_flat,
                                      F_trend=F_flat){
  pars = dynamic_vector_control(pars)
  coverage <- list()
  class(coverage) <- 'func'
  coverage$mean <- mean
  coverage$F_season <- F_season
  coverage$F_trend <- F_trend
  pars$sugar_baits$coverage <- coverage
  return(pars)
}
