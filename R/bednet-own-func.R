
#' @title Set no own_bednets
#' @description The null model for own_bednets
#' @inheritParams OwnBedNets
#' @return [list]
#' @export
OwnBedNets.func <- function(t, pars) {with(pars$own_bednets,{
  pars$vars$bednet_ownership = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_own_bednets
#' @export
setup_own_bednets.func = function(name, pars, opts=list()){
  pars = setup_own_bednets_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean mean bednet ownership
#' @param F_season the seasonal signal in bednet ownership
#' @param F_trend a temporal trend in bednet ownership
#' @return an **`xds`** object
#' @export
setup_own_bednets_func = function(pars, opts=list(), mean=0.7, F_season=F_flat, F_trend=F_flat){
  own <- list()
  class(own) <- 'func'
  own$mean <- mean
  own$F_season <- F_season
  own$F_trend <- F_trend
  pars$bednets$own <- own
  return(pars)
}
