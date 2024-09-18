
#' @title Set no use_bednets
#' @description The null model for use_bednets
#' @inheritParams UseBedNets
#' @return [list]
#' @export
UseBedNets.func <- function(t, pars) {with(pars$use_bednets,{
  pars$vars$bednet_useage = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_use_bednets
#' @export
setup_use_bednets.func = function(name, pars, opts=list()){
  pars = setup_use_bednets_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean mean bednet useership
#' @param F_season the seasonal signal in bednet useership
#' @param F_trend a temporal trend in bednet useership
#' @return an **`xds`** object
#' @export
setup_use_bednets_func = function(pars, opts=list(), mean=0.7, F_season=F_flat, F_trend=F_flat){
  use <- list()
  class(use) <- 'func'
  use$mean <- mean
  use$F_season <- F_season
  use$F_trend <- F_trend
  pars$bednets$use <- use
  return(pars)
}
