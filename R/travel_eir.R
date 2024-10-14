
#' @title Travel EIR
#' @description Compute the EIR while traveling
#' @param t the time
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return the daily travel EIR, a [numeric] vector
#' @export
travel_eir <- function(t, pars, i){
  with(pars$travel_eir[[i]], return(travelEIR*F_season(t)*F_trend(t)))
}

#' @title Setup Travel EIR
#' @description Set up a function to compute the travel EIR
#' @param pars a [list]
#' @param travelEIR the time spent traveling
#' @param F_season a function describing a seasonal pattern
#' @param F_trend a function describing a trend
#' @param i the host species index
#' @return an **`xds`** object
#' @export
setup_travel_eir = function(pars, travelEIR=0, F_season=F_flat, F_trend=F_flat, i=1){
  trv <- list()
  trv$travelEIR <- travelEIR
  trv$F_season <- F_season
  trv$F_trend <- F_trend
  pars$travel_eir[[i]] <- trv
  return(pars)
}

