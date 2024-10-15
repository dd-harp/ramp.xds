
#' @title Time Spent Traveling
#' @description This function sets the value of a parameter
#' describing time spent traveling. It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @param t current simulation time
#' @param pars a [list]#
#' @param i host species index
#' @return an **`xds`** object
#' @export
traveling <- function(t, pars, i) {
  UseMethod("traveling", pars$travel[[i]])
}

#' @title Time Spent Traveling
#' @description For a static model for time spent traveling, the function
#' does not update anything.
#' @inheritParams traveling
#' @return an **`xds`** object
#' @export
traveling.static <- function(t, pars, i) {
  return(pars)
}

#' @title Time Spent Traveling
#' @description This function sets a static value for the parameter
#' describing time spent traveling, resets the class to `static` and
#' then triggers an update to the blood feeding model.
#' It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams traveling
#' @return an **`xds`** object
#' @export
traveling.setup <- function(t, pars, i) {
  pars$time_traveling[[i]] <- pars$travel[[i]]$traveling_fraction
  class(pars$travel[[i]]) <- "static"
  pars <- trigger_setup(pars$BFpar)
  return(pars)
}

#' @title Time Spent Traveling
#' @description This function sets the value of a parameter
#' describing time spent traveling. It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams traveling
#' @return an **`xds`** object
#' @export
traveling.dynamic <- function(t, pars, i) {
  with(pars$travel[[i]],{
    pars$time_traveling[[i]] = traveling*F_season(t)*F_trend(t)
    return(pars)
  })
}

#' @title Set up no travel
#' @description Setup a static model for time spent traveling with
#' no time spent traveling
#' @param pars a [list]
#' @param i the host species index
#' @return a [list]
#' @export
setup_no_travel = function(pars, i=1){
  trv <- list()
  class(trv) <- "static"
  trv$traveling_fraction <- 0
  pars$travel[[i]] <- trv
  return(pars)
}

#' @title Set up static travel
#' @description Setup a static model for time spent traveling
#' @title A function to set up malaria importation
#' @description Setup a static model for travel malaria
#' @param pars an **`xds`** object
#' @param traveling_fraction the fraction of time spent traveling
#' @param i the host species index
#' @return a [list]
#' @export
setup_static_travel = function(pars, traveling_fraction=0, i=1){
  pars <- setup_dynamic_travel(pars, traveling_fraction)
  pars$time_traveling[[i]] <- traveling_fraction
  class(pars$travel[[i]]) <- "static"
  return(pars)
}

#' @title Set up a dynamic model for travel
#' @description Setup a dynamical model for time spent traveling
#' @param pars a [list]
#' @param traveling the time spent traveling
#' @param F_season a function describing a seasonal pattern
#' @param F_trend a function describing a trend
#' @param i the host species index
#' @return a [list]
#' @export
setup_dynamic_travel = function(pars, traveling=0, F_season=F_flat, F_trend=F_flat, i=1){
  trv <- list()
  class(trv) <- "dynamic"
  class(pars$BFpar) <- "dynamic"
  trv$traveling <- traveling
  trv$F_season <- F_season
  trv$F_trend <- F_trend
  pars$travel[[i]] <- trv
  return(pars)
}
