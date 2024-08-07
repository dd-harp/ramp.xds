
#' @title Set bionomic parameter rates relative to baseline
#' @description This calls Mbionomics and Lbionmics for each species. This function
#' resets bionomic parameters to their pre-control baseline value, which can later be
#' modified by vector control. In some models, the pre-control baseline is computed in
#' here as a function of resource availability.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return a [list]
#' @export
Bionomics <- function(t, y, pars){
  pars <- AvailableSugar(pars)
  for(s in 1:pars$nVectors){
    pars <- MBionomics(t, y, pars, s)
    pars <- LBionomics(t, y, pars, s)
  }
  return(pars)
}

#' @title Adult Mosquito Bionomics
#' @description Handle adult mosquito bionomics as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
MBionomics <- function(t, y, pars, s) {
  UseMethod("MBionomics", pars$MYZpar[[s]]$baseline)
}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing
#' @inheritParams MBionomics
#' @return an `xds` object
#' @export
MBionomics.static <- function(t, y, pars, s) {
  return(pars)
}

#' @title Larval and Aquatic Stage Bionomics
#' @description Handle immature mosquito bionomic parameters as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
LBionomics <- function(t, y, pars, s) {
  UseMethod("LBionomics", pars$Lpar[[s]]$baseline)
}

#' @title Set mosquito bionomics to baseline
#' @description Implements [LBionomics] for models with no forcing
#' @inheritParams LBionomics
#' @return the model as a [list]
#' @export
LBionomics.static <- function(t, y, pars, s) {
  return(pars)
}
