#' @title Adult Mosquito Bionomics
#' @description Handle adult mosquito bionomics as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' @return the modified `xds` object
#' @export
MBionomics <- function(t, y, pars, s) {
  UseMethod("MBionomics", pars$MYZpar[[s]]$baseline)
}

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing
#' @inheritParams MBionomics
#' @return the unmodified `xds` object
#' @export
MBionomics.static <- function(t, y, pars, s) {
  return(pars)
}

#' @title Larval and Aquatic Stage Bionomics
#' @description Handle immature mosquito bionomic parameters as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return the unmodified `xds` object
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

