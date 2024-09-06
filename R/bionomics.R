
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
BaselineBionomics <- function(t, y, pars){
  pars <- AvailableSugar(pars)
  for(s in 1:pars$nVectors){
    pars <- MBaseline(t, y, pars, s)
    pars <- LBaseline(t, y, pars, s)
  }
  return(pars)
}

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

#' @title Set mosquito bionomics to baseline
#' @description Implements [MBionomics] for models with no forcing
#' @inheritParams MBionomics
#' @return an `xds` object
#' @export
MBionomics.static <- function(t, y, pars, s) {
  return(pars)
}

#' @title Set mosquito bionomics to baseline
#' @description Implements [LBionomics] for models with no forcing
#' @inheritParams LBionomics
#' @return the model as a [list]
#' @export
LBionomics.static <- function(t, y, pars, s) {
  return(pars)
}
