
#' @title Set no own_bednets
#' @description The null model for own_bednets
#' @inheritParams OwnBedNets
#' @return an **`xds`** object
#' @export
OwnBedNets.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no own_bednets"
#' @inheritParams setup_own_bednets
#' @return an **`xds`** object
#' @export
setup_own_bednets.none <- function(name, pars, opts) {
  own <- 'none'
  class(own) <- 'none'
  pars$bednets$own <- own
  return(pars)
}
