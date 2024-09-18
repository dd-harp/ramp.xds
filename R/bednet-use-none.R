
#' @title Set no use_bednets
#' @description The null model for use_bednets
#' @inheritParams UseBedNets
#' @return [list]
#' @export
UseBedNets.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no use_bednets"
#' @inheritParams setup_use_bednets
#' @return an **`xds`** object
#' @export
setup_use_bednets.none <- function(name, pars, opts) {
  use <- 'none'
  class(use) <- 'none'
  pars$bednets$use <- use
  return(pars)
}
