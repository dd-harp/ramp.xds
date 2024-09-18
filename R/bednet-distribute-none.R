
#' @title Set no distribute_bednets
#' @description The null model for distribute_bednets
#' @inheritParams DistributeBedNets
#' @return [list]
#' @export
DistributeBedNets.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no distribute_bednets"
#' @inheritParams setup_distribute_bednets
#' @return an **`xds`** object
#' @export
setup_distribute_bednets.none <- function(name, pars, opts) {
  distribute <- 'none'
  class(distribute) <- 'none'
  pars$bednets$distribute <- distribute
  return(pars)
}
