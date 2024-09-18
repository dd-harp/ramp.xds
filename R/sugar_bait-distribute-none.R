
#' @title Distribute no sugar baits
#' @description The model for no sugar bait distribution
#' @inheritParams DistributeSugarBaits
#' @return [list]
#' @export
DistributeSugarBaits.none <- function(t, pars) {
  return(pars)
}

#' @title Set up distribution for the no sugar baits model
#' @inheritParams setup_distribute_sugar_baits
#' @return an **`xds`** object
#' @export
setup_distribute_sugar_baits.none <- function(name, pars, opts) {
  distribute_sugar_baits <- 'none'
  class(distribute_sugar_baits) <- 'none'
  pars$sugar_baits$distribute <- distribute_sugar_baits
  return(pars)
}
