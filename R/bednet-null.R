# specialized methods for the no_nets model of bed nets

#' @title Distribute bed nets
#' @description Implements [DistributeBedNets] for the no_nets model of bed nets (do nothing)
#' @inheritParams DistributeBedNets
#' @return a [list]
#' @export
DistributeBedNets.no_nets <- function(t, pars) {
  pars
}

#' @title Bed net ownership
#' @description Implements [OwnBedNet] for the no_nets model of bed nets (do nothing)
#' @inheritParams OwnBedNet
#' @return a [list]
#' @export
OwnBedNet.no_nets <- function(t, y, pars) {
  pars
}

#' @title Bed net ownership
#' @description Implements [UseBedNet] for the no_nets model of bed nets (do nothing)
#' @inheritParams UseBedNet
#' @return a [list]
#' @export
UseBedNet.no_nets <- function(t, y, pars) {
  pars
}

#' @title Bed net ownership
#' @description Implements [BedNetEffects] for the no_nets model of bed nets (do nothing)
#' @inheritParams BedNetEffects
#' @return a [list]
#' @export
BedNetEffects.no_nets <- function(t, pars, s) {
  pars
}

#' @title Bed net ownership
#' @description Implements [BedNetEffectSizes] for the no_nets model of bed nets (do nothing)
#' @inheritParams BedNetEffectSizes
#' @return a [list]
#' @export
BedNetEffectSizes.no_nets <- function(t, pars,s) {
  pars
}

#' @title Make parameters for the no_nets model of bed nets (do nothing)
#' @param pars a [list]
#' @return a [list]
#' @export
setup_no_nets <- function(pars) {
  ITN<- list()
  class(ITN)   <- 'no_nets'
  pars$ITNdist <- ITN
  pars$ITNown  <- ITN
  pars$ITNuse  <- ITN
  pars$ITNeff  <- ITN
  pars$ITNefsz <- ITN
  return(pars)
}
