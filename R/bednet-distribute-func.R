
#' @title Set no distribute_bednets
#' @description The null model for distribute_bednets
#' @inheritParams DistributeBedNets
#' @return [list]
#' @export
DistributeBedNets.func <- function(t, pars) {
  #F_distribute_bednets(t, pars)
  return(pars)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @inheritParams setup_distribute_bednets
#' @export
setup_distribute_bednets.func = function(name="func", pars, opts=list()){
  pars <- setup_distribute_bednets_func(pars, opts)
}

#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bed net  and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param eventT the time when a distribute occurs
#' @param F_distribute the effects of the distribute
#' @return an **`xds`** object
#' @export
setup_distribute_bednets_func = function(pars, opts=list(), eventT=365, F_distribute=NULL){
  distribute <- list()
  class(distribute) <- 'func'
  distribute$eventT = eventT
  distribute$F_distribute = F_distribute
  pars$bednets$distribute <- distribute
  return(pars)
}
