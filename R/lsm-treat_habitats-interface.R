
#' @title Set the treat_habitats_lsm
#' @description Set the value of exogenous variables related to
#' treat_habitats_lsm
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
TreatHabitats <- function(t, pars) {
  UseMethod("TreatHabitats", pars$lsm$treat_habitats)
}


#' @title Set up dynamic lsm
#' @description If dynamic lsm has not
#' already been set up, then turn on dynamic
#' lsm and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_treat_habitats = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_treat_habitats", name)
}
