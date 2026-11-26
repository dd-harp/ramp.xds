# X





get_time_away = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$time_away)
}

#' @title Get Blood Feeding Search Weights
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param i the host species index
#'
#' @return a [numeric] vector of length `nStrata`
#' @export
get_search_weights_blood = function(xds_obj, s=1, i=1){
  return(xds_obj$XH_obj[[i]]$search_weights[[s]])
}

#' @title Get human population density 
#'
#' @param xds_obj an **`xds`** model object
#' @param i the human species index
#'
#' @return human population density as a [numeric] vector
#' @export
get_H = function(xds_obj, i=1){
  y = get_inits(xds_obj, flatten=TRUE)
  F_H(0, y, xds_obj, i)
}
#' @title Get orbits (**XH**)
#'
#' @description Return the saved, parsed orbits (trajectories) for **XH**
#' for the selected species after solving.
#' @note Orbits are stored at `xds_obj$outputs$orbits$XH[[i]]`.
#' The returned list includes `time` from `xds_obj$outputs$time`.
#' This function does not parse outputs
#' or modify the model object.
#' @seealso [parse_XH_orbits], [xds_solve]
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a [list] of module-specific orbits (trajectories) with a `time` vector
#'
#' @export
get_XH_orbits = function(xds_obj, i=1){

  got = xds_obj$outputs$orbits$XH[[i]]
  got$time = xds_obj$outputs$time

  return(got)
}

#' @title Get the *Pf*PR from a malaria model
#' @description `'method'` should be
#' + `true` for the true *Pf*PR (default)
#' + `obs` for observed prevalence
#' + `lm` for the *Pf*PR by light microscopy
#' + `rdt` for the *Pf*PR by RDT
#' + `pcr` for the *Pf*PR by PCR
#' @note The method assigns `class(method)=method` then
#' dispatches on `method`.
#' @param xds_obj an **`xds`** model object
#' @param method the method used for computing *Pf*PR
#' @param i the host species index
#' @return a [list]
#' @export
get_PR <- function(xds_obj, method="true", i=1) {
  class(method) = method
  UseMethod("get_PR", method)
}

#' @title Get the *Pf*PR from a malaria model
#' @description Return the true *Pf*PR.
#'
#' @inheritParams get_PR
#' @return a [list] containing `time`, `pr`, and `method`
#' @keywords internal
#' @export
get_PR.true <- function(xds_obj, method="true", i=1) {
  with(get_XH_orbits(xds_obj, i), return(list(time=time, pr=true_pr, method = "true")))
}

#' @title Get the *Pf*PR from a malaria model
#' @description Return the *Pf*PR by PCR.
#' @inheritParams get_PR
#' @return a [list]
#' @keywords internal
#' @export
get_PR.pcr<- function(xds_obj, method="pcr", i=1) {
  XH <- get_XH_orbits(xds_obj, i)
  pr = F_pfpr_by_pcr(XH, xds_obj$XH_obj[[i]])
  return(list(time=XH$time, pr=pr, method="pcr"))
}

#' @title Get the *Pf*PR from a malaria model
#' @description Return the PR by light microscopy.
#' @inheritParams get_PR
#' @return a [list]
#' @keywords internal
#' @export
get_PR.lm<- function(xds_obj, method = "lm", i=1) {
  XH <- get_XH_orbits(xds_obj, i)
  pr = F_pfpr_by_lm(XH, xds_obj$XH_obj[[i]])
  return(list(time=XH$time, pr=pr, method="lm"))
}

#' @title Get observed prevalence
#' @description Return observed prevalence using [F_pr_obs].
#' @inheritParams get_PR
#' @return a [list] containing `time`, `pr`, and `method`
#' @keywords internal
#' @export
get_PR.obs <- function(xds_obj, method = "obs", i=1) {
  XH <- get_XH_orbits(xds_obj, i)
  pr = F_pr_obs(XH, xds_obj$XH_obj[[i]])
  return(list(time=XH$time, pr=pr, method="obs"))
}

#' @title Get the *Pf*PR from a malaria model
#' @description Return the PR by RDT.
#' @inheritParams get_PR
#' @return a [list]
#' @keywords internal
#' @export
get_PR.rdt<- function(xds_obj, method = "rdt", i=1) {
  XH <- get_XH_orbits(xds_obj, i)
  pr = F_pfpr_by_rdt(XH, xds_obj$XH_obj[[i]])
  return(list(time=XH$time, pr=pr, method="rdt"))
}
