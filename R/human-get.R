
#' @title Get the Time Spent Matrix
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a [matrix]
#' @export
get_timespent_matrix = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$timespent)
}

#' @title Get the Time Away Vector
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a [numeric] vector of length `nStrata`
#' @export
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
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_H = function(xds_obj, i=1){
  y = get_inits(xds_obj, flatten=TRUE)
  F_H(0, y, xds_obj, i)
}

#' @title Get orbits (**XH**)
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return the orbits for the **XH** component
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
#' + `lm` for the *Pf*PR by light microscopy
#' + `rdt` for the *Pf*PR by RDT
#' + `pcr` for the *Pf*PR by PCR
#' @note The method assigns `class(method)='method'` then
#' dispatches on `'method'`
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
#' @description  Return the true *Pf*PR
#'
#' @inheritParams get_PR
#' @return the true PR
#' @keywords internal
#' @export
get_PR.true <- function(xds_obj, method="true", i=1) {
  with(get_XH_orbits(xds_obj, i), return(list(time=time, pr=true_pr, method = "true")))
}

#' @title Get the *Pf*PR from a malaria model
#' @description Return the *Pf*PR by PCR
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
#' @description Return the PR by light microscopy
#' @inheritParams get_PR
#' @return a [list]
#' @keywords internal
#' @export
get_PR.lm<- function(xds_obj, method = "lm", i=1) {
  XH <- get_XH_orbits(xds_obj, i)
  pr = F_pfpr_by_lm(XH, xds_obj$XH_obj[[i]])
  return(list(time=XH$time, pr=pr, method="lm"))
}

#' @title Get the *Pf*PR from a malaria model
#' @description Return the PR by RDT
#' @inheritParams get_PR
#' @return a [list]
#' @keywords internal
#' @export
get_PR.rdt<- function(xds_obj, method = "rdt", i=1) {
  XH <- get_XH_orbits(xds_obj, i)
  pr = F_pfpr_by_rdt(XH, xds_obj$XH_obj[[i]])
  return(list(time=XH$time, pr=pr, method="rdt"))
}
