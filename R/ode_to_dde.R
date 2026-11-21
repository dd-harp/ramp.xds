# X

#' @title Compute as DDE
#'
#' @description Set up a differential equation model
#' to be solved using `deSolve::dede`.
#'
#' @details
#' For differential equations, the value of `xds_obj$xde`
#' is set to `ode` by default. This utility is called during setup to change
#' `xds_obj$xde` from `ode` to `dde` when a module requires a delay.
#'
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @export
ode_to_dde = function(xds_obj){
  UseMethod("ode_to_dde", xds_obj$xde)
}

#' @title Compute as DDE
#' @description If `class(xds_obj$xde) == "dts"`, don't change anything.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @keywords internal
#' @export
ode_to_dde.dts = function(xds_obj){
  return(xds_obj)
}

#' @title Compute as DDE
#' @description Switch to `dde` if any module's skill set indicates a delay.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @keywords internal
#' @export
ode_to_dde.ode = function(xds_obj){
  delay = FALSE
  for(i in 1:xds_obj$nHostSpecies)
    if(get_skillset_XH(xds_obj, i)$delay == TRUE) delay=TRUE
  for(s in 1:xds_obj$nVectorSpecies){
    if(get_skillset_MY(xds_obj, s)$delay == TRUE) delay=TRUE
    if(get_skillset_L(xds_obj, s)$delay == TRUE) delay=TRUE
  } 
  if(delay==TRUE){
    xds_obj$xde = "dde"
    class(xds_obj$xde) = "dde"
  }
  return(xds_obj)
}

#' @title Compute as DDE
#' @description Leave `dde` unchanged.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @keywords internal
#' @export
ode_to_dde.dde = function(xds_obj){
  return(xds_obj)
}
