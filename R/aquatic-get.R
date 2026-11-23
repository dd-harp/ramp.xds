# X
#' @title Get the delayed maturation parameter (**L**)
#' @description Return the stored realized delayed maturation parameter per habitat.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' This function does not evaluate ports or apply effect sizes.
#' @seealso [change_xi]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nHabitats`
#' @export
get_xi = function(xds_obj, s=1){
  UseMethod("get_xi", xds_obj$L_obj[[s]])
}

#' @title Get the density-independent mortality rate (**L**)
#' @description Return the stored realized density-independent mortality rate per habitat.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' This function does not evaluate ports or apply effect sizes.
#' @seealso [change_phi]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nHabitats`
#' @export
get_phi = function(xds_obj, s=1){
  UseMethod("get_phi", xds_obj$L_obj[[s]])
}

#' @title Get the maturation rate (**L**)
#' @description Return the stored realized maturation rate per habitat.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' This function does not evaluate ports or apply effect sizes.
#' @seealso [change_psi]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nHabitats`
#' @export
get_psi = function(xds_obj, s=1){
  UseMethod("get_psi", xds_obj$L_obj[[s]])
}

#' @title Get the slope of density-dependent mortality (**L**)
#' @description Return the stored realized slope of density-dependent mortality per habitat.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' This function does not evaluate ports or apply effect sizes.
#' @seealso [change_theta]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nHabitats`
#' @export
get_theta = function(xds_obj, s=1){
  UseMethod("get_theta", xds_obj$L_obj[[s]])
}