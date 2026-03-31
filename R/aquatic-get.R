
#' @title Get xi, delayed maturation 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_xi = function(xds_obj, s=1){
  UseMethod("get_xi", xds_obj$L_obj[[s]])
}

#' @title Get phi, density independent mortality 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_phi = function(xds_obj, s=1){
  UseMethod("get_phi", xds_obj$L_obj[[s]])
}

#' @title Get psi, the maturation rate 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_psi = function(xds_obj, s=1){
  UseMethod("get_psi", xds_obj$L_obj[[s]])
}

#' @title Get theta, density dependent mortality 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_theta = function(xds_obj, s=1){
  UseMethod("get_theta", xds_obj$L_obj[[s]])
}