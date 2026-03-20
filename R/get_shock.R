
#' @title Get the shock parameters
#'
#' @description
#' Get the parameters for the perturbation function
#' forcing the model
#'
#' @note Dispatches on `forced_by`
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return shock parameters
#'
#' @export
get_shock = function(xds_obj, s=1){
  UseMethod("get_shock", xds_obj$forced_by)
}

#' @title Get the shock parameters
#'
#' @description
#' Get the parameters for the perturbation function
#'
#' @inheritParams get_shock
#'
#' @return shock parameters
#'
#' @keywords internal
#' @export
get_shock.none = function(xds_obj, s=1){
  return(c())
}

#' @title Get the shock parameters
#'
#' @description
#' Get the shock parameters for a model forced by emergence 
#'
#' @inheritParams get_shock
#'
#' @return shock parameters
#'
#' @keywords internal
#' @export
get_shock.Lambda = function(xds_obj, s=1){
  xds_obj$L_obj[[s]]$shock_par
}

#' @title Get the shock parameters
#'
#' @description
#' Get the shock parameters for an EIR forced model 
#'
#' @inheritParams get_shock
#'
#' @return shock parameters
#'
#' @keywords internal
#' @export
get_shock.eir = function(xds_obj, s=1){
  xds_obj$EIR_obj$shock_par
}

#' @title Get the shock parameters
#'
#' @description
#' Get the shock parameters for a model forced by infectious biting 
#'
#' @inheritParams get_shock
#'
#' @return shock parameters
#'
#' @keywords internal
#' @export
get_shock.MY = function(xds_obj, s=1){
  xds_obj$MY_obj[[s]]$shock_par
}
