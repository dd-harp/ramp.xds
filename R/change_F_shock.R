#' @title Change F_shock
#'
#' @description
#' Change  the shock function. The function `F` should have the form
#' \eqn{F(t,V)}.  If the variable \eqn{V} is not used, it's default value should be set to an empty list.  
#' 
#' @param F new shock function
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#'
#' @seealso [F_one]
#' 
#' @return an **`xds`** object
#'
#' @export
change_F_shock = function(F, xds_obj, ix=1){
  UseMethod("change_F_shock", xds_obj$forced_by)
}

#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "none"`
#' 
#' @inheritParams change_F_shock
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.none = function(F, xds_obj, ix=1){
  return(xds_obj)
}

#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "Lambda"`
#' 
#' @inheritParams change_F_shock
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.Lambda = function(F, xds_obj, ix=1){
  xds_obj$L_obj[[ix]]$F_shock = F
  return(xds_obj)
}

#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "eir"`
#' 
#' @inheritParams change_F_shock
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.eir = function(F, xds_obj, ix=1){
  xds_obj$EIR_obj$F_shock = F
  return(xds_obj)
}


#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "kappa"`
#' 
#' @inheritParams change_F_shock
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.kappa = function(F, xds_obj, ix=1){
  xds_obj$XH_obj[[ix]]$F_shock = F
  return(xds_obj)
}


#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "MY"`
#' 
#' @inheritParams change_F_shock
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.MY = function(F, xds_obj, ix=1){
  xds_obj$MY_obj[[ix]]$F_shock = F
  return(xds_obj)
}

