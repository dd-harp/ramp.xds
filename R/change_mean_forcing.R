
#' @title Change mean forcing
#'
#' @description
#' Change the mean forcing parameter to `X`
#'
#' @param X the new mean forcing parameter
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#'
#' @return an **`xds`** object
#'
#' @export
change_mean_forcing = function(X, xds_obj, ix=1){
  UseMethod("change_mean_forcing", xds_obj$forced_by)
}

#' @title Change mean forcing
#'
#' @description
#' Implement `change_mean_forcing` for a model
#' with no forcing
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.none = function(X, xds_obj, ix=1){
  return(xds_obj)
}

#' @title Change mean forcing
#'
#' @description
#' Change the mean emergence rate
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.Lambda = function(X, xds_obj, ix=1){
  stopifnot(length(xds_obj$L_obj[[ix]]$Lambda) == length(X))
  xds_obj$L_obj[[ix]]$Lambda = X
  return(xds_obj)
}

#' @title Change mean forcing
#'
#' @description
#' Change the mean daily EIR 
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.eir = function(X, xds_obj, ix=1){
  stopifnot(length(xds_obj$EIR_obj$eir) == length(X))
  xds_obj$EIR_obj$eir = X
  return(xds_obj)
}

#' @title Change mean forcing
#'
#' @description
#' Change the mean egg deposition rate
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.eggs = function(X, xds_obj, ix=1){
  stopifnot(length(xds_obj$MY_obj[[ix]]$eggs) == length(X))
  xds_obj$MY_obj[[ix]]$eggs = X
  return(xds_obj)
}


#' @title Change mean forcing
#'
#' @description
#' Change mean infectious biting rate, \eqn{fqZ}
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.fqZ = function(X, xds_obj, ix=1){
  stopifnot(length(xds_obj$MY_obj[[ix]]$fqZ) == length(X))
  xds_obj$MY_obj[[ix]]$fqZ = X
  return(xds_obj)
}


#' @title Change mean forcing
#'
#' @description
#' Change mean net infectiousness (NI)
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.kappa = function(X, xds_obj, ix=1){
  stopifnot(length(xds_obj$XH_obj[[ix]]$kappa) == length(X))
  xds_obj$XH_obj[[ix]]$kappa = X
  return(xds_obj)
}

