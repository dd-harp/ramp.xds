
#' @title Change mean forcing
#'
#' @description
#' Change the mean forcing parameter to `X`
#'
#' @param X the new mean forcing parameter
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
change_mean_forcing = function(X, xds_obj, s=1){
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
change_mean_forcing.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Change mean forcing
#'
#' @description
#' Change `Lambda` for when `forced_by` = "Lambda"
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.Lambda = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$L_obj[[s]]$Lambda) == length(X))
  xds_obj$L_obj[[s]]$Lambda = X
  return(xds_obj)
}

#' @title Change mean forcing
#'
#' @description
#' Change the mean daily EIR for an `eir` model
#'
#' @inheritParams change_mean_forcing
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_mean_forcing.eir = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIR_obj$eir) == length(X))
  xds_obj$EIR_obj$eir = X
  return(xds_obj)
}


