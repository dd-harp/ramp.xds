# X

#' @title Get mean forcing
#'
#' @description
#' Get the parameter(s) that set mean forcing.
#'
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#'
#' @return the mean forcing parameter 
#'
#' @export
get_mean_forcing = function(xds_obj, ix=1){
  UseMethod("get_mean_forcing", xds_obj$forced_by)
}

#' @title Get mean forcing
#'
#' @description
#' Return `NULL` for models without forcing.
#'
#' @inheritParams get_mean_forcing
#'
#' @return `NULL`
#'
#' @keywords internal
#' @export
get_mean_forcing.none = function(xds_obj, ix=1){
  return(c())
}

#' @title Get mean forcing
#'
#' @description
#' Get `Lambda`, the mean emergence
#' rate(s) \eqn{\Lambda}, for a model
#' forced by adult mosquito emergence.
#'
#' @inheritParams get_mean_forcing
#'
#' @return a [vector], the value of `Lambda` for the indexed vector species
#'
#' @keywords internal
#' @export
get_mean_forcing.Lambda = function(xds_obj, ix=1){
  return(xds_obj$L_obj[[ix]]$Lambda)
}

#' @title Get mean forcing
#'
#' @description
#' Get the mean daily EIR for an `eir` model.
#'
#' @inheritParams get_mean_forcing
#'
#' @return a vector, the value of `eir`
#'
#' @keywords internal
#' @export
get_mean_forcing.eir = function(xds_obj, ix=1){
  return(xds_obj$EIR_obj$eir)
}

#' @title Get mean forcing
#'
#' @description
#' Get mean egg deposition rates
#' for a trivial **MY** model.
#'
#' @inheritParams get_mean_forcing
#'
#' @return a [vector] of mean egg deposition rates for the indexed vector species
#' @keywords internal
#' @export
get_mean_forcing.eggs = function(xds_obj, ix=1){
  return(xds_obj$MY_obj[[ix]]$eggs)
}


#' @title Get mean forcing
#'
#' @description
#' Get the mean infective biting rate
#' for a trivial **MY** model.
#'
#' @inheritParams get_mean_forcing
#'
#' @return a [vector] of mean infective biting rates for the indexed vector species
#' @keywords internal
#' @export
get_mean_forcing.fqZ = function(xds_obj, ix=1){
  return(xds_obj$MY_obj[[ix]]$fqZ)
}

#' @title Get mean forcing
#'
#' @description
#' Get net infectiousness.
#'
#' @inheritParams get_mean_forcing
#'
#' @return a [vector] of net infectiousness for the indexed host species
#' @keywords internal
#' @export
get_mean_forcing.kappa = function(xds_obj, ix=1){
  return(xds_obj$XH_obj[[ix]]$kappa)
}