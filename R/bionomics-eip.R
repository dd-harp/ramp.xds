# X
# generic methods to compute the extrinsic incubation period (EIP)

#' @title Set up bionomic port for extrinsic incubation period (**MY**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the extrinsic incubation period
#' for a static model.
#'
#' @param eip the extrinsic incubation period in days
#' @param MY_obj an **`MY`** module object
#'
#' @return an **`MY`** module object
#'
#' @keywords internal
#' @export
setup_eip_obj = function(eip, MY_obj){
  MY_obj$eip = eip
  MY_obj$eip_t = eip
  MY_obj$es_eip = rep(1, length(eip))
  MY_obj$eip_obj <- list()
  class(MY_obj$eip_obj) <- "static"
  MY_obj$eip_obj$eip <- eip
  MY_obj$eip_obj$dF_eip <- F_zero
  return(MY_obj)
}

#' @title Compute the EIP (**MY**)
#'
#' @description Compute the extrinsic incubation period.
#'
#' @param t time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_eip <- function(t, xds_obj, s){
  UseMethod("F_eip", xds_obj$MY_obj[[s]]$eip_obj)
}

#' @title Static model for the EIP (**MY**)
#'
#' @description Return the extrinsic incubation period for a static model.
#'
#' @inheritParams F_eip
#'
#' @return \eqn{eip}, the extrinsic incubation period in days
#' @keywords internal
#' @export
F_eip.static <- function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$eip_obj$eip)
}

#' @title Compute the EIP derivative (**MY**)
#'
#' @description Compute the time derivative of the extrinsic incubation period.
#'
#' @param t time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#'
#' @return the time derivative of the extrinsic incubation period; zero for a static model
#'
#' @keywords internal
#' @export
dF_eip <- function(t, xds_obj, s){
  UseMethod("dF_eip", xds_obj$MY_obj[[s]]$eip_obj)
}

#' @title Time derivative of the EIP for a static model (**MY**)
#'
#' @description Return zero because the extrinsic incubation period is constant.
#'
#' @inheritParams dF_eip
#'
#' @return zero
#' @keywords internal
#' @export
dF_eip.static <- function(t, xds_obj, s){
  return(0)
}


#' @title Check the extrinsic incubation period
#'
#' @description Check that EIP values are numeric, non-negative, and have
#' length `nPatches`.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the validated extrinsic incubation period in days
#' @export
check_eip = function(xds_obj, s=1){
  eip <- F_eip(0, xds_obj, s)
  stopifnot(is.numeric(eip))
  stopifnot(eip >= 0)
  stopifnot(length(eip) == xds_obj$nPatches)
  return(eip)
}

#' @title Change the extrinsic incubation period (**MY**)
#'
#' @description Update the extrinsic incubation period for the \eqn{s^{th}}
#' vector species.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
change_eip = function(xds_obj, s=1){
  eip <- check_eip(xds_obj, s)
  xds_obj$MY_obj[[s]]$eip_obj$eip <- eip
  xds_obj$MY_obj[[s]]$eip <- eip
  xds_obj$MY_obj[[s]]$eip_t <- eip
  class(xds_obj$MY_obj[[s]]$eip_obj) <- "static"
  return(xds_obj)
}

#' @title Set up the extrinsic incubation period (**MY**)
#'
#' @description Configure and validate the EIP strategy.
#'
#' @param name an EIP value or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_eip = function(name, xds_obj, options=list(), s=1){
  xds_obj <- setup_F_eip(name, xds_obj, options, s)
  xds_obj <- change_eip(xds_obj, s)
  return(xds_obj)
}

#' @title Configure the extrinsic incubation period function (**MY**)
#'
#' @description Configure the function used to compute the extrinsic incubation period.
#'
#' @param name an EIP value or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_F_eip = function(name, xds_obj, options=list(), s=1){
  if(is.numeric(name)) class(options) <- "as_is"
  if(is.character(name)) class(options) <- name
  UseMethod("setup_F_eip", options)
}

#' @title Set up F_eip (**MY**)
#'
#' @description Set up extrinsic incubation periods using an options list.
#'
#' @note If an options list is passed as the first argument, use its
#' `name` field to select the setup method. If `name` is NULL, use the
#' `no_setup` method. Pass the list as `options` to [setup_F_eip].
#'
#' @inheritParams setup_F_eip
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_eip.list = function(name, xds_obj, options=list(), s=1){
  options <- name
  Feip_name <- name$name
  if(is.null(Feip_name)) Feip_name <- "no_setup"
  xds_obj <- setup_F_eip(Feip_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up F_eip (no setup) (**MY**)
#'
#' @description Leave the existing EIP configuration unchanged.
#' @inheritParams setup_F_eip
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_eip.no_setup = function(name, xds_obj, options=list(), s=1){
  return(xds_obj)
}

#' @title Set up static EIP values (**MY**)
#'
#' @description Set up supplied static extrinsic incubation periods from `options$eip`
#' or a numeric `name`. Values in `options$eip` take precedence.
#' @inheritParams setup_F_eip
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_eip.as_is = function(name, xds_obj, options=list(), s=1){
  if (is.list(options) && !is.null(options$eip)) {
    eip <- options$eip
  } else if (is.numeric(name)) {
    eip <- name
  } else {
    stop("Supply EIP values in `options$eip` or as numeric `name`.")
  }
  xds_obj$MY_obj[[s]]$eip_obj$eip <- eip
  class(xds_obj$MY_obj[[s]]$eip_obj) <- "static"
  return(xds_obj)
}

#' @title Set up random EIP values (**MY**)
#'
#' @description Configure random extrinsic incubation periods using [make_F_eip_random].
#' @inheritParams setup_F_eip
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_eip.random = function(name, xds_obj, options=list(), s=1){
  eip_obj <- make_F_eip_random(xds_obj$nPatches, options)
  xds_obj$MY_obj[[s]]$eip_obj <- eip_obj
  return(xds_obj)
}

#' @title Make random EIP configuration (**MY**)
#'
#' @description Draw extrinsic incubation periods once during setup.
#' Subsequent evaluations reuse these values.
#'
#' @note The values are stored in a static port object.
#' The stored time derivative is zero.
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator called as `f_rand(nPatches, p1, p2)`
#' that returns a positive numeric vector of length `nPatches`
#' @param p1 first distribution parameter (Gamma shape by default)
#' @param p2 second distribution parameter (Gamma rate by default)
#' @return a static port object for [F_eip.static]
#' @export
make_F_eip_random = function(nPatches, options=list(), f_rand=stats::rgamma,
                             p1=12, p2=1){
  with(options, {
    eip_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    eip_obj$eip <- f_rand(nPatches, p1, p2)
    eip_obj$dF_eip <- F_zero
    class(eip_obj) <- "static"
    return(eip_obj)
  })
}
