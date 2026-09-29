# generic methods to compute the extrinsic incubation period (EIP)

#' @title Set up the extrinsic incubation period object
#'
#' @description Set up an object
#' to compute the EIP
#'
#' @param eip the extrinsic incubation period in days
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_eip_obj = function(eip, MY_obj){
  MY_obj$eip = eip
  MY_obj$eip_t = eip
  MY_obj$eip_obj <- list()
  class(MY_obj$eip_obj) <- "static"
  MY_obj$eip_obj$eip <- eip
  MY_obj$eip_obj$dF_eip <- F_zero
  return(MY_obj)
}

#' @title Compute the EIP
#'
#' @description This method dispatches on the type of `eip_obj` and computes
#' the extrinsic incubation period.
#'
#' @param t current simulation time
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

#' @title Static model for the EIP
#'
#' @description Implements [F_eip] for a static model
#'
#' @inheritParams F_eip
#'
#' @return \eqn{eip}, the extrinsic incubation period in days
#' @keywords internal
#' @export
F_eip.static <- function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$eip_obj$eip)
}

#' @title Compute the EIP derivative
#'
#' @description This method dispatches on the type of `eip_obj` and computes
#' the extrinsic incubation period.
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
dF_eip <- function(t, xds_obj, s){
  UseMethod("F_eip", xds_obj$MY_obj[[s]]$eip_obj)
}

#' @title Static model for the EIP
#'
#' @description Implements [F_eip] for a static model
#'
#' @inheritParams F_eip
#'
#' @return \eqn{eip}, the extrinsic incubation period in days
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

#' @title Change the extrinsic incubation period
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

#' @title Set up the extrinsic incubation period
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

#' @title Set up F_eip
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

#' @title Set up F_eip from options
#'
#' @description If an options list is passed as the first argument, use its
#' `name` field to select the setup method.
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

#' @title Set up no F_eip
#'
#' @description Leave the existing EIP configuration unchanged.
#' @inheritParams setup_F_eip
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_eip.no_setup = function(name, xds_obj, options=list(), s=1){
  return(xds_obj)
}

#' @title Set up static EIP values
#'
#' @description Set up supplied static extrinsic incubation periods.
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

#' @title Set up random EIP values
#'
#' @description Configure random extrinsic incubation periods.
#' @inheritParams setup_F_eip
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_eip.random = function(name, xds_obj, options=list(), s=1){
  dF_eip <- xds_obj$MY_obj[[s]]$eip_obj$dF_eip
  eip_obj <- make_F_eip_random(xds_obj$nPatches, options)
  eip_obj$dF_eip <- dF_eip
  xds_obj$MY_obj[[s]]$eip_obj <- eip_obj
  return(xds_obj)
}

#' @title Compute random extrinsic incubation periods
#'
#' @description Generate patch-level EIP values with the configured positive
#' random number generator.
#' @inheritParams F_eip
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eip.random = function(t, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$eip_obj, {
    eip <- f_rand(xds_obj$nPatches, p1, p2)
    return(eip)
  })
}

#' @title Make random EIP configuration
#'
#' @description Build a configuration object for random EIP values.
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a positive random number generator
#' @param p1 first distribution parameter (Gamma shape by default)
#' @param p2 second distribution parameter (Gamma rate by default)
#' @return a configuration object for [F_eip.random]
#' @export
make_F_eip_random = function(nPatches, options=list(), f_rand=stats::rgamma,
                             p1=12, p2=1){
  with(options, {
    eip_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    class(eip_obj) <- "random"
    return(eip_obj)
  })
}


