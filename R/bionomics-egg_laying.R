# X

#' @title Set up bionomic port for oviposition rate (**MY**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the baseline oviposition rate,
#' \eqn{nu}, for a static model.
#'
#' @param nu the oviposition rate: the number of egg batches laid in each patch,
#' per mosquito, per day
#' @param MY_obj an **`MY`** module object
#'
#' @return an **`MY`** module object
#'
#' @keywords internal
#' @export
setup_nu_obj = function(nu, MY_obj){
  
  nu_obj <- list() 
  class(nu_obj) <- "static"
  nu_obj$nu=nu 
  
  MY_obj$nu_t = nu 
  MY_obj$nu = nu
  MY_obj$es_nu = 1
  MY_obj$nu_obj <- nu_obj 
  
  return(MY_obj)
}

#' @title Compute the baseline oviposition rate (**MY**)
#'
#' @description Compute the baseline oviposition rate, \eqn{\nu}.
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_nu = function(t, xds_obj, s){
  UseMethod("F_nu", xds_obj$MY_obj[[s]]$nu_obj)
}

#' @title Static model oviposition rate (**MY**)
#'
#' @description Return the baseline oviposition rate for a static model.
#'
#' @inheritParams F_nu
#'
#' @return \eqn{nu}, the baseline oviposition rate
#' 
#' @keywords internal
#' @export
F_nu.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$nu_obj$nu)
}

#' @title Check oviposition rate
#' 
#' @description
#' Check that the oviposition rates are in the expected range and length:
#' + all elements are non-negative
#' + length(F_nu) == nPatches
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the oviposition rate
#' @seealso [xds_info_egg_laying]
#' @export
check_nu = function(xds_obj, s=1){
  nu = F_nu(0, xds_obj, s)
  stopifnot(is.numeric(nu))
  stopifnot(nu >= 0)
  stopifnot(length(nu) == xds_obj$nPatches)
  return(nu)
}

#' @title Change the oviposition rate (**MY**)
#' 
#' @description
#' Update the oviposition rate for the \eqn{s^{th}} vector species.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' 
#' @export
change_nu = function(xds_obj, s=1){
  nu <- check_nu(xds_obj, s)
  xds_obj$MY_obj[[s]]$nu_obj$nu <- nu
  xds_obj$MY_obj[[s]]$nu <- nu
  xds_obj$MY_obj[[s]]$nu_t <- nu
  class(xds_obj$MY_obj[[s]]$nu_obj) <- "static"
  return(xds_obj)
}

#' @title Set up oviposition rates (**MY**)
#' 
#' @description Set up the oviposition rate (\eqn{\nu})
#' for static systems
#'
#' @param name an oviposition rate or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_nu = function(name, xds_obj, options = list(), s=1){
  xds_obj <- setup_F_nu(name, xds_obj, options, s)
  xds_obj <- change_nu(xds_obj, s)
  return(xds_obj)
}

#' @rdname setup_nu
#' @export
setup_egg_batch_rates <- setup_nu


#' @title Configure the oviposition rate function (**MY**)
#'
#' @description Configure the function used to compute the baseline oviposition rate.
#'
#' @param name an oviposition rate or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_F_nu = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_nu", options)
}

#' @title Set up F_nu (**MY**)
#'
#' @description Set up oviposition rates using an options list.
#'
#' @note If an options list is passed as the first argument, use its
#' `name` field to select the setup method. If `name` is NULL, use the
#' `no_setup` method. Pass the list as `options` to [setup_F_nu].
#'
#' @inheritParams setup_F_nu
#'
#' @return an **`xds`** model object
#' 
#' @keywords internal
#' @export
setup_F_nu.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Fnu_name = name$name
  if(is.null(Fnu_name)) Fnu_name = "no_setup"
  xds_obj <- setup_F_nu(Fnu_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up static oviposition rates (**MY**)
#'
#' @description Set up supplied static oviposition rates from `options$nu`
#' or a numeric `name`. Values in `options$nu` take precedence.
#'
#' @inheritParams setup_F_nu 
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_nu.as_is = function(name, xds_obj, options=list(), s=1){
  if (is.list(options) && !is.null(options$nu)) {
    nu <- options$nu
  } else if (is.numeric(name)) {
    nu <- name
  } else {
    stop("Supply oviposition rates in `options$nu` or as numeric `name`.")
  }

  xds_obj$MY_obj[[s]]$nu_obj$nu = nu
  xds_obj <- change_nu(xds_obj, s)
  return(xds_obj)
}

#' @title Set up F_nu (no setup) (**MY**)
#' @description Leave the existing oviposition rate configuration unchanged.
#' @inheritParams setup_F_nu
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_nu.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up random oviposition rates (**MY**)
#' 
#' @description Configure random oviposition rates using [make_F_nu_random].
#' 
#' @inheritParams setup_F_nu
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_nu.random = function(name, xds_obj, options = list(), s=1){
  nu_obj = make_F_nu_random(xds_obj$nPatches, options)
  xds_obj$MY_obj[[s]]$nu_obj = nu_obj
  return(xds_obj)
}

#' @title Make random oviposition rates (**MY**)
#' 
#' @description Draw oviposition rates once during setup.
#' Subsequent evaluations reuse these values.
#'
#' @note The values are stored in a static port object.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator called as `f_rand(nPatches, p1, p2)`
#' that returns a numeric vector of length `nPatches`
#' @param p1 argument 1 for `f_rand`
#' @param p2 argument 2 for `f_rand`
#' 
#' @return a static port object for [F_nu.static]
#' @export
make_F_nu_random = function(nPatches, options=list(), 
                            f_rand = stats::rbeta, 
                            p1=980, p2=1000){
  with(options,{
    nu_obj <- list() 
    nu_obj$p1=p1
    nu_obj$p2=p2
    nu_obj$f_rand = f_rand
    nu_obj$nu <- f_rand(nPatches, p1, p2)
    class(nu_obj) <- "static"
    return(nu_obj)
})}



#' @title Compute the baseline oviposition rate (type 2) (**MY**)
#'
#' @description Compute the baseline oviposition rate using a type 2 functional
#' response to water availability, \eqn{Q}.
#' \deqn{\nu = \nu_x \frac{s_\nu Q}{1+s_\nu Q}}  
#'
#' @inheritParams F_nu
#'
#' @return \eqn{\nu}, the baseline oviposition rate
#' 
#' @keywords internal
#' @export
F_nu.type2 = function(t, xds_obj, s){
  Q <- xds_obj$terms$Qall[[s]]
  with(xds_obj$MY_obj[[s]]$nu_obj, {
      nu = vx*sv*Q/(1+sv*Q)
      return(nu)
})}

#' @title Set up type 2 oviposition rates (**MY**)
#' 
#' @description Configure a type 2 functional response for the oviposition rate.
#' 
#' @inheritParams setup_F_nu
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_nu.type2 = function(name, xds_obj, options = list(), s=1){
  nu_obj <- make_F_nu_type2(xds_obj$nPatches, options)
  xds_obj$MY_obj[[s]]$nu_obj <- nu_obj

  return(xds_obj)
}

#' @title Make type 2 oviposition rate port (**MY**)
#' 
#' @description Build a configuration object for a type 2 oviposition rate
#' response to water availability.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
#' @param vx the maximum oviposition rate: the maximum number of egg batches
#' laid in each patch, per mosquito, per day
#' @param sv the shape parameter
#' @return a configuration object for [F_nu.type2]
#' @export
make_F_nu_type2 = function(nPatches, options=list(), vx = .3, sv = .1) {with(options,{
  nu_obj <- list() 
  nu_obj$vx = vx
  nu_obj$sv = sv
  class(nu_obj) <- "type2"
  return(nu_obj)
})}
