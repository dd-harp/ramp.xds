# X

#' @title Set up bionomic port for mosquito mortality rate (**MY**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the baseline mosquito mortality
#' rate, \eqn{g}, for a static model.
#'
#' @param g the mosquito mortality rate
#' @param MY_obj an **`MY`** module object
#'
#' @return an **`MY`** module object
#'
#' @keywords internal
#' @export
setup_g_obj = function(g, MY_obj){
  MY_obj$g = g
  MY_obj$g_t = g
  MY_obj$es_g = 1
  MY_obj$g_obj <- list()
  class(MY_obj$g_obj) <- "static"
  MY_obj$g_obj$g <- g
  return(MY_obj)
}

#' @title Compute the baseline mosquito mortality rate (**MY**)
#'
#' @description Compute the baseline mosquito mortality rate, \eqn{g}.
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_g = function(t, xds_obj, s) {
  UseMethod("F_g", xds_obj$MY_obj[[s]]$g_obj)
}

#' @title Static model mosquito mortality rate (**MY**)
#' @description Return the baseline mosquito mortality rate for a static model.
#' @inheritParams F_g
#' @return \eqn{g}, the baseline mosquito mortality rate
#' @keywords internal
#' @export
F_g.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$g_obj$g)
}

#' @title Check mosquito mortality rate
#' 
#' @description
#' Check that the mosquito mortality rates are non-negative and have length
#' `nPatches`.
#' 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the validated mosquito mortality rate
#' @seealso [xds_info_mosquito_demography]
#' @export
check_g = function(xds_obj, s=1){
  g <- F_g(0, xds_obj, s)
  stopifnot(is.numeric(g))
  stopifnot(g >= 0)
  stopifnot(length(g) == xds_obj$nPatches)
  return(g)
}

#' @title Change mosquito mortality rate (**MY**)
#' 
#' @description Update the mosquito mortality rate for the \eqn{s^{th}} vector
#' species and refresh the demographic matrix.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' 
#' @export
change_g = function(xds_obj, s=1){
  g <- check_g(xds_obj, s)
  xds_obj$MY_obj[[s]]$g_obj$g <- g
  xds_obj$MY_obj[[s]]$g <- g
  xds_obj$MY_obj[[s]]$g_t <- g
  xds_obj <- change_Omega(xds_obj, s)
  class(xds_obj$MY_obj[[s]]$g_obj) <- "static"
  return(xds_obj)
}

#' @title Set up mosquito mortality rate (**MY**)
#'
#' @description Set up the mosquito mortality strategy and validate its rates.
#'
#' @param name a mosquito mortality rate or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_g = function(name, xds_obj, options=list(), s=1){
  xds_obj <- setup_F_g(name, xds_obj, options, s)
  xds_obj <- change_g(xds_obj, s)
  return(xds_obj)
}

#' @rdname setup_g
#' @export
setup_mosquito_mortality <- setup_g

#' @title Configure the mosquito mortality rate function (**MY**)
#'
#' @description Configure the function used to compute the baseline mosquito
#' mortality rate.
#'
#' @param name a mosquito mortality rate or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_F_g = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_g", options)
}

#' @title Set up F_g (**MY**)
#'
#' @description Set up mosquito mortality rates using an options list.
#'
#' @note If an options list is passed as the first argument, use its
#' `name` field to select the setup method. If `name` is NULL, use the
#' `no_setup` method. Pass the list as `options` to [setup_F_g].
#'
#' @inheritParams setup_F_g
#'
#' @return an **`xds`** model object
#' 
#' @keywords internal
#' @export
setup_F_g.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Fg_name = name$name
  if(is.null(Fg_name)) Fg_name = "no_setup"
  xds_obj <- setup_F_g(Fg_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up F_g (no setup) (**MY**)
#' @description Leave the existing mosquito mortality rate configuration unchanged.
#' @inheritParams setup_F_g
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_g.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up static mosquito mortality rate (**MY**)
#'
#' @description Set up supplied static mosquito mortality rates from `options$g`
#' or a numeric `name`. Values in `options$g` take precedence.
#'
#' @inheritParams setup_F_g
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_g.as_is = function(name, xds_obj, options=list(), s=1){
  if (is.list(options) && !is.null(options$g)) {
    g <- options$g
  } else if (is.numeric(name)) {
    g <- name
  } else {
    stop("Supply mortality rates in `options$g` or as numeric `name`.")
  }
  xds_obj$MY_obj[[s]]$g_obj$g <- g
  class(xds_obj$MY_obj[[s]]$g_obj) <- "static"
  return(xds_obj)
}

#' @title Set up random mosquito mortality rate (**MY**)
#' 
#' @description Configure random mosquito mortality rates using [make_F_g_random].
#' 
#' @inheritParams setup_F_g
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_g.random = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$g_obj <- make_F_g_random(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Make random mosquito mortality rates (**MY**)
#' 
#' @description Draw mosquito mortality rates once during setup.
#' Subsequent evaluations reuse these values.
#'
#' @note The values are stored in a static port object.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator called as `f_rand(nPatches, p1, p2)`
#' that returns a numeric vector of length `nPatches`
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a static port object for [F_g.static]
#' @export
make_F_g_random = function(nPatches, options=list(), f_rand = stats::rbeta,
                           p1=80, p2=1000) {
  with(options, {
    g_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    g_obj$g <- f_rand(nPatches, p1, p2)
    class(g_obj) <- "static"
    return(g_obj)
  })
}
