# X

#' @title Set up bionomic port for human fraction (**MY**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the baseline human
#' fraction, \eqn{q}, for a static model.
#'
#' @param q the human fraction
#' @param MY_obj an **`MY`** module object
#'
#' @return an **`MY`** module object
#'
#' @keywords internal
#' @export
setup_q_obj = function(q, MY_obj){
  MY_obj$q = q
  MY_obj$q_t = q
  MY_obj$es_q = 1
  MY_obj$q_obj <- list()
  class(MY_obj$q_obj) <- "static"
  MY_obj$q_obj$q <- q
  return(MY_obj)
}

#' @title Compute the baseline human fraction (**MY**)
#'
#' @description Compute the baseline human fraction, \eqn{q}.
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_q = function(t, xds_obj, s) {
  UseMethod("F_q", xds_obj$MY_obj[[s]]$q_obj)
}

#' @title Static model human fraction (**MY**)
#' @description Return the baseline human fraction for a static model.
#' @inheritParams F_q
#' @return \eqn{q}, the baseline human fraction
#' @keywords internal
#' @export
F_q.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$q_obj$q)
}

#' @title Check human fraction
#' 
#' @description
#' Check that the human fractions are between zero and one and have
#' length `nPatches`.
#' 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the validated human fraction
#' @seealso [xds_info_blood_feeding]
#' @export
check_q = function(xds_obj, s=1){
  q <- F_q(0, xds_obj, s)
  stopifnot(is.numeric(q))
  stopifnot(q >= 0)
  stopifnot(q <= 1)
  stopifnot(length(q) == xds_obj$nPatches)
  return(q)
}

#' @title Change human fraction (**MY**)
#' 
#' @description Update the human fraction for the \eqn{s^{th}} vector
#' species.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' 
#' @export
change_q = function(xds_obj, s=1){
  q <- check_q(xds_obj, s)
  xds_obj$MY_obj[[s]]$q_obj$q <- q
  xds_obj$MY_obj[[s]]$q <- q
  xds_obj$MY_obj[[s]]$q_t <- q
  class(xds_obj$MY_obj[[s]]$q_obj) <- "static"
  return(xds_obj)
}

#' @title Set up human fraction (**MY**)
#'
#' @description Configure and validate the human fraction.
#'
#' @param name a human fraction or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @export
setup_q = function(name, xds_obj, options=list(), s=1){
  xds_obj <- setup_F_q(name, xds_obj, options, s)
  xds_obj <- change_q(xds_obj, s)
  return(xds_obj)
}

#' @rdname setup_q
#' @export
setup_human_fractions <- setup_q

#' @rdname setup_q
#' @export
setup_human_fraction <- setup_q

#' @title Configure the human fraction function (**MY**)
#'
#' @description Configure the function used to compute the baseline human
#' fraction.
#'
#' @param name a human fraction or setup method name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_F_q = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_q", options)
}

#' @title Set up F_q (**MY**)
#'
#' @description Set up human fractions using an options list.
#'
#' @note If an options list is passed as the first argument, use its
#' `name` field to select the setup method. If `name` is NULL, use the
#' `no_setup` method. Pass the list as `options` to [setup_F_q].
#'
#' @inheritParams setup_F_q
#'
#' @return an **`xds`** model object
#' 
#' @keywords internal
#' @export
setup_F_q.list = function(name, xds_obj, options=list(), s=1){
  options = name
  Fq_name = name$name
  if(is.null(Fq_name)) Fq_name = "no_setup"
  xds_obj <- setup_F_q(Fq_name, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up F_q (no setup) (**MY**)
#' @description Leave the existing human fraction configuration unchanged.
#' @inheritParams setup_F_q
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_q.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up static human fraction (**MY**)
#'
#' @description Set up supplied static human fractions from `options$q`
#' or a numeric `name`. Values in `options$q` take precedence.
#'
#' @inheritParams setup_F_q
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_q.as_is = function(name, xds_obj, options=list(), s=1){
  if (is.list(options) && !is.null(options$q)) {
    q <- options$q
  } else if (is.numeric(name)) {
    q <- name
  } else {
    stop("Supply human fractions in `options$q` or as numeric `name`.")
  }
  xds_obj$MY_obj[[s]]$q_obj$q <- q
  class(xds_obj$MY_obj[[s]]$q_obj) <- "static"
  return(xds_obj)
}

#' @title Set up random human fractions (**MY**)
#' 
#' @description Configure random human fractions using [make_F_q_random].
#' 
#' @inheritParams setup_F_q
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_q.random = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$q_obj <- make_F_q_random(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Make random human fractions (**MY**)
#' 
#' @description Draw human fractions once during setup.
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
#' @return a static port object for [F_q.static]
#' @export
make_F_q_random = function(nPatches, options=list(), f_rand = stats::rbeta,
                           p1=960, p2=1000) {
  with(options, {
    q_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    q_obj$q <- f_rand(nPatches, p1, p2)
    class(q_obj) <- "static"
    return(q_obj)
  })
}


#' @title WVB human fraction (**MY**)
#'
#' @description Compute the baseline human fraction from the availability of
#' resident hosts (\eqn{W}), visitors (\eqn{V}), and all available blood hosts
#' (\eqn{B}).
#' 
#' \deqn{q = \frac{W+V}{B}}
#'
#' @inheritParams F_q
#'
#' @return \eqn{q}, the baseline human fraction
#' @keywords internal
#' @export
F_q.WVB = function(t, xds_obj, s){
  W = xds_obj$terms$W[[s]]
  V = xds_obj$patches$visitors[[s]]
  B = xds_obj$terms$B[[s]]
  q = (W+V)/B
  return(q)
}

#' @title Set up WVB human fraction (**MY**)
#' 
#' @description Configure the WVB human fraction method.
#' 
#' @inheritParams setup_F_q
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_q.WVB = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$q_obj <- make_F_q_WVB(xds_obj$nPatches)
  return(xds_obj)
}

#' @title Make a WVB human fraction configuration (**MY**)
#' 
#' @description Create a configuration object for the WVB human
#' fraction method.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
#' 
#' @return a configuration object for [F_q.WVB]
#' 
#' @seealso [F_q.WVB]
#' 
#' @export
make_F_q_WVB = function(nPatches, options=list()) {
    q_obj <- list()
    class(q_obj) <- "WVB"
    return(q_obj)
}
