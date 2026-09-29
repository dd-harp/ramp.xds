
#' @title Set up a human fraction bionomic object
#'
#' @description Set up an object
#' to compute the human fraction, \eqn{q}
#'
#' @param q the human fraction
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
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

#' @title Compute the human feeding fraction, q
#'
#' @description This method dispatches on the type of `q_obj` and computes
#' the human feeding fraction, \eqn{q}.
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

#' @title Static model human feeding fraction
#' @description Implements [F_q] for a static model
#' @inheritParams F_q
#' @return \eqn{q}, the baseline human feeding fraction
#' @keywords internal
#' @export
F_q.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$q_obj$q)
}

#' @title Check human feeding fraction
#' 
#' @description
#' Check that the human feeding fractions are between zero and one and have
#' length `nPatches`.
#' 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the validated human feeding fraction
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

#' @title Change human feeding fraction
#' 
#' @description Update the human feeding fraction for the \eqn{s^{th}} vector
#' species.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
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

#' @title Set up human feeding fraction
#'
#' @description Configure and validate the human feeding fraction.
#'
#' @param name a human feeding fraction or setup method name
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

#' @title Set up F_q
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_F_q = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_q", options)
}

#' @title Set up F_q
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `Fq_name = name$name` 
#' + `options = name`
#' and call `setup_F_q(Fq_name, xds_obj, options, s)` 
#'
#' @inheritParams setup_F_q
#'
#' @return an **`xds`** object 
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

#' @title Set up no F_q 
#' @description Don't change anything 
#' @inheritParams setup_F_q
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_q.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up static human feeding fraction
#'
#' @description Set up supplied static human feeding fractions.
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

#' @title Set up F_q
#' 
#' @description Implements the "random" case for [setup_F_q]. See [make_F_q_random].
#' 
#' @inheritParams setup_F_q
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_q.random = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$q_obj <- make_F_q_random(xds_obj$nPatches, options)
  return(xds_obj)
}

#' @title Compute random human feeding fractions
#'
#' @description Generate patch-level human feeding fractions using the
#' configured random number generator.
#'
#' @inheritParams F_q
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_q.random = function(t, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$q_obj, {
    q <- f_rand(xds_obj$nPatches, p1, p2)
    return(q)
  })
}

#' @title Make random human feeding fractions
#' 
#' @description Build a configuration object for random human feeding fractions.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a configuration object for [F_q.random]
#' @export
make_F_q_random = function(nPatches, options=list(), f_rand = stats::rbeta,
                           p1=960, p2=1000) {
  with(options, {
    q_obj <- list(f_rand=f_rand, p1=p1, p2=p2)
    class(q_obj) <- "random"
    return(q_obj)
  })
}


#' @title WVB human feeding fraction
#'
#' @description Computes the human feeding fraction from the availability of
#' resident hosts (\eqn{W}), visitors (\eqn{V}), and all available blood hosts
#' (\eqn{B}).
#' 
#' \deqn{q = \frac{W+V}{B}}
#'
#' @inheritParams F_q
#'
#' @return \eqn{q}, the human feeding fraction
#' @keywords internal
#' @export
F_q.WVB = function(t, xds_obj, s){
  W = xds_obj$terms$W[[s]]
  V = xds_obj$patches$visitors[[s]]
  B = xds_obj$terms$B[[s]]
  q = (W+V)/B
  return(q)
}

#' @title Set up WVB human feeding fraction
#' 
#' @description Configure the WVB human feeding fraction method.
#' 
#' @inheritParams setup_F_q
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_q.WVB = function(name, xds_obj, options = list(), s=1){
  xds_obj$MY_obj[[s]]$q_obj <- make_F_q_WVB(xds_obj$nPatches)
  return(xds_obj)
}

#' @title Make a WVB human feeding fraction configuration
#' 
#' @description Create a configuration object for the WVB human feeding
#' fraction method.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
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
