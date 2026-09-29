

#' @title Setup Laying Rate Bionomic Object
#'
#' @description Set up an object
#' to compute the egg laying rate, \eqn{nu}
#'
#' @param nu the egg laying rate (# batches, per mosquito, per day)
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
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

#' @title Compute the Egg Laying Rate
#'
#' @description This method dispatches on the type of `nu_obj`. It should
#' set the values of the egg laying rate, \eqn{\nu}
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

#' @title Static model egg laying rate
#'
#' @description Implements [F_nu] for a static model
#'
#' @inheritParams F_nu
#'
#' @return \eqn{nu}, the egg laying rate
#' 
#' @keywords internal
#' @export
F_nu.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$nu_obj$nu)
}

#' @title Check egg laying rate
#' 
#' @description
#' Check that the egg laying rates are in the expected range and length:
#' + all elements are non-negative
#' + length(F_nu) == nPatches
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the egg laying rate
#' @seealso [xds_info_egg_laying]
#' @export
check_nu = function(xds_obj, s=1){
  nu = F_nu(0, xds_obj, s)
  stopifnot(is.numeric(nu))
  stopifnot(nu >= 0)
  stopifnot(length(nu) == xds_obj$nPatches)
  return(nu)
}

#' @title Change F_nu
#' 
#' @description
#' Update F_nu for the \eqn{s^{th}} vector species.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
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

#' @title Setup nu
#' 
#' @description Set up the egg laying rate (\eqn{\nu}) 
#' for static systems
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_nu = function(name, xds_obj, options = list(), s=1){
  xds_obj <- setup_F_nu(name, xds_obj, options, s)
  xds_obj <- change_nu(xds_obj, s)
  return(xds_obj)
}


#' @title Set up F_nu
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_F_nu = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_F_nu", options)
}

#' @title Set up F_nu
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `Fnu_name = name$name` 
#' + `options = name`
#' and call `setup_F_nu(Fnu_name, xds_obj, options, s)` 
#'
#' @inheritParams setup_F_nu
#'
#' @return an **`xds`** object 
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

#' @title Setup egg laying
#'
#' @description Change the static values. 
#'
#' @inheritParams setup_F_nu 
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_nu.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    nu = options$nu
  if(is.matrix(name))
    nu <- name
  
  xds_obj$MY_obj[[s]]$nu_obj$nu = nu
  xds_obj <- change_nu(xds_obj, s)
  return(xds_obj)
}

#' @title Set up no F_nu 
#' @description Don't change anything 
#' @inheritParams setup_F_nu
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_nu.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Compute random egg laying rates
#'
#' @description Compute random egg laying rates
#'
#' @inheritParams F_nu
#'
#' @return \eqn{nu}, the egg laying rate
#' @keywords internal
#' @export
F_nu.random = function(t, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$nu_obj,{
    nu = F_nu(xds_obj$nPatches, p1, p2)
    return(nu)
})}


#' @title Set up F_nu
#' 
#' @description Implements the "random" case for [setup_nu]. See [make_F_nu_random]
#' 
#' @inheritParams setup_F_nu
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_nu.random = function(name, xds_obj, options = list(), s=1){
  nu_obj = make_F_nu_random(xds_obj$nPatches, options)
  xds_obj$MY_obj[[s]]$nu_obj = nu_obj
  return(xds_obj)
}

#' @title Make random laying rates
#' 
#' @description Set up a random vector 
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument 1 for f_rand 
#' @param p2 argument 2 for f_rand
#' 
#' @return an object to set up egg laying
#' @export
make_F_nu_random = function(nPatches, options=list(), 
                            f_rand = stats::rbeta, 
                            p1=980, p2=1000){
  with(options,{
    nu_obj <- list() 
    nu_obj$p1=p1
    nu_obj$p2=p2
    nu_obj$F_nu = f_rand
    class(nu_obj) <- "random" 
    return(nu_obj)
})}



#' @title Egg laying rate
#'
#' @description Egg laying rates are a type2 functional response 
#' to available water. 
#' 
#' Letting \eqn{Q} be the availability of all water. The egg laying rate is 
#' \deqn{\nu = \nu_x \frac{s_\nu Q}{1+s_\nu Q}}  
#'
#' @inheritParams F_nu
#'
#' @return \eqn{\nu}, the baseline egg laying rate
#' 
#' @keywords internal
#' @export
F_nu.type2 = function(t, xds_obj, s){
  Q <- xds_obj$terms$Qall[[s]]
  with(xds_obj$MY_obj[[s]]$nu_obj, {
      nu = vx*sv*Q/(1+sv*Q)
      return(nu)
})}

#' @title Setup F_nu
#' 
#' @description Set up a type 2 functional response for
#' egg laying
#' 
#' @inheritParams setup_F_nu
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_nu.type2 = function(name, xds_obj, options = list(), s=1){
  nu_obj <- make_F_nu_type2(xds_obj$nPatches, options)
  xds_obj$MY_obj[[s]]$nu_obj <- nu_obj

  return(xds_obj)
}

#' @title Make type2 egg laying rates
#' 
#' @description Set up a type2 vector 
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param vx the maximum laying
#' @param sv the shape parameter
#' @return an object to set up egg laying
#' @export
make_F_nu_type2 = function(nPatches, options=list(), vx = .3, sv = .1) {with(options,{
  nu_obj <- list() 
  nu_obj$vx = vx
  nu_obj$sv = sv
  class(nu_obj) <- "type2"
  return(nu_obj)
})}
