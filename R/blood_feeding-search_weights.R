#' @title Check the blood search weights
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(blood_search_weights) == nStrata
#' 
#' @param blood_search_weights search weights for hosts
#' @param nStrata the number of population strata
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_blood_search_weights = function(blood_search_weights, nStrata){
  stopifnot(is.numeric(blood_search_weights))
  stopifnot(blood_search_weights >= 0)
  stopifnot(length(blood_search_weights) == nStrata) 
}

#' @title Change the blood search weights
#' 
#' @description
#' Change the *static* blood search weights
#' 
#' @param blood_search_weights a blood_search_weights
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_blood_search_weights = function(blood_search_weights, xds_obj, i=1){
  check_blood_search_weights(blood_search_weights, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$search_weights <- blood_search_weights
  xds_obj$XH_obj[[i]]$search_weights_obj <- make_static_obj()
  xds_obj <- update_XY_interface(xds_obj)
  return(xds_obj)
}

#' @title Set up blood search weights
#' 
#' @description Advanced setup for heterogeneous biting. Useful methods include:
#' + "as_is" changes the values to a vector of values
#' + "random" sets random search weights (see [make_blood_search_weights_random])
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param optionStrata configuration optionStrata
#'
#' @return an **`xds`** object
#'
#' @export
setup_blood_search_weights = function(name, xds_obj, optionStrata = list(), i=1){
  if(is.numeric(name)) class(optionStrata) = "as_is"
  if(is.character(name)) class(optionStrata) = name
  UseMethod("setup_blood_search_weights", optionStrata)
}

#' @title Set up a blood_search_weights
#'
#' @description If an optionStrata list is passed
#' as the first argument, then set 
#' + `BSWname = name$name` 
#' + `optionStrata = name`
#' and call `setup_blood_search_weights(BSWname, xds_obj, optionStrata, i)` 
#'
#' @inheritParams setup_blood_search_weights
#'
#' @return a numeric vector
#' 
#' @keywords internal
#' @export
setup_blood_search_weights.list = function(name, xds_obj, optionStrata=list(), i=1){
  optionStrata = name
  BSWname = name$name
  if(is.null(BSWname)) BSWname = "no_setup"
  xds_obj <- setup_blood_search_weights(BSWname, xds_obj, optionStrata, i)
  return(xds_obj)
}

#' @title Set up no blood_search_weights 
#' @description Don't change anything 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_search_weights.setup = function(name, xds_obj, optionStrata = list(), i=1){
  blood_search_weights = rep(1, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$search_weights <- blood_search_weights
  xds_obj$XH_obj[[i]]$search_weights_obj <- make_static_obj()
  return(xds_obj)
}

#' @title Set up no blood_search_weights 
#' @description Don't change anything 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_search_weights.no_setup = function(name, xds_obj, optionStrata = list(), i=1){
  return(xds_obj)
}

#' @title Set up a blood_search_weights
#' 
#' @description Implements the "method1" case for [setup_blood_search_weights]. See [make_blood_search_weights_random]
#' 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_search_weights.random = function(name, xds_obj, optionStrata = list(), i=1){
  blood_search_weights = make_blood_search_weights_random(xds_obj$nStrata[i], optionStrata)
  xds_obj <- change_blood_search_weights(blood_search_weights, xds_obj, i)
  return(xds_obj)
}

#' @title Make blood search weights 
#' 
#' @description Set up a random vector 
#' 
#' @param nStrata is the number of population strata
#' @param optionStrata is a set of optionStrata that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a numeric vector
#' @export
make_blood_search_weights_random = function(nStrata, optionStrata=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(optionStrata,{
  blood_search_weights <- f_rand(nStrata, p1, p2)
  return(blood_search_weights)
})}

#' @title Set up a blood_search_weights
#' @description 
#' Pass a pre-configured blood_search_weights. If it passes 
#' the checks, it replaces the current blood_search_weights.
#' 
#' If the first argument to `setup_blood_search_weights` is a
#' numeric vector, the `"as_is"` method is used. 
#' 
#' The conventional way to call this method is `name = "as_is"`, 
#' and the new blood search weights are passed as
#' `optionStrata$blood_search_weights` 
#' 
#' @inheritParams setup_blood_search_weights
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_search_weights.as_is = function(name, xds_obj, optionStrata=list(), i=1){
  
  if(is.list(optionStrata)) 
    blood_search_weights_as_is = optionStrata$weights
  if(is.numeric(name))
    blood_search_weights_as_is = as.vector(name)
  
  xds_obj <- change_blood_search_weights(blood_search_weights_as_is, xds_obj, i)
  
  return(xds_obj)
}

#' @title Update the blood search weights
#' 
#' @description 
#' This is called to update dy
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_search_weights <- function(t, y, xds_obj, i){
  UseMethod("update_blood_search_weights", xds_obj$XH_obj[[i]]$search_weights_obj)
}

#' @title Update the blood_search_weights
#' @description The `static` method returnStrata `xds_obj` unmodified
#' @inheritParams update_blood_search_weights
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_search_weights.static <- function(t, y, xds_obj, i) { return(xds_obj) }

#' @title Get availability of blood search weights
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a vector 
#' @export
get_blood_search_weights = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$search_weights)
}


