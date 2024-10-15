
#' @title Trigger `setup`
#' @description For various `S3` functions, change
#' the class of the dispatching object from `static`
#' to `setup`
#' @param obj part of an **`xds`** object
#' @return the `obj`
#' @export
trigger_setup = function(obj){
  UseMethod("trigger_setup", obj)
}

#' @title Trigger `setup`
#' @description For various `S3` functions, change
#' the class of the dispatching object from `static`
#' to `setup`
#' @param obj part of an **`xds`** object
#' @return the `obj`
#' @export
trigger_setup.static = function(obj){
  class(obj) <- 'setup'
  return(obj)
}

#' @title Trigger `setup`
#' @description For various `S3` functions, change
#' leave the class of the dispatching object as `setup`
#' @param obj part of an **`xds`** object
#' @return the `obj`
#' @export
trigger_setup.setup = function(obj){
  return(obj)
}

#' @title Trigger `setup`
#' @description For various `S3` functions, change
#' leave the class of the dispatching object as `dynamic`
#' @param obj part of an **`xds`** object
#' @return the `obj`
#' @export
trigger_setup.dynamic = function(obj){
  return(obj)
}
