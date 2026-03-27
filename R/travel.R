
#' @title Change the time at home
#'
#' @description Set a new *static* value for the fraction of time spent
#' in the spatial domain (*i.e.* in the patches)
#'
#' @param time_away time spent outside the spatial domain
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
change_time_away = function(time_away, xds_obj, i){
  stopifnot(length(time_away) == xds_obj$nStrata[[i]])
  xds_obj$XY_interface$time_away[[i]] = time_away
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}

#' @title Change the travel EIR
#'
#' @description Set a new *static* value for the travel EIR
#'
#' @param teir the travel entomological inoculation rate
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
change_travel_EIR = function(teir, xds_obj, i){
  stopifnot(length(teir) == xds_obj$nStrata[[i]])
  xds_obj$terms$travel_EIR[[i]] = teir
  return(xds_obj)
}
