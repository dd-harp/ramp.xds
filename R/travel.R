
#' @title Update Time Away
#' @description Port function for the fraction of time spent outside the
#' spatial domain, `time_away`.
#' Dispatches on `class(xds_obj$XY_interface$time_away_obj[[i]])`.
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_time_away <- function(xds_obj, i) {
  UseMethod("update_time_away", xds_obj$XH_obj[[i]]$timespent_obj)
}

#' @title Update Time Away (static)
#' @description Returns `xds_obj` unmodified; `time_away` is static.
#' @inheritParams update_time_away
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_time_away.static <- function(xds_obj, i) { return(xds_obj) }

#' @title Update Time Away (setup)
#' @description Acknowledges a one-time update to `time_away`
#' and sets the port back to `"static"`.
#' @inheritParams update_time_away
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_time_away.setup <- function(xds_obj, i) {
  class(xds_obj$XH_obj[[i]]$timespent_obj) <- "static"
  return(xds_obj)
}

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
  xds_obj$XH_obj[[i]]$time_away = time_away
  if(!is.null(xds_obj$XH_obj[[i]]$timespent_obj))
    xds_obj$XH_obj[[i]]$timespent_obj <- trigger_setup(xds_obj$XH_obj[[i]]$timespent_obj)
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
