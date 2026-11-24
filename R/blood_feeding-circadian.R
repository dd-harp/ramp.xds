#' @title Setup a circadian pattern function
#' 
#' @description Set the circadian activity function for vector species \eqn{s}.
#'
#' @section Circadian Pattern Function:
#'
#' A circadian pattern function is a model for relative mosquito
#' activity by time of day, \eqn{F_c(t)}. On average, it should
#' be true that \deqn{\int_{t}^{t+1} F_c(s) ds \approx 1.}
#' A circadian function is used to weight time spent by time of day
#' to compute time at risk.
#'
#' @seealso [F_TaR]
#' 
#' @param F_circadian a circadian function or a method name (*e.g.* "setup")
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' 
#' @export
setup_F_circadian = function(F_circadian, xds_obj, s=1){
  if(is.character(F_circadian)) class(F_circadian) = F_circadian
  UseMethod("setup_F_circadian", F_circadian)
}

#' @title Set the default circadian activity function
#' 
#' @description Set the activity function to [F_one] for vector species \eqn{s}.
#' 
#' @param F_circadian a circadian function
#' @inheritParams setup_F_circadian
#'
#' @return an **`xds`** model object
#' @keywords internal
#' 
#' @export
setup_F_circadian.setup = function(F_circadian, xds_obj, s=1){
  xds_obj$MY_obj[[s]]$F_circadian <- F_one
  return(xds_obj)
}

