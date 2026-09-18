#' @title Setup a circadian pattern function
#' 
#' @description
#' Update the circadian for the \eqn{s^{th}} host species
#' 
#' @param F_circadian a circadian function or a method name (*e.g.* "setup")
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
setup_F_circadian = function(F_circadian, xds_obj, s=1){
  if(is.character(F_circadian)) class(F_circadian) = F_circadian
  UseMethod("setup_F_circadian", F_circadian)
}

#' @title Show Circadian
#' 
#' @description
#' Update the circadian for the \eqn{s^{th}} host species
#' 
#' @param F_circadian a circadian function
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' 
#' @export
setup_F_circadian.setup = function(F_circadian, xds_obj, s=1){
  xds_obj$MY_obj[[s]]$F_circadian <- F_one
  return(xds_obj)
}



