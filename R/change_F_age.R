#' @title Change F_age
#'
#' @description
#' Change the age function. The function `F` should have the form
#' \eqn{F_a(a,V)}. If the variable \eqn{V} is not used, it's default value should be set to an empty list. 
#' 
#' @param F new age function
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#'
#' @seealso [F_one]
#' 
#' @return an **`xds`** object
#'
#' @export
change_F_age = function(F, xds_obj, ix=1){
  xds_obj$EIR_obj[[ix]]$F_age = F
  return(xds_obj)
}
