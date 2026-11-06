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

#' @title Change age 
#'
#' @description
#' Change parameters for the relative biting rates
#' by age and rebuild the function 
#'
#' @param X a list with new parameters
#' @param xds_obj an **`xds`** model object
#' @param ix the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
#' @keywords internal
change_age = function(X, xds_obj, ix=1){
  xds_obj$EIR_obj[[ix]]$age_par = X 
  xds_obj$EIR_obj[[ix]]$F_age = make_function(X)
  return(xds_obj)
}