# X

#' @title Change F_age
#'
#' @description
#' Change the relative biting rate function by age. The function `F` should have the form
#' \eqn{F_a(a,V)}. If the variable \eqn{V} is not used, its default value should be set to an empty list.
#' 
#' @param F new age function
#' @param xds_obj an **`xds`** model object
#'
#' @seealso [F_one]
#' 
#' @return an **`xds`** model object
#'
#' @export
change_F_age = function(F, xds_obj){
  xds_obj$EIR_obj$F_age = F
  return(xds_obj)
}

#' @title Change age 
#'
#' @description
#' Change parameters for the relative biting rates
#' by age and rebuild the function.
#'
#' @param X a list with new parameters
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** model object
#'
#' @export
#' @keywords internal
change_age = function(X, xds_obj){
  xds_obj$EIR_obj$age_par = X
  xds_obj$EIR_obj$F_age = make_function(X)
  return(xds_obj)
}