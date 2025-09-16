#' @title Burn In
#'
#' @description Run the model from some time in the 
#' remote past (\eqn{t_{-\infty}}) up to the present (\eqn{t=0}), and then  
#' reset the initial conditions.
#'
#' @note The algorithm sets \eqn{t_{-\infty}} to \eqn{-|}`t_neg_inf`\eqn{|}, the negative absolute value of `t_neg_inf`
#' 
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param t_neg_inf days before the present, \eqn{t_{-\infty}} 
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
burnin = function(xds_obj, t_neg_inf = -3650){
  times = c(-abs(t_neg_inf), 0)
  xds_obj <- xds_solve(xds_obj, times=times)
  xds_obj <- last_to_inits(xds_obj)
  return(xds_obj)
}
