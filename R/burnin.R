#' @title Burn-in
#'
#' @description Use the defined hindcast to
#' run a model from the past up to the present.
#' Reset the initial conditions.
#'
#' @param model a **`ramp.xds`** model object
#' @param t_neg_inf run from -abs(t_neg_inf) days to \eqn{t=0} 
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
burnin = function(model, t_neg_inf = -3650){
  times = c(-abs(t_neg_inf), 0)
  model <- xds_solve(model, times=times)
  model <- last_to_inits(model)
  return(model)
}
