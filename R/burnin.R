#' @title Burn-in
#'
#' @description Use the defined hindcast to
#' run a model from the past up to the present.
#' Reset the initial conditions.
#'
#' @param model a **`ramp.xds`** model object
#' @param neg_inf_yr run from -neg_inf_yr years to the present
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
burnin = function(model, neg_inf_yr = 10){
  times = c(-abs(neg_inf_yr)*365, 0)
  model <- xds_solve(model, times=times)
  model <- last_to_inits(model)
  return(model)
}
