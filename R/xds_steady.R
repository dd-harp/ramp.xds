
#' @title Solve for the steady state 
#' @description Compute the steady state for a 
#' system of differential equations.
#' 
#' + Run for \eqn{Y} years
#' 
#' + Check the sun of squared differences in the final state is less than `tol`
#' 
#' + If not 
#' 
#' @param xds_obj an **`xds`** model object
#' @param Y the number of years to run once
#' @param tol the desired accuracy
#' @return an **`xds`** object
#' @export
xds_steady = function(xds_obj, Y=10, tol=1e-5){
  xds_obj <- xds_solve(xds_obj, Y*365, Y*365)
  y_0 <- get_last(xds_obj)
  diff = 1
  i=0
  while(diff > tol){
    i = i+1
    xds_obj <- last_to_inits(xds_obj)
    xds_obj <- xds_solve(xds_obj, Y*365, Y*365)
    y_1 <- get_last(xds_obj) 
    diff = sum((y_0-y_1)^2)
    y_0 = y_1
    diff = 0
    stopifnot(i < 10) 
  }
  xds_obj$outputs$steady = parse_y(y_1, xds_obj)
  return(xds_obj)
}

#' @title Compute stable orbits
#'
#' @description
#' Run the system for 10 years, and save the last year's orbits.
#'
#' @inheritParams xds_steady
#' 
#' @return an **`xds`** object
#' @export
xds_stable_orbit = function(xds_obj, Y=10, tol=1e-5){
  xds_obj <- xds_steady(xds_obj, Y, tol)
  
  xds_obj <- xds_solve(xds_obj, Y*365, dt=1)
  deout = tail(xds_obj$outputs$orbits$deout, 365)
  tm = deout[,1]
  xds_obj <- parse_outputs(xds_obj, deout[,-1], tm)
  xds_obj$outputs$tm <- tm %% 365
  xds_obj$outputs$deout <- deout
  
  return(xds_obj)
}

