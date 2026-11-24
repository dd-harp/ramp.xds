# X

#' @title Set up multiplicative time-series patterns
#' @description Store supplied seasonal, trend, and shock functions with empty
#' parameter lists, or construct them from their parameter lists.
#' @param obj the object receiving the pattern functions and parameters
#' @param options a named list of resolved function and parameter values:
#' `F_season`, `F_trend`, `F_shock` and their `season_par`, `trend_par`,
#' `shock_par` counterparts. Human population patterns may instead use
#' `H_season`, `H_trend`, `H_shock` and matching `H_*_par` names.
#' @return the updated object
#' @keywords internal
make_ts_obj = function(obj, options){
  function_names <- intersect(
    c("F_season", "F_trend", "F_shock", "H_season", "H_trend", "H_shock"),
    names(options)
  )
  for (function_name in function_names) {
    parameter_name <- paste0(sub("^F_", "", function_name), "_par")
    if (is.function(options[[function_name]])) {
      obj[[parameter_name]] <- list()
      obj[[function_name]] <- options[[function_name]]
    } else {
      obj[[parameter_name]] <- options[[parameter_name]]
      obj[[function_name]] <- make_function(options[[parameter_name]])
    }
  }
  return(obj)
}