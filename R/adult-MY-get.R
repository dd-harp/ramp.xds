
#' @title Get **MY** outputs
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the orbits for the **MY** component
#'
#' @export
get_MY_orbits = function(xds_obj, s=1){

  got = xds_obj$outputs$orbits$MY[[s]]
  got$time = xds_obj$outputs$time

  return(got)
}