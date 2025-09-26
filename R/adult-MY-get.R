
#' @title Get **MY** outputs
#' 
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' 
#' @return the orbits for the **MY** component 
#' 
#' @export
get_MY_out = function(xds_obj, s=1){
  
  got = xds_obj$outputs$orbits$MY[[s]]
  got$time = xds_obj$outputs$time
  
  return(got)
}