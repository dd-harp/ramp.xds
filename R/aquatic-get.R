
#' @title Get **L** outputs
#' 
#' @description
#' Pull the saved, parsed orbits for the **L** 
#' component from an **`xds`** object
#' 
#' 
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' 
#' @return the orbits for the **L** component 
#' 
#' @export
get_L_orbits = function(xds_obj, s=1){
  
  got = xds_obj$outputs$orbits$L[[s]]
  got$time = xds_obj$outputs$time
  
  return(got)
}