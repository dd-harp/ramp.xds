
#' @title Resources
#'  
#' @description Computes availability of resources potentially
#' affecting adult mosquito bionomics: blood hosts, 
#' aquatic habitats, and sugar. There are ports to 
#' compute variables that are part of the blood feeding, 
#' egg laying, or exposure interfaces. 
#' 
#' + [BloodHosts] availability of alternative blood hosts 
#' + [HabitatDynamics] to modify habitat search weights
#' + [Travel] to compute variables associated with exposure while traveling
#' + [Visitors] to compute parasite/pathogen blood feeding on visitors
#' + [Traps] to compute availability of oviposition traps
#' + [Sugar] availability of sugar 
#'  
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** model object
#' 
#' @export
Resources = function(t, y, xds_obj){
  xds_obj <- BloodHosts(t, y, xds_obj)
  xds_obj <- HabitatDynamics(t, y, xds_obj) 
  xds_obj <- Travel(t, y, xds_obj)
  xds_obj <- Visitors(t, y, xds_obj)
  xds_obj <- Traps(t, y, xds_obj) 
  xds_obj <- Sugar(t, y, xds_obj)
  return(xds_obj)
}
