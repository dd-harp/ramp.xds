#' @title Visitors 
#' 
#' @description
#' There are two parameters built into the transmission interface to handle
#' biting on non-resident humans:
#' 
#' + `visitors` is the availability of non-resident humans
#' 
#' + `visitor_kappa` is the net infectiousness of the visitors 
#' 
#'  
#' @name xds_info_visitors
NULL

#' @title Visitors 
#' 
#' @description
#' There are two parameters built into the transmission interface to handle
#' biting on non-resident humans:
#' 
#' + `visitors` is the availability of non-resident humans
#' 
#' + `visitor_kappa` is the net infectiousness of the visitors 
#' 
#'  
#' @name xds_port_visitors
NULL

#' @title Visitors Infectiousness
#' 
#' @description
#' There are two parameters built into the transmission interface to handle
#' biting on non-resident humans:
#' 
#' + `visitors` is the availability of non-resident humans
#' 
#' + `visitor_kappa` is the net infectiousness of the visitors 
#'  
#' @name xds_port_visitor_kappa
NULL


#' @title Change Availability of Visitors 
#' @description Set the value of the parameter describing
#' available visitors
#' @param visitors availability of visitors
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
change_visitors = function(visitors, xds_obj, s){
  stopifnot(length(visitors) == xds_obj$nPatches)
  xds_obj$XY_interface$visitors[[s]] = visitors 
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}

#' @title Change Visitor's NI
#' @description Set the value of the parameter describing
#' available visitors
#' @param visitor_kappa net infectiousness of visitors
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
change_visitor_kappa = function(visitor_kappa, xds_obj, s){
  stopifnot(length(visitor_kappa) == xds_obj$nPatches)
  xds_obj$XY_interface$vis_kappa[[s]] = visitor_kappa 
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}
