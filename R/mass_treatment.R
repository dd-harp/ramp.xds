# X

#' @title Setup mass treatment
#' 
#' @description
#' Add mass treatment ports to modules. 
#' 
#' @param xds_obj an **`xds`** model object
#' @param mda a function of the form \eqn{F(t,V)}
#' @param msat a function of the form \eqn{F(t,V)}
#' @param options options list (overrides command line arguments)
#' @param i the species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_mass_treatment = function(xds_obj, mda=F_zero, msat=F_zero, options=list(), i=1){with(options,{
   xds_obj$XH_obj[[i]]$mda <- mda
   xds_obj$XH_obj[[i]]$msat <- msat
   return(xds_obj)
})}