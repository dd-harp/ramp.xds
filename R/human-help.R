
#' @title Generic methods for the XH component
#'
#' @description
#' The functions that define **XH**: 
#'
#' \itemize{
#'   \item \code{\link{dXHdt}} -- compute derivatives
#'   \item \code{\link{Update_XHt}} -- update state variables
#'   \item \code{\link{F_H}} -- compute host population density
#'   \item \code{\link{F_I}} -- compute infectious host population size
#'   \item \code{\link{F_infectivity}} -- compute infectivity (pre-erythrocytic immunity)
#'   \item \code{\link{F_ni}} -- compute net infectiousness
#'   \item \code{\link{F_prevalence}} -- compute true prevalence
#'   \item \code{\link{F_pfpr_by_lm}} -- compute prevalence by light microscopy
#'   \item \code{\link{F_pfpr_by_rdt}} -- compute prevalence by RDT
#'   \item \code{\link{F_pfpr_by_pcr}} -- compute prevalence by PCR
#'   \item \code{\link{get_HTC}} -- compute human transmitting capacity
#'   \item \code{\link{setup_XH_obj}} -- set up the XH module object
#'   \item \code{\link{setup_XH_ix}} -- set index values
#'   \item \code{\link{setup_XH_inits}} -- set up initial values
#'   \item \code{\link{get_XH_inits}} -- get initial values
#'   \item \code{\link{change_XH_inits}} -- change initial values
#'   \item \code{\link{get_XH_vars}} -- list state variables
#'   \item \code{\link{get_XH_ix}} -- get variable indices
#'   \item \code{\link{get_XH_pars}} -- get parameters
#'   \item \code{\link{change_XH_pars}} -- change parameters
#'   \item \code{\link{change_H}} -- change host population density
#'   \item \code{\link{parse_XH_orbits}} -- parse outputs
#'   \item \code{\link{get_XH_orbits}} -- get saved orbits
#'   \item \code{\link{get_PR}} -- get prevalence from orbits
#'   \item \code{\link{steady_state_X}} -- compute X steady state given FoI and H
#'   \item \code{\link{steady_state_XH}} -- compute XH steady state given FoI
#'   \item \code{\link{xds_plot_X}} -- plot X outputs
#'   \item \code{\link{check_XH}} -- run consistency checks
#'   \item \code{\link{skill_set_XH}} -- return module skill set
#' }
#'
#' @name XH_functions
NULL

#' @title Get functions for XH
#'
#' @description
#' Get functions for **XH** 
#' 
#' `?get_XH`
#' @section Outputs: 
#' \itemize{
#'   \item \code{\link{get_XH_orbits}} -- get the XH orbits
#'   \item \code{\link{get_EIR}} -- get the EIR orbits
#'   \item \code{\link{get_PR}} -- get the PR orbits
#'   \item \code{\link{get_H}} -- get the PR orbits
#' }
#'
#' @section Variables:  
#' \itemize{
#'   \item \code{\link{get_XH_inits}} -- get initial values
#'   \item \code{\link{get_XH_vars}} -- get state variables from \eqn{y}
#'   \item \code{\link{get_XH_ix}} -- get variable indices
#' }
#' 
#' @section Parameters:  
#' \itemize{
#'   \item \code{\link{get_XH_pars}} -- get parameters
#'   \item \code{\link{get_TimeSpent_matrix}} -- get the Time Spent Matrix 
#' }
#'
#'
#' @name XH_get
NULL

#' @title A list of functions to change the XH component
#'
#' @description
#' Change functions for **XH** 
#' 
#' `?change_XH`
#' \itemize{
#'   \item \code{\link{change_H}} -- change host population density
#'   \item \code{\link{change_XH_inits}} -- change initial values
#'   \item \code{\link{change_XH_pars}} -- change parameters
#' }
#'
#' @name XH_change
NULL

#' @title Setup functions for the XH component
#'
#' @description
#' Set up functions for **XH** 
#' 
#' `?setup_XH`
#' \itemize{
#'   \item \code{\link{setup_XH_obj}} -- set up the XH module object
#'   \item \code{\link{setup_XH_ix}} -- set up variable indices
#'   \item \code{\link{setup_XH_inits}} -- set up initial values
#' }
#'
#' @name XH_setup
NULL
