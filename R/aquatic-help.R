# X
#' @title Functions for the L component
#'
#' @description
#' A list of functions for the L component
#' \itemize{
#'   \item \code{\link{dLdt}} -- compute derivatives
#'   \item \code{\link{Update_Lt}} -- update state variables
#'   \item \code{\link{LBionomics}} -- compute baseline bionomic parameter values
#'   \item \code{\link{LEffectSizes}} -- apply vector control effect sizes
#'   \item \code{\link{F_emerge}} -- compute emergent adults
#'   \item \code{\link{setup_L_obj}} -- set up the L module object
#'   \item \code{\link{setup_L_ix}} -- set up variable indices
#'   \item \code{\link{get_L_ix}} -- get variable indices
#'   \item \code{\link{setup_L_inits}} -- set up initial values
#'   \item \code{\link{get_L_inits}} -- get initial values
#'   \item \code{\link{change_L_inits}} -- change initial values
#'   \item \code{\link{get_L_vars}} -- get the state variables
#'   \item \code{\link{get_L_pars}} -- get parameters
#'   \item \code{\link{change_L_pars}} -- change parameters
#'   \item \code{\link{parse_L_orbits}} -- parse outputs
#'   \item \code{\link{get_L_orbits}} -- get saved orbits
#'   \item \code{\link{xds_plot_L}} -- plot aquatic mosquito population density
#'   \item \code{\link{xds_lines_L}} -- add lines for aquatic mosquito population density
#'   \item \code{\link{steady_state_L}} -- compute steady states
#'   \item \code{\link{check_L}} -- run consistency checks
#'   \item \code{\link{get_skillset_L}} -- return module skill set
#' }
#'
#' @name L_functions
NULL

#' @title Get functions (**L**)
#'
#' @description
#' A list of get functions for aquatic mosquito modules.
#'
#' \itemize{
#'   \item \code{\link{get_L_vars}} -- get state variables from \eqn{y}
#'   \item \code{\link{get_L_ix}} -- get variable indices
#'   \item \code{\link{get_L_inits}} -- get initial values
#'   \item \code{\link{get_L_pars}} -- get parameters
#'   \item \code{\link{get_L_orbits}} -- get saved orbits
#'   \item \code{\link{get_psi}} -- get the maturation rate
#'   \item \code{\link{get_phi}} -- get the density-independent mortality rate
#'   \item \code{\link{get_xi}} -- get the delayed maturation parameter
#'   \item \code{\link{get_theta}} -- get the slope of density-dependent mortality
#' }
#'
#' @name L_get
NULL

#' @title Change functions (**L**)
#'
#' @description
#' A list of change functions for aquatic mosquito modules.
#'
#' \itemize{
#'   \item \code{\link{change_L_inits}} -- change initial values
#'   \item \code{\link{change_L_pars}} -- change parameters
#'   \item \code{\link{change_psi}} -- change the maturation rate
#'   \item \code{\link{change_phi}} -- change the density-independent mortality rate
#'   \item \code{\link{change_xi}} -- change the delayed maturation parameter
#'   \item \code{\link{change_theta}} -- change the slope of density-dependent mortality
#' }
#'
#' @name L_change
NULL

#' @title Set up functions (**L**)
#'
#' @description
#' A list of setup functions for aquatic mosquito modules.
#'
#' \itemize{
#'   \item \code{\link{setup_L_obj}} -- set up the L module object
#'   \item \code{\link{setup_L_ix}} -- set up variable indices
#'   \item \code{\link{setup_L_inits}} -- set up initial values
#' }
#'
#' @name L_setup
NULL
