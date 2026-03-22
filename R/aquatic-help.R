
#' @title Generic methods for the L component
#'
#' @description
#' A list of generic 
#' \itemize{
#'   \item \code{\link{dLdt}} -- compute derivatives
#'   \item \code{\link{Update_Lt}} -- update state variables
#'   \item \code{\link{LBionomics}} -- compute bionomic parameters
#'   \item \code{\link{LEffectSizes}} -- apply vector control effect sizes
#'   \item \code{\link{F_emerge}} -- compute emergent adults
#'   \item \code{\link{setup_L_obj}} -- set up the L module object
#'   \item \code{\link{setup_L_ix}} -- set index values
#'   \item \code{\link{setup_L_inits}} -- set up initial values
#'   \item \code{\link{get_L_inits}} -- get initial values
#'   \item \code{\link{change_L_inits}} -- change initial values
#'   \item \code{\link{get_L_vars}} -- list state variables
#'   \item \code{\link{get_L_pars}} -- get parameters
#'   \item \code{\link{change_L_pars}} -- change parameters
#'   \item \code{\link{parse_L_orbits}} -- parse outputs
#'   \item \code{\link{get_L_orbits}} -- get saved orbits
#'   \item \code{\link{steady_state_L}} -- compute steady states
#'   \item \code{\link{check_L}} -- run consistency checks
#'   \item \code{\link{skill_set_L}} -- return module skill set
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
#' }
#'
#' @name L_change
NULL

#' @title Setup functions (**L**)
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
