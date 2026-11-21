# X 

#' @title Change Initial Conditions
#'
#' @description
#'
#' In **`ramp.xds`**, initial values for all the dependent variables are assigned
#' default values and stored during basic setup.
#' Values to override defaults or replace existing values are passed by name to `options` in a [list].
#' To learn the variable names:
#'
#' - Set up a model without setting `options` and then use:
#'    - [get_XH_inits] to examine the stored initial conditions for the **XH** module
#'    - [get_MY_inits] to examine the stored initial conditions for the **MY** module
#'    - [get_L_inits] to examine the stored initial conditions for the **L** module
#'
#' - To change the value of one or more variables, pass the new values by name in a list `options` to:
#'    - [change_XH_inits] for the **XH** module variables
#'    - [change_MY_inits] for the **MY** module variables
#'    - [change_L_inits] for the **L** module variables
#'
#' - Module setup functions replace the existing initial values (use with caution):
#'    - [setup_XH_inits] for the **XH** module
#'    - [setup_MY_inits] for the **MY** module
#'    - [setup_L_inits] for the **L** module
#'
#' - Read the module documentation
#'
#' Two useful utilities for initial values are:
#'
#' - [last_to_inits] --- after calling [xds_solve], replace the initial values with the final values
#' - [burnin] --- runs the model from `t_neg_inf` to 0 and then calls [last_to_inits]
#'
#'
#' @seealso [XH_module_list], [MY_module_list], [L_module_list], [xds_basic_setup]
#'
#' @name xds_initial_conditions
#' @aliases xds_info_initial_conditions
NULL
