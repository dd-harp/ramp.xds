# X

#' @title Change Parameters
#'
#' @description
#'
#' In **`ramp.xds`**, parameter values for the dynamical components
#' can be set during basic setup or changed after basic setup. Values to
#' override defaults or replace existing values are passed by name
#' in `options` (a [list]). To learn
#' the parameter names:
#'
#' - Set up a model without setting `options` and then use:
#'    - [get_XH_pars] to examine the **XH** module parameter names and defaults
#'    - [get_MY_pars] to examine the **MY** module parameter names and defaults
#'    - [get_L_pars] to examine the **L** module parameter names and defaults
#'
#' - To change parameter values, pass the new values in a named list as `options` to:
#'    - [change_XH_pars] for **XH** module parameters
#'    - [change_MY_pars] for **MY** module parameters
#'    - [change_L_pars] for **L** module parameters
#'
#' - The module setup functions reinitialize an existing module and may overwrite parameter changes (use with caution):
#'    - [setup_XH_obj] for the **XH** module
#'    - [setup_MY_obj] for the **MY** module
#'    - [setup_L_obj] for the **L** module
#'
#' - Read the module documentation
#'
#' @seealso [XH_module_list], [MY_module_list], [L_module_list], [xds_basic_setup]
#' @name xds_change_parameters
#' @aliases xds_info_change_parameters
NULL
