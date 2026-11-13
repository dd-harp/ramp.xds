#' @title **`ramp.xds`**: Change Parameters
#' 
#' @description
#' 
#' In **`ramp.xds`**, the parameter in one of the dynamical components can 
#' can be set during basic setup or changed after basic setup. These functions
#' require setting up *options*, a named list. There are several
#' ways to learn the names of the parameters for one of the modules:  
#' 
#' - Set up a model without passing any arguments and then use: 
#'    - [get_XH_pars] to examine the **XH** module parameter names and defaults
#'    - [get_MY_pars] to examine the **MY** module parameter names and defaults
#'    - [get_L_pars] to examine the **L** module parameter names and defaults
#' 
#' - Read the module documentation and follow the links to the module documentation: see [XH_module_list] or [MY_module_list] or [L_module_list]
#' 
#' - To change the value of a parameter, pass the new values of the parameter in a named list to:
#'    - [change_XH_pars] for **XH** module parameters
#'    - [change_MY_pars] for **MY** module parameters
#'    - [change_L_pars] for **L** module parameters
#'    
#' - The built-in functions that set up modules can also be used to change parameters. In general, these functions should not be used
#' after basic setup because they will override other changes, including biting weights and the time spent matrix: 
#'    - [setup_XH_obj] for the **XH** module 
#'    - [setup_MY_obj] for the **MY** module 
#'    - [setup_L_obj] for the **L** module 
#' 
#' @name xds_info_change_parameters
NULL

