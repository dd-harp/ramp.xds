#' @title **`ramp.xds`**: Initial Conditions
#' 
#' @description 
#' 
#' In **`ramp.xds`**, initial values for all the dependent variables are assigned
#' default values and stored during basic setup. The defaults can be overridden during setup,
#' and the stored values can be set after setup. There are several ways to learn
#' the variable names:
#' 
#' - Set up a model without passing any arguments, and examine the variable names and default initial values using: 
#'    - [get_XH_inits] to examine the stored initial conditions for the **XH** module
#'    - [get_MY_inits] to examine the stored initial conditions for the **MY** module
#'    - [get_L_inits] to examine the stored initial conditions for the **L** module
#' 
#' - Read the module documentation and follow the links to the module documentation: see [XH_module_list] or [MY_module_list] or [L_module_list]
#' 
#' - To change the value of one or more variables, pass the new values in a named list. Use the functions:
#'    - [change_XH_inits] for **XH** module variables
#'    - [change_MY_inits] for **MY** module variables
#'    - [change_L_inits] for **L** module variables
#' 
#' - The functions that are called during basic setup can also be used to change the initial values. 
#' This will forget any stored values and use the defaults if no other value is provided. 
#'    - [setup_XH_inits] for the **XH** module 
#'    - [setup_MY_inits] for the **MY** module 
#'    - [setup_L_inits] for the **L** module 
#' 
#' @name xds_info_initial_conditions
NULL