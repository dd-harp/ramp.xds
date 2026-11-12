#' @title Dynamical Components 
#' 
#' @description
#' 
#' In basic setup, modules are selected by passing the name as a character string: 
#' 
#'  \describe{
#'   \item{`XHname`}{**XH** Component module name: see [XH_module_list]}
#'   \item{`MYname`}{**MY** Component module name: see [MY_module_list]}
#'   \item{`Lname`}{**L** Component module name: see [L_module_list]}
#' }
#' 
#' @section Modules: 
#'  
#' Dynamical systems for malaria and other mosquito-borne diseases
#' are made up of three core **dynamical components** that describe 
#' five core processes. There is also a generic interface to add other variables:  
#' 
#' + **XH** - a system of equations describing two inextricably linked processes:
#' 
#'    - **X** - the dynamics of host infection and immunity 
#'    
#'    - **H** - human / host demography and behavior
#' 
#' + **MY** - a system of equations describing two inextricably linked processes:
#' 
#'      - **M** - adult mosquito ecology 
#'      
#'      - **Y** - parasite / pathogen infection dynamics in mosquitoes 
#'
#' + **L** - a system of equations describing immature mosquito ecology 
#' 
#' + **V** - other dynamical components: see [Other_State_Variables] 
#' 
#' @section Internals: 
#' 
#' - **`xds_obj$XH_obj[[1]]`** defines the module for human / host infection dynamics of class `XHname` (the **XH** component). The parameter values are passed in a named list, `XHoptions`.
#' 
#' - **`xds_obj$MY_obj[[1]]`** defines the module for adult mosquito ecology & infection dynamics of class `MYname` (the **MY** Component). The parameter values are passed in a named list, `MYoptions`.
#' 
#' - **`xds_obj$L_obj[[1]]`** defines the module for aquatic mosquito ecology of class `Lname` (the **L** Component). The parameter values are passed in a named list, `Loptions`.
#' 
#' @seealso [xds_info_basic_setup]
#' 
#' @name xds_info_dynamical_components 
NULL
