#' @title Dynamical Components 
#' 
#' @description
#' Dynamical systems for malaria and other mosquito-borne diseases
#' are made up of three core **dynamical components:**
#' 
#' + **XH** - a system of equations describing the dynamics of host infection and immunity (**X**) and demography (**H**)
#' 
#' + **MY** - a system of equations describing adult mosquito ecology (**M**) and infection dynamics (**Y**) 
#'
#' + **L** - a system of equations describing immature mosquito ecology 
#' 
#' During setup, modules are selected by passing their names: 
#' 
#'  \describe{
#'   \item{`Xname`}{a character string: module name for **X** Component module}
#'   \item{`MYname`}{a character string: module name for **MY** Component module}
#'   \item{`Lname`}{a character string: module name for **L** Component module}
#' }
#'  
#' Other variables can also be added to the dynamical system (see [Other_State_Variables]). 
#' 
#' @seealso [xds_object_frame]
#' 
#' @name dynamical_components 
NULL

