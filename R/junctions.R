#' @title Ports 
#' 
#' @description
#' In **`ramp.xds,`** a *port* is any parameter or term that has  
#' a flexible implementation, usually by calling an `S3` function.
#' Ports are an important design feature that make the software
#' extensible through the addition of new `S3` methods. 
#' 
#' By policy, the default static values assigned to ports at basic 
#' setup are the "simplest" method. Other methods in **`ramp.xds`** 
#' are easily configured through a `setup_*` function. 
#' 
#' Examples:
#' + [mosquito_bionomics] parameters in some **MY** modules are handled as ports 
#' 
#'  
#' @name port 
NULL

#' @title Junctions 
#' 
#' @description
#' In **`ramp.xds,`** clusters of [port]s are bundled 
#' into a **junction.** A junction is a structural element, 
#' a function that is designed to handle one or more [port]s. 
#' 
#' + [Other_State_Variables] -- a junction to add new state variables, effectively extending the dynamical system  
#' + [Forcing] -- a junction to handle exogenous forcing by weather, *etc.*  
#' + [Vector_Control] -- a junction to handle vector control  
#'  
#' @name junction 
NULL
