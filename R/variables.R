#' @title Variables 
#' 
#' @description 
#' The term *variables* is overloaded. 
#' 
#' @section Terminology: 
#' 
#' In **`ramp.xds,`**  there are several kinds of variables and terms:
#' 
#' + the **independent variable** is usually time, but it is sometimes age 
#' 
#' + the **dependent variables** or *state variables* describe the *state* of the system 

#'       - most dependent variables are computed as part of a **Core Dynamical Components**
#'       
#'       - **other state variables** are not can be set up and computed along with state variables (see [Other_State_Variables])
#' 
#' + **accessory variables** are computed for other reasons. For example, accessory variables are used in delay differential equations to internalize computation of lagged terms using [lagderiv] (see [Delay Equations](https://dd-harp.github.io/ramp.xds/articles/Delays.html)) 
#'  
#' @name variables 
NULL

#' @title Other State Variables 
#' 
#' @description 
#' For extensibility, **`ramp.xds`** is capable of setting up and computing *other state variables,* 
#' or *dependent variables* that are not part of one of the core dynamical components. 
#' 
#' @seealso [variables] 
#'  
#' @name Other_State_Variables 
NULL