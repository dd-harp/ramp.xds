#' @title **`ramp.xds`**: Basic Setup
#' 
#' @description
#' 
#' **Basic Setup** for **`ramp.xds`** uses 
#' [xds_setup] or one of its variants. 
#' 
#' @section Modularity:
#'   
#' **`ramp.xds`** is designed for modular computation. The
#' mathematical framework has 
#' five core biological processes organized
#' into three chunks called **dynamical components** (see [dynamical_components]).  
#' The modular design  
#' makes it possible to build models that fully define all 
#' components (using [xds_setup]), or to 
#' develop systems of equations to isolate and study some part
#' of a system. 
#' 
#' To implement *plug-and-play* modularity, a trivial module was developed
#' for each component: 
#' a *trace function* in 
#' the trivial *upstream* component is configured to pass known inputs (see [trivial_forcing]). 
#'
#' @section The Frame: 
#' 
#' [xds_setup] sets up a model  
#' that has all three components. Functions were developed to setup and solve systems that 
#  trivialized the adult mosquito component, 
#' making another component completely unnecessary. 
#' 
#' All `xds_setup_*` functions define `xds_obj$frame` 
#' and the functions used to solve and parse differential equations  
#' use the `S3` system, with methods
#' that dispatch on `class(xds_obj$frame)`. 
#'
#' + [xds_setup] (`frame="full"`) sets up all three components.
#' 
#' + [xds_setup_mosy] (`frame="mosy"`) sets up models for mosquito ecology:
#'      - the model includes an **L** component 
#'      - the model has an **M** component but no **Y** component (*eg* [basicM]) 
#'      - a trivial **H** module can be configured
#'      - the **X** component is not used 
#'       
#' + [xds_setup_aquatic] (`frame="aquatic"`) sets up models aquatic mosquito ecology: 
#'      - a trivial **MY** component is configured for egg laying 
#'      - the **XH** component is not used 
#'      
#' + [xds_setup_human] (`frame="human"`) sets up models to study epidemiology  
#'      - a trivial **MY** component is configured for infectious biting in patches 
#'      - the **L** component is not used 
#'      
#' + [xds_setup_eir] (`frame="eir"`) sets up models to study epidemiology  
#'      - a trace function computes the EIR, denoted \eqn{F_E(t)} 
#'      - the **MY** component is not used 
#'      - the **XH** component is not used 
#' 
#' @seealso [dynamical_components], [trivial_forcing] and [xds_object] 
#' 
#' @name xds_help_basic_setup
NULL