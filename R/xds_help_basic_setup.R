#' @title **`ramp.xds`**: Guide to Basic Setup
#' 
#' @description
#' **Basic Setup** uses one of the following functions to build an
#' **`xds`** model: 
#'
#' + [xds_setup] sets up a full model with:
#'      - an **XH** Component 
#'      - an **MY** Component 
#'      - an **L** Component 
#'      
#' + [xds_setup_human] sets up a models to study human epidemiology, given exposure: 
#'      - an **XH** component
#'      - a trivial **MY** component is configured for infectious biting in patches 
#'      - the time spent matrix is used
#'      - the **L** component is not used 
#'      
#' + [xds_setup_eir] sets up models to study human epidemiology, given exposure:
#'      - an **XH** component with one population stratum 
#'      - a *trace function* to output the daily EIR
#'      - a function configures relative biting rates by age for cohort dynamics  
#'      - the **MY** component is not used 
#'      - the **L** component is not used 
#' 
#' + [xds_setup_mosy] sets up models for mosquito ecology:
#'      - the **L** component 
#'      - the **M** component (*eg* [basicM]) 
#'      - a trivial **H** module (for blood feeding) 
#'      - the **X** and **Y** components are not used 
#'       
#' + [xds_setup_aquatic] sets up models for aquatic mosquito ecology: 
#'      - the **L** component
#'      - a trivial **MY** component is configured for egg laying 
#'      - the **XH** component is not used 
#' 
#' @section The Frame:  
#' Each one of the basic setup functions assigns a different value to `class(xds_obj$frame),` 
#' which dispatches the `S3` methods that solve and parse differential equations: 
#' + [xds_setup] sets `class(frame) = "full"`
#' + [xds_setup_human] sets `class(frame) = "human"`
#' + [xds_setup_eir] sets `class(frame) = "eir"`
#' + [xds_setup_mosy] sets `class(frame) = "mosy"`
#' + [xds_setup_aquatic] sets `class(frame) = "aquatic"`
#' 
#' @seealso [dynamical_components], [trivial_forcing] and [xds_object] 
#' 
#' @name xds_help_basic_setup
NULL