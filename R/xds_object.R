#' @title The **`xds`** Model Object  
#' 
#' @description
#' A dynamical system is encoded and stored 
#' on an **`xds`** model 
#' object, 
#' a compound [list] with 
#' a specific set of features. 
#'  
#' A properly constructed xds object is
#' returned by [xds_setup] or one of its variants (see [xds_object_frame]).
#' The object is created in several steps: 
#' + a basic template for an xds object is returned by 
#' [make_xds_object_template], which 
#' 
#'      + sets the value of structural parameters;
#'      + creates empty lists to hold the module objects: `XH_obj` `MY_obj,` and `L_obj`;   
#'      + sets up empty [port]s and [junction]s;
#' 
#' + the core dynamical components are setup; and 
#'
#' + the values of a few modifiable parameters affecting transmission are set. 
#' 
#' While xds object internals are useful for developers, 
#' functions have been developed to 
#' set up (`setup_*`), inspect (`get_*`), and modify (`change_*) various
#' aspects of xds objects. 
#' 
#' @seealso [Getting Started](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html), [The xds Model Object](https://dd-harp.github.io/ramp.xds/articles/Working.html), and [The 5-3-4 Model](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
#' 
#'
#' 
#' 
#'  
#'   
#' @name xds_object 
#' @example
#' mod <- xds_setup
#' names(mod)
#' get_XH_pars(mod)
NULL

#' @title The xds object frame
#' 
#' @description
#' There are several variants of [xds_setup] that set the value of `xds_obj$frame.`
#' 
#' The modular design makes it possible to study a 
#' dynamical component in isolation, or to focus on some 
#' combination of those elements. 
#' 
#' To facilitate those studies, each component has a trivial module 
#' that computes the dynamical term that gets passed to  
#' another component (see [trivial_forcing]). The user configures the *trace function* 
#' of the upstream component.  
#' 
#' With [xds_setup], a user defines all three components, but other variants 
#' were developed to make it easier to set up partial systems. The functions that solve 
#' differential equations and parse outputs are written as `S3` functions with methods
#' that dispatch on `xds_obj$frame`
#'
#' + [xds_setup] (`frame="full"`) sets up all three components.
#' 
#' + [xds_setup_mosy] (`frame="mosy"`) sets up models for mosquito ecology:
#'      - the model includes an **L** component 
#'      - the model has an **M** component but no **Y** component (*eg* [basicM]) 
#'      - a trivial **H** module can be configured, but the **XH** derivatives are not computed 
#' + [xds_setup_aquatic] (`frame="aquatic"`) sets up models aquatic mosquito ecology: 
#'      - a trivial **MY** component is configured for egg laying 
#'      - the **XH** component is missing 
#' + [xds_setup_human] (`frame="human"`) sets up models to study epidemiology  
#'      - a trivial **MY** component is configured for infectious biting in patches 
#'      - the **L** component is missing 
#' + [xds_setup_eir] (`frame="eir"`) sets up models to study epidemiology  
#'      - a function \eqn{F_E(t),} a trace function that passes the EIR.  These models lack the **MY** and **L** components.  
#'      - a trivial **MY** component is configured for infectious biting in patches 
#'      - the **XH** component is missing 
#' 
#' @seealso [trivial_forcing] and [xds_object] 
#' 
#' @example
#'   names(xds_setup()) 
#' @name xds_object_frame
NULL