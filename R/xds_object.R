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
#' returned by [xds_setup] or one of its variants (see [xds_help_basic_setup]).
#' The object is created in several steps: 
#' + a basic template for an xds object is returned by 
#' [make_xds_object_template], which 
#' 
#'      + sets the value of structural parameters;
#'      + creates empty lists to hold the module objects: `XH_obj` `MY_obj,` and `L_obj`;   
#'      + sets up empty ports and junctions;
#' 
#' + the core dynamical components are setup; and 
#'
#' + the values of a few modifiable parameters affecting transmission are set. 
#' 
#' While xds object internals are useful for developers, 
#' functions have been developed to 
#' set up (`setup_*`), inspect (`get_*`), and modify (`change_*`) various
#' aspects of xds objects. 
#' 
#' @seealso Vignettes: [Getting Started](https://dd-harp.github.io/ramp.xds/articles/GettingStarted.html), [Working](https://dd-harp.github.io/ramp.xds/articles/Working.html), and [The 5-3-4 Model](https://dd-harp.github.io/ramp.xds/articles/ex_534.html)
#'   
#' @name xds_object 
NULL
