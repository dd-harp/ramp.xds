#' @title **`ramp.xds`**: All Setup Options 
#'
#' @description
#'  
#' **Basic Setup** uses [xds_setup] or one of its variants (see [xds_help_basic_setup])
#' to create an xds object. 
#' 
#' The following is a list of configurable options 
#' and where to look for more help. 
#'  
#' @section Structural Parameters: 
#' 
#' + `nPatches` -- see [patch_dynamics]
#' + `nStrata` -- see [human_populations] 
#' + `nHabitats` -- see [aquatic_habitats] 
#' 
#' @section Modules: 
#' 
#' + `Xname` -- see [XH_module_list]
#' + `MYname`-- see [MY_module_list]
#' + `Lname` -- see [L_module_list]
#' 
#' Parameter values and initial values for the variables are 
#' passed as named lists. The module help pages have information
#' about how to configure: 
#' `Xoptions,` 
#' `MYoptions,` & 
#' `Loptions.` 
#'
#' @section Basic options:  
#' 
#' + `Koptions` see [mosquito_dispersal] and [setup_K_matrix]
#' + [search_weights]
#' + [time_spent]
#' 
#' @section Blood Feeding and Transmission: 
#' 
#' + [available_blood_hosts]
#'    + [blood_search_weights]
#'    + [other_blood_hosts]
#'    + [blood_traps]
#' + [local_frac]
#' 
#' @section Exposure:
#' 
#' + [environmental_heterogeneity] 
#' + [travel_malaria] 
#' 
#' @section Malaria Importation:
#' 
#' + [time_spent_here] 
#' + [travel_malaria] 
#' 
#' @section Mosquito Bionomics: 
#' 
#' [mosquito_bionomics] 
#' 
#' @section Forcing:
#' 
#' [xds_forcing] 
#' [trivial_forcing] 
#' [xds_junction_forcing] 
#' 
#' @section Resources:
#' 
#' [xds_junction_resources]
#' 
#' @section Habitats and Egg Laying: 
#' 
#' + [bad_habitats]
#' + [ovitraps]
#' 
#' @name xds_help_setup_options
NULL