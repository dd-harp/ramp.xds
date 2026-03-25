#' @title **`ramp.xds`**: All Setup Options 
#'
#' @description
#'  
#' **Basic Setup** uses [xds_setup] or one of its variants (see [xds_info_basic_setup])
#' to create an xds object. 
#' 
#' The following is a list of configurable options 
#' and where to look for more help. 
#'  
#' @section Structural Parameters: 
#' 
#' + `nPatches` -- see [xds_info_patch_dynamics]
#' + `nStrata` -- see [xds_info_human_populations] 
#' + `nHabitats` -- see [xds_info_aquatic_habitats] 
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
#' + `Koptions` see [xds_info_mosquito_dispersal] and [setup_K_matrix]
#' + [xds_info_search_weights]
#' + [xds_info_time_spent]
#' 
#' @section Blood Feeding and Transmission: 
#' 
#' + [xds_info_available_blood_hosts]
#'    + [xds_info_search_weights_blood]
#'    + [xds_port_other_blood_hosts]
#'    + [xds_port_blood_traps]
#' + [xds_info_local_fraction]
#' 
#' @section Exposure:
#' 
#' + [xds_info_environmental_heterogeneity] 
#' + [xds_info_travel_malaria] 
#' 
#' @section Malaria Importation:
#' 
#' + [xds_info_malaria_importation]
#' + [xds_info_travel_malaria] 
#' + [xds_info_visitors] 
#' 
#' @section Mosquito Bionomics: 
#' 
#' [xds_info_mosquito_bionomics] 
#' 
#' @section Forcing:
#' 
#' [xds_info_forcing] 
#' [xds_info_trivial_forcing] 
#' 
#' @section Resources:
#' 
#' [xds_info_resources]
#' 
#' @section Habitats and Egg Laying: 
#' 
#' + [xds_info_aquatic_habitats]
#' + [xds_port_ovitraps]
#' 
#' @name xds_info_setup_options
NULL