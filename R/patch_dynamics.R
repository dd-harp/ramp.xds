#' @title Patch Dynamics 
#' 
#' @description
#' 
#' Spatial dynamics in **`ramp.xds`** are patch-based, also known as 
#' *metapopulation dynamics.* 
#' A *patch* is the primary spatial unit for modeling transmission, and 
#' the number of patches is a core structural parameter:
#' 
#' \describe{
#'   \item{`nPatches`}{the number of patches }
#' } 
#' 
#' In patch-based models, a patch is a 
#' spatial unit that structures
#' adult mosquito populations. 
#' A patch could have any number of 
#' aquatic habitats (see [aquatic_habitats]), and mosquitoes disperse among patches as 
#' they search for blood hosts, aquatic habitats to lay eggs, and other resources 
#' they need (see [mosquito_dispersal]). 
#' 
#' Patches are used to model blood feeding (see [blood_feeding]), so there is a corresponding human model 
#' for  
#' time spent (see [time_spent]).  
#' 
#' @name patch_dynamics 
NULL