#' @title Patch Dynamics 
#' 
#' @description
#' 
#' \describe{
#'   \item{`nPatches`}{the number of patches}
#' } 
#' 
#' A spatial domain is sub-divided into a set of patches.
#' The patches are used to model 
#' adult mosquito ecology, [blood_feeding], and [Transmission].
#' 
#' Each patch could some number of 
#' aquatic habitats (see [aquatic_habitats]). Mosquitoes disperse among patches as 
#' they search for or resources and blood feed (if there are any blood hosts), lay eggs (if there are any aquatic habitats), nd other resources 
#' they need (see [mosquito_dispersal]). 
#' 
#' Patches are used to model blood feeding (see [blood_feeding]), so there is a corresponding human model 
#' for  
#' time spent (see [time_spent]).  
#' 
#' @name patch_dynamics 
NULL