# X

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
#' adult mosquito ecology, [xds_info_blood_feeding], and [xds_info_transmission].
#' 
#' Each patch could contain some number of
#' aquatic habitats (see [xds_info_aquatic_habitats]). Mosquitoes disperse among patches
#' as they search for resources, including blood hosts and aquatic habitats
#' (see [xds_info_mosquito_dispersal]).
#' 
#' Patches are used to model blood feeding (see [xds_info_blood_feeding]), so there is a corresponding human model 
#' for  
#' time spent (see [xds_info_time_spent]).  
#' 
#' @name xds_info_patch_dynamics 
NULL