# X

#' @title Search Weights 

#' @description
#' 
#' \describe{
#'   \item{`XH_obj[[i]]$search_weights`}{blood-feeding search weights for host species `i`, with length `nStrata[i]`}
#'   \item{`L_obj[[s]]$search_weights`}{egg-laying search weights for vector species `s`, with length `nHabitats`}
#' }
#' 
#' By default, the values are all set to 1. 
#'  
#' Mosquitoes search for resources, including vertebrate animals for
#' blood feeding and aquatic habitats for egg laying. 
#' In developing a modular design, a 
#' rigorous interface was needed to guarantee mathematical
#' consistency for blood feeding 
#' and parasite transmission, and for adult mosquito ecology 
#' and behaviors around egg laying.  
#' Models for *heterogeneous biting* among host strata and
#' *heterogeneous egg laying* among habitats are implemented through *search weights.* 
#' 
#' A search 
#' weight is a scalar value assigned to each human population stratum
#' or habitat that is used to compute *availability*
#' to mosquitoes and relative biting rates or relative laying rates.
#' The values of these search weights are configurable during basic setup.
#' 
#' @name xds_info_search_weights
NULL


#' @title Blood Search Weights
#' 
#' @description 
#' 
#' Blood feeding is an interaction among humans and mosquitoes: 
#' in this model, humans are spending time, and 
#' mosquitoes are searching. To model exposure, each human (or host) 
#' population stratum is assigned a *search weight,* a  
#' number used to weigh *time spent* and get a measure of 
#' *availability.*
#' Search weights for blood feeding thus play a key role in modeling blood feeding. 
#' 
#' @seealso [xds_info_blood_feeding] 
#' @name xds_info_search_weights_blood
NULL

#' @title Habitat Search Weights 
#' 
#' @description 
#' Each aquatic habitat is assigned a search weight that contributes to its
#' availability to mosquitoes and the allocation of eggs among habitats.
#' See [setup_habitat_search_weights] for configuration.
#'  
#' @seealso [xds_info_blood_feeding], [xds_info_transmission], and [xds_info_egg_laying]
#' @name xds_info_search_weights_habitat
NULL