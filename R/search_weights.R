#' @title Search Weights 

#' @description
#' 
#' \describe{
#'   \item{`searchB`}{a vector of search weights for blood feeding: `length(searchB)=nStrata`}
#'   \item{`searchQ`}{a vector of search weights for egg laying: `length(searchQ)=nHabitats`}
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
#' The value of these search weights are configurable during basic setup.
#' 
#' @name search_weights
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
#' Search weights for blood feeding thus play a key role in blood feeding (see [blood_feeding])
#' 
#' @name blood_search_weights
NULL

#' @title Habitat Search Weights 
#' 
#' @description 
#' where
#'  
#' @seealso [blood_feeding], [Transmission], and [egg_laying]
#' @name habitat_search_weights
NULL