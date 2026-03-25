# WRITE ME for human-info.R 
# The sections on demography and principled
# stratification should be revisited after 
# ramp.demog v1.0 is done.

#' @title The human population
#' 
#' @description
#' A human (or host) population is sub-divided into *strata.* 
#' The human population (or other host population) and its structure, size and behavior
#' are critical aspects of transmission. 
#' 
#' \describe{
#'   \item{`HPop`}{initial human population size: `length(HPop) = nStrata`}
#' }
#' 
#' @section Human Population Size, \eqn{H}: 
#' 
#' The human (or host) population size plays a key role 
#' in the blood feeding and transmission interface. In 
#' most **XH** modules, it is handled as a variable. Setup
#' requires having some assigning a value to H, so it is passed
#' at setup as `HPop.` Since \eqn{H} is part of the 
#' blood feeding and transmission interface, changing its 
#' initial value triggers functions to refresh the dynamical 
#' terms for models where  
#' \eqn{dH/dt = 0}. 
#'  
#' @name xds_info_human_populations
NULL

#' @title Population Strata
#' 
#' @description
#' A human (or host) population is sub-divided into *strata.* 
#' The human population (or other host population) and its structure, size and behavior
#' are critical aspects of transmission. 
#' 
#' \describe{
#'   \item{`residence`}{a vector: the patch index where each human stratum lives}
#'   \item{`nStrata`}{`nStrata = length(residence)`}
#' }
#' 
#' Human / host populations are heterogeneous, and some of that heterogeneity is important
#' for accurately describing the epidemiology of malaria and other mosquito-borne pathogens.
#' **`ramp.xds`** was thus designed with the capability of segmenting a population 
#' into as many strata
#' as needed to achieve a degree of accuracy. In designing a study, decisions about
#' how to construct a model are translated into a set of population strata. Information
#' about the number of strata is configured through the `residence` vector.
#' 
#' @section Residency and Time Spent:
#' In designing **`ramp.xds`**, the first concern was how to handle spatial dynamics. 
#' Since mosquitoes and humans are very different, a metapopulation that is set up for 
#' adult mosquito ecology might not work as well for the humans, whose behaviors follow
#' very different rules.  Humans are enumerated by where
#' they live, and most people spend most of their time at risk in or around home.  
#' 
#' The human population is thus stratified around the concept of patch residency -- the
#' patch where home is found. Exposure is based on the notions of *time spent* (see [xds_info_time_spent]) 
#' and *time at risk* (see [xds_info_time_at_risk]). The model is configured by passing 
#' a vector, called `residence.`
#' The residence vector is the index of the patch where each
#' human population resides, and multiple strata can reside in the same patch.
#' Conversely, there might be some patches where no one resides, such as the mosquito
#' habitats in a buffer around the places where humans live. 
#' 
#' @section Residency Matrix: 
#' The residence vector is used to construct a 
#' residency matrix, \eqn{J}, that is the same shape as the time
#' spent and time at risk matrices. It can be used to sum quantities
#' by patch. For example, if \eqn{X} is the density of infected individuals,
#' then \eqn{J \cdot X} is the number of infected individuals by patch, so
#' true prevalence would be \deqn{\frac{J \cdot X}{J \cdot H}}.

#' @section Demography:
#' 
#' Utilities to handle host age and *cohort dynamics* are handled in 
#' **`ramp.demog`**. Similarly, the functionality to handle 
#' 
#' @section Principled Stratification: 
#' 
#' Principled stratification is the algorithm: 
#' + interventions 
#' 
#'  
#' @name xds_info_strata
NULL


