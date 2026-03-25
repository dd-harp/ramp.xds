#' @title Environmental Heterogeneity 
#' 
#' @description
#' Humans can be exposed to malaria at home or as they move around. 
#' The number of infectious bites can vary by chance, depending on 
#' where and when humans encounter mosquitoes.  
#' Environmental heterogeneity models 
#' exposure for homogeneous population strata under a model for the distribution
#' of the expectation. In simple terrms, it models super-spreading without super-spreaders. 
#' 
#' + Poisson
#' + Negative binomial 
#'   
#' @name xds_info_environmental_heterogeneity 
NULL

#' @title Heterogeneity 
#' 
#' @description 
#' **`ramp.xds`** was designed to explore heterogeneous exposure & transmission:  
#' 
#' + *heterogeneous biting rates* - differences in the expected number of expected
#' bites, per person, among heterogeneous biting rates: 
#' see [xds_info_heterogeneous_biting_rates].
#' 
#' + *environmental heterogeneity* - describes variability in the distribution 
#' of the number of bites, per person in a homogenous population stratum: 
#' see [xds_info_environmental_heterogeneity] 
#'   
#' + heterogeneous mixing describes 
#'  
#' 
#' @name xds_info_heterogeneity
NULL

#' @title Heterogeneous Biting Rates 
#' 
#' @description 
#' Heterogeneous biting rates are closely 
#' related to *relative biting 
#' rates:* for a stratum, the relative biting rate is 
#' a value \eqn{\omega} such that if the
#' population average daily EIR is \eqn{E}, then the
#' daily EIR for the stratum is \eqn{\omega E}. 
#' 
#' Models for heterogeneous biting are constructed
#' by passing *blood search weights* for human population 
#' strata, \eqn{w}. If the time spent in a patch 
#' is \eqn{\theta}, then 
#' the availability of humans in that patch is is: 
#' \deqn{W_j = \sum_i w_i \theta_{i,j} H_i.} 
#' The fraction of bites received by the \eqn{i^{th}} 
#' stratum in patch \eqn{j} is: 
#' \deqn{\frac{w_i \theta_{i,j} H_i}{W_j}} 
#' The daily EIR is:
#' \deqn{E_i = w_i \sum_j \frac{\theta_{i,j} }{W_j}f_j q_j Z_j} 
#' The population average EIR is  
#' \deqn{E = \sum_i E_i H_i/ \sum_i H_i} 
#' The relative biting rate is  
#' \deqn{\omega_i = E_i / E} 
#' 
#' @name xds_info_heterogeneous_biting_rates
NULL


#' @title Relative Biting Rate
#' 
#' @description In **`ramp.xds`**, heterogeneous
#' biting rates are handled by assigning *search weights*
#' to various population strata. The *relative biting 
#' rate* for a stratum is a value \eqn{\omega} such that if the
#' population average daily EIR is \eqn{E}, then the
#' daily EIR for the stratum is \eqn{\omega E}. 
#' 
#' Relative biting rates are handled in two ways, depending on the frame:
#' + In model that define multiple human population strata, the RBRs are computed from the 
#' stratum specific EIR: \deqn{\omega_i = E_i / E} 
#' + In models that are forced by the EIR, the user configures a function
#' to compute the population average EIR over time, and another function to 
#' compute the relative biting 
#' rate by age.  
#'  
#'   
#' @seealso [xds_info_search_weights_blood] | [xds_info_heterogeneous_biting_rates] | [xds_info_heterogeneity]
#' 
#' @name xds_info_relative_biting_rates
NULL