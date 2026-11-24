#' @title Glossary for **`ramp.xds`**
#'
#' @description
#' A vocabulary guide to model structure, mosquito behavior, exposure,
#' transmission, and the modular framework.
#' Entries link to the concept pages and functions that provide fuller explanations.
#'
#' @section Model Structure:
#' \describe{
#'   \item{Patch}{A spatial unit in which adult mosquito populations and
#'     interactions with resources are represented. See [xds_info_patch_dynamics].}
#'   \item{Population stratum}{A subdivision of a human/host population
#'     represented by its own population and infection variables.
#'     See [xds_info_strata].}
#'   \item{Aquatic habitat}{A location within a patch where immature mosquitoes
#'     develop. See [xds_info_aquatic_habitats].}
#'   \item{Residence}{The patch where a population stratum lives, which need not
#'     be the only patch where it spends time. See [residence_vector].}
#'   \item{Residence matrix}{A matrix that aggregates quantities in population
#'     strata by patch of residence. See [residence_matrix].}
#'   \item{Habitat Membership}{The patch location assigned to each aquatic habitat.
#'     See [habitat_vector].}
#'   \item{Habitat matrix}{A matrix that aggregates quantities in aquatic
#'     habitats by patch. See [habitat_matrix].}
#' }
#'
#' @section Mosquito Behavior and Resources:
#' \describe{
#'   \item{Search weight}{A weight assigned to a host stratum or habitat
#'     that contributes to its availability to searching mosquitoes.
#'     See [xds_info_search_weights].}
#'   \item{Availability}{A measure of accessibility to mosquitoes, rather than
#'     abundance alone, that incorporates search weights and, for hosts,
#'     time at risk. See [available_blood_hosts] and [habitat_availability].}
#'   \item{Blood feeding}{The interaction between mosquitoes searching for
#'     blood hosts and hosts spending time in patches. See [blood_feeding].}
#'   \item{Egg laying}{The allocation of eggs laid by adult mosquitoes in
#'     patches among aquatic habitats. See [egg_laying].}
#'   \item{Laying matrix}{A matrix that distributes eggs from patches to
#'     aquatic habitats according to relative availability. See [laying_matrix].}
#'   \item{Emergence}{The transition from aquatic immature stages to adult
#'     mosquitoes in patches. See [xds_info_emergence].}
#'   \item{Bionomics}{The demographic and behavioral properties of mosquitoes,
#'     including feeding, survival, movement, and development.
#'     See [xds_info_mosquito_bionomics].}
#'   \item{Functional response}{A relationship that expresses a mosquito
#'     bionomic parameter as a function of resource availability.
#'     See [blood_feeding].}
#' }
#'
#' @section Movement and Exposure:
#' \describe{
#'   \item{Time spent}{The fraction of time a population stratum spends
#'     in each patch when not traveling. See [xds_info_time_spent].}
#'   \item{Time at risk}{Time spent when not traveling, weighted by mosquito
#'     activity. See [time_at_risk].}
#'   \item{Time away}{The fraction of time spent outside the spatial domain
#'     represented by the patches. See [xds_port_time_away].}
#'   \item{Travel malaria}{Exposure to malaria outside the modeled spatial
#'     domain. See [xds_info_travel_malaria].}
#'   \item{Visitor}{A non-resident host whose availability and infectiousness
#'     contribute to blood feeding and transmission in patches.
#'     See [xds_info_visitors].}
#'   \item{Local fraction}{The fraction of blood feeding on local hosts
#'     among blood meals on local hosts and visitors.
#'     See [xds_info_local_fraction].}
#'   \item{Environmental heterogeneity}{Variation in exposure within a
#'     population stratum, represented through a model for the distribution
#'     of bites. See [xds_info_environmental_heterogeneity].}
#'   \item{Relative biting rate}{A stratum's daily EIR relative to the
#'     population average daily EIR. See [xds_info_relative_biting_rates].}
#' }
#'
#' @section Infection and Transmission:
#' \describe{
#'   \item{Mixing matrix}{The biting distribution matrix that maps biting
#'     density in patches to per-capita biting rates in population strata.
#'     See [mixing_matrix].}
#'   \item{HBR}{Human/host biting rate: the number of mosquito bites per
#'     person or host per day. See [F_hbr].}
#'   \item{EIR}{Entomological inoculation rate: the number of infective bites
#'     per person or host per day. See [F_eir] and [xds_info_transmission].}
#'   \item{FoI}{Force of infection: the rate of acquiring infection.
#'     See [F_foi] and [xds_info_exposure].}
#'   \item{Attack rate}{The probability of acquiring infection over a time
#'     step. See [F_ar] and [Exposure].}
#'   \item{Infected mosquito}{A mosquito carrying infection, which need not
#'     yet be capable of transmitting it. See [MY_module_list].}
#'   \item{Infective mosquito}{A mosquito capable of transmitting the
#'     pathogen through a bite. See [F_fqZ].}
#'   \item{Infectivity}{The probability of host infection per infective bite,
#'     potentially affected by immunity. See [F_infectivity].}
#'   \item{Infectiousness}{The capacity of an infected host population to
#'     infect mosquitoes during blood feeding. See [F_I].}
#'   \item{Net infectiousness}{The probability that a mosquito becomes
#'     infected after feeding on the modeled hosts and visitors in a patch.
#'     See [F_kappa] and [xds_info_transmission].}
#' }
#'
#' @section Framework Terminology:
#' \describe{
#'   \item{Component}{A group of related dynamical processes: **XH** for
#'     host demography and infection, **MY** for adult mosquito ecology and
#'     infection, or **L** for immature mosquito ecology.
#'     See [xds_info_dynamical_components].}
#'   \item{Module}{An implementation of a dynamical component, selected
#'     during setup. See [xds_info_dynamical_components].}
#'   \item{Interface}{A standardized set of computations that connects
#'     dynamical processes. See [xds_info_interfaces].}
#'   \item{Port}{A parameter or variable whose value is set through a
#'     standardized function call with a flexible implementation.
#'     See [xds_info_port].}
#'   \item{Junction}{A function that coordinates a bundle of ports.
#'     See [xds_info_junction].}
#'   \item{Forcing}{Externally specified variation in parameters or
#'     dynamical terms, implemented through ports, junctions, or trivial
#'     modules. See [xds_info_forcing] and [xds_info_trivial_forcing].}
#' }
#'
#' @seealso [xds_help_info], [xds_help_guide]
#' @name xds_glossary
#' @aliases xds_info_glossary
NULL
