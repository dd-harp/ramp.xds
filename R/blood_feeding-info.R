#' @title The residence vector
#'
#' @description
#' Each human/host population stratum resides in a patch.
#'
#' The residence vector is passed to a basic setup function
#' through the argument `residence`.
#'
#' + `residence` is a vector of positive integers indexing patches
#' + `nStrata <- length(residence)` for a single host species
#'
#' @section Residence Vector:
#' The residence vector indexes the patch where each population stratum resides.
#' If the \eqn{i^{th}} stratum resides in the \eqn{j^{th}} patch, then
#' `residence[i] = j`.
#'
#' Residence identifies where a stratum lives, not necessarily where it spends
#' all its time. Movement within the spatial domain is described by the
#' time spent matrix (see [time_spent_matrix]).
#'
#' @note The residence vector is stored on the **XH** module object.
#' Each host species has its own residence vector.
#'
#' @seealso [xds_basic_setup], [get_residence], [residence_matrix]
#' @name residence_vector
NULL

#' @title The residence matrix
#'
#' @description
#' \describe{
#'   \item{\eqn{\mathbf J}}{the residence matrix}
#' }
#'
#' @section Residence Matrix:
#' The residence matrix \eqn{(\mathbf J)} is an `nPatches` \eqn{\times}
#' `nStrata` matrix constructed from the [residence_vector] for a host species.
#'
#' If the \eqn{i^{th}} stratum resides in the \eqn{j^{th}} patch, then
#' \eqn{J_{j,i}=1}. Otherwise, \eqn{J_{j,i}=0}.
#'
#' If \eqn{x} is any vector describing a quantity in population strata
#' (*i.e.*, \eqn{\left|x\right|=}`nStrata`), then
#' \deqn{X = \mathbf J \cdot x}
#' sums \eqn{x} by patch of residence, and \eqn{\left|X\right|=}`nPatches`.
#' In particular, \eqn{\mathbf J \cdot H} gives the resident population
#' in each patch from the stratum population densities \eqn{H}.
#'
#' The residence matrix initializes the time spent matrix during basic setup.
#' Time spent and time at risk can subsequently differ from residence.
#'
#' @seealso [residence_vector], [make_residence_matrix],
#' [get_residence_matrix], [xds_info_time_spent], [available_blood_hosts]
#' @name residence_matrix
NULL

#' @title Human Availability
#'
#' @description
#' \describe{
#'   \item{\eqn{W_H}}{local human/host availability in patches}
#' }
#'
#' @section Human Availability:
#' Blood feeding is modeled as an interaction between hosts that spend time in
#' patches and mosquitoes that search for blood hosts. Each host population
#' stratum has a per-capita search weight \eqn{(w)}. The fraction of time
#' spent outside the patches is time away \eqn{(\delta)} (see [time_away]), so
#' the population density present in the spatial domain is \eqn{(1-\delta)H}.
#'
#' The time-spent matrix \eqn{(\Theta)} describes how hosts allocate their
#' time among patches. Ambient host population density by patch is
#' \deqn{A = \Theta \cdot ((1-\delta)H),}
#' where vector multiplication is element-wise.
#'
#' The time-at-risk matrix \eqn{(\Psi)} weights time spent by relative
#' mosquito activity over the day (see [time_at_risk]). Human availability
#' in patches is
#' \deqn{W_H = \Psi \cdot (w(1-\delta)H),}
#' where the product inside the parentheses is element-wise. Search weights
#' are specific to host species and shared across vector species. Availability
#' can differ by vector species because each can have a different
#' time-at-risk matrix.
#'
#' @seealso [available_blood_hosts], [F_W_available], [time_spent_matrix],
#' [time_at_risk]
#' @name human_availability
#' @aliases host_availability
#' @aliases xds_info_available_humans
NULL

#' @title Available Blood Hosts
#'
#' @description
#' \describe{
#'   \item{\eqn{W_H}}{local human/host availability in patches}
#'   \item{\eqn{B}}{total availability to blood-seeking mosquitoes in patches}
#' }
#' 
#' @section Total Blood Host Availability:
#' Local human/host availability \eqn{(W_H)} is computed as described in
#' [human_availability]. Visitors, other blood hosts, and blood traps also
#' attract blood-seeking mosquitoes. Total availability in each patch is:
#' \deqn{B = W_H + W_V + W_O + W_T}
#' where \eqn{W_V} is visitor availability, \eqn{W_O} is other blood host
#' availability, and \eqn{W_T} is blood trap availability.
#'
#' @note
#' For one host species, \eqn{\left|w\right|=\left|H\right|=}`nStrata`
#' and \eqn{\Psi} has dimensions `nPatches` \eqn{\times} `nStrata`.
#' Both \eqn{W_H} and \eqn{B} have length `nPatches`.
#'
#' @seealso [human_availability], [time_spent_matrix], [time_at_risk],
#' [xds_info_search_weights_blood], [mixing_matrix]
#' @name available_blood_hosts
#' @aliases xds_info_available_blood_hosts
NULL

#' @title Other Blood Hosts
#'
#' @description
#' \describe{
#'   \item{`other_blood_hosts` or \eqn{W_O}}{the availability of other vertebrate hosts}
#'   \item{`blood_traps` or \eqn{W_T}}{the availability of traps that attact and catch blood feeding mosquitoes}
#' }
#'
#' Availability of vertebrate hosts that are not represented by an **XH**
#' module or by visitors.
#'
#' @section Other Blood Host Availability:
#' Other blood hosts attract blood-seeking mosquitoes and contribute to total
#' blood host availability \eqn{(B)}, but not to local human/host availability
#' \eqn{(W_H)}. The availability vector is initialized to zero and configured
#' by vector species using [setup_other_blood_hosts].
#'
#' @seealso [available_blood_hosts], [update_other_blood_hosts]
#' @name other_blood_hosts
#' @aliases xds_port_other_blood_hosts
NULL

#' @title The mixing matrix
#'
#' @description
#' \describe{
#'   \item{\eqn{\beta}}{the mixing matrix, also called the biting distribution matrix}
#' }
#'
#' @section Mixing Matrix:
#' The mixing matrix maps mosquito biting density in patches to per-capita
#' biting rates in human/host population strata.
#' It uses the same search weights and time at risk as [available_blood_hosts].
#'
#' For a host species, the mixing matrix is an `nStrata` \eqn{\times}
#' `nPatches` matrix:
#' \deqn{\beta =
#' \mbox{diag}(w(1-\delta)) \cdot \Psi^T \cdot \mbox{diag}(1/W_H)}
#' The product \eqn{w(1-\delta)} is element-wise, with \eqn{\delta}
#' denoting the fraction of time away for each stratum.
#' Here, \eqn{W_H} is total local host availability, summed over host species.
#' [F_beta] replaces zero entries of \eqn{W_H} with one before dividing,
#' to avoid division by zero.
#'
#' For one host species, the fraction of bites on local hosts in patch
#' \eqn{j} received by stratum \eqn{i} is \eqn{H_i \beta_{i,j}}.
#' The entries of \eqn{\beta} alone are per-capita allocation weights,
#' not fractions of bites.
#'
#' If \eqn{D} is mosquito biting density in each patch and \eqn{\ell}
#' is the local fraction, then the biting rate in each stratum is:
#' \deqn{\beta \cdot (\ell D)}
#' The product \eqn{\ell D} is element-wise.
#' Using infective biting density gives the daily local EIR.
#'
#' @seealso [available_blood_hosts], [F_beta], [F_hbr], [F_eir],
#' [xds_info_local_fraction], [xds_info_transmission]
#' @name mixing_matrix
NULL

#' @title Blood Feeding in **`ramp.xds`**
#'
#' @description
#' \describe{
#'   \item{\eqn{f}}{the blood-feeding rate per mosquito}
#'   \item{\eqn{q}}{the fraction of blood meals on the modeled hosts and visitors}
#'   \item{\eqn{W_H}}{local host availability}
#'   \item{\eqn{B}}{total availability to blood-seeking mosquitoes}
#'   \item{\eqn{\beta}}{the mixing matrix}
#' }
#'
#' @section Blood Hosts:
#' **Hosts in Patches** --- The population densities in strata are computed
#' by the **XH** component function [F_H]. Search weights and time at risk
#' determine local host availability (see [available_blood_hosts]).
#'
#' **Other Hosts and Traps** --- Local hosts compete with visitors, other
#' blood hosts, and blood traps for mosquito blood-seeking activity:
#' + \eqn{W_H} --- local host availability
#' + \eqn{W_V} --- visitor availability
#' + \eqn{W_O} --- other blood host availability
#' + \eqn{W_T} --- blood trap availability
#'
#' \deqn{B = W_H + W_V + W_O + W_T}
#'
#' **Mosquito Bionomics** --- Blood-feeding rates and other mosquito
#' bionomic parameters can be modeled as functional responses to availability.
#' For example:
#' \deqn{f = F_f(B, \ldots)}
#' For positive total availability, an availability-based host fraction is:
#' \deqn{q = \frac{W_H+W_V}{B}}
#' This is a modeling relationship, not a constraint automatically imposed
#' on every **MY** module. Directly assigned bionomic parameters need not
#' satisfy it.
#'
#' **Bites on Local Hosts** --- The local fraction separates bites on local
#' hosts from bites on visitors:
#' \deqn{\ell = \frac{W_H}{W_H+W_V}}
#' [F_local_frac] sets this fraction to one when \eqn{W_H+W_V=0}.
#' For a single vector species, the human/host biting rate and daily local EIR
#' are computed using the [mixing_matrix]:
#' \deqn{\mbox{HBR} = \beta \cdot (\ell f q M)}
#' \deqn{\mbox{EIR} = \beta \cdot (\ell f q Z)}
#' Products within the parentheses are element-wise. With multiple vector
#' species, their contributions are summed for each host stratum.
#'
#' The transmission interface also computes net infectiousness to mosquitoes.
#' Conversion of EIR to infection rates is handled separately by [Exposure].
#'
#' @seealso [BloodFeeding], [available_blood_hosts], [mixing_matrix],
#' [xds_info_mosquito_bionomics], [xds_info_transmission], [Exposure]
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @name blood_feeding
#' @aliases xds_info_blood_feeding
NULL

#' @title Blood Traps
#'
#' @description
#' Availability of traps that attract blood-seeking mosquitoes.
#'
#' @section Blood Traps:
#' The blood-feeding interface has a port for blood trap availability.
#' It contributes to total availability \eqn{B} and is initialized to zero
#' in each patch. Values are configured for each vector species through
#' [setup_blood_traps].
#'
#' @seealso [available_blood_hosts], [xds_info_mosquito_traps], [update_blood_traps]
#' @name xds_port_blood_traps
NULL
