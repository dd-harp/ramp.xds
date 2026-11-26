# X

#' @title Visitor Availability
#' 
#' @description
#' 
#' \describe{
#'   \item{`visitors` or \eqn{W_V}}{the availability of non-resident humans in each patch}
#' }
#'
#' Visitor availability contributes to total availability for searching mosquitoes,
#' alongside resident hosts, other blood hosts, and blood traps. In transmission,
#' it is used with resident-host availability to compute the local fraction.
#'
#' @seealso [setup_visitors], [get_visitors], [change_visitors],
#'   [update_visitors], [visitor_infectiousness], [available_blood_hosts],
#'   [blood_feeding], [xds_info_transmission], [xds_info_local_fraction],
#'   [malaria_importation]
#'
#' @name visitor_availability
#' @aliases xds_info_visitors
NULL

#' @title Visitor Infectiousness
#'
#' @description
#'
#' \describe{
#'   \item{`vis_kappa` or \eqn{\kappa_V}}{the net infectiousness of visitors to mosquitoes}
#' }
#'
#' Visitor infectiousness represents the probability that a mosquito becomes
#' infected after blood feeding on a visitor.
#'
#' @seealso [setup_vis_kappa], [get_vis_kappa], [change_vis_kappa],
#'   [update_vis_kappa], [visitor_availability], [malaria_importation]
#'
#' @name visitor_infectiousness
#' @aliases xds_info_vis_kappa
NULL
