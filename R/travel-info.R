# X

#' @title Travel Malaria 
#' 
#' @description
#'
#' Travel malaria is defined as exposure to malaria
#' acquired outside the area. The user can configure two ports:
#'
#' \describe{
#'   \item{[time_away] or \eqn{\delta}}{the fraction of time spent outside the area}
#'   \item{[travel_eir] or \eqn{E_\delta}}{the average daily EIR experienced during travel}
#' }
#' 
#' The rate of exposure to malaria for each population stratum while traveling is:
#' \deqn{\delta E_\delta}
#' 
#' The terms are integrated into the interfaces that
#' compute blood feeding, exposure, and transmission.
#' 
#' @seealso [travel_eir], [time_away], [malaria_importation],
#'   [xds_info_exposure]
#' 
#' @name travel_malaria
#' @aliases xds_info_travel_malaria
NULL

#' @title Travel EIR 
#' 
#' @description
#'
#' \describe{
#'   \item{`travel_eir` or \eqn{E_\delta}}{the average daily EIR experienced during travel by each population stratum}
#' }
#'
#' @seealso [setup_travel_eir], [get_travel_eir], [change_travel_eir],
#'   [time_away], [travel_malaria], [malaria_importation]
#' 
#'  
#' @name travel_eir
#' @aliases xds_port_travel_eir
#' 
NULL
