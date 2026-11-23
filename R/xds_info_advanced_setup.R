# Y 

#' @title Advanced setup for **`ramp.xds`**
#'
#' @description
#' 
#' Advanced setup options for progressive elaboration after basic setup (see [xds_basic_setup])
#' 
#' 
#' @section Spatial Dynamics: 
#' 
#' **Mosquito Dispersal** 
#' + Dispersal Kernel --- [setup_K_matrix]
#' + Patch Emigration --- [setup_mosy_emigration] | [setup_emigration_loss]
#' + **Blood** ---  [setup_blood_search_weights] | [setup_other_blood_hosts] | [setup_blood_traps] 
#' + **Habitats** ---  [setup_habitat_search_weights] | [setup_bad_habitats] | [setup_ovitraps]
#' + **Sugar** --- [ramp.forcing::setup_sugar]
#' 
#' **Human Mobility**
#' + [setup_timespent] 
#' + [setup_F_circadian]
#' 
#' @section Human Demography:
#' + [setup_births] | [setup_deaths]
#'  
#' @section Blood Feeding, Exposure, and Transmission: 
#' 
#' + [setup_blood_search_weights]
#' + [setup_blood_feeding_rates] | [setup_human_fraction]
#' + Resources: [setup_other_blood_hosts] | [setup_blood_traps] 
#' 
#' @section Exposure:
#' + [setup_exposure]
#' + 
#' 
#' @section Malaria Importation:
#' + [setup_time_away] | [setup_travel_eir]
#' + [setup_visitors] | [setup_vis_kappa]
#' 
#' @section Mosquito Bionomics: 
#' 
#' + \eqn{f} --- [setup_f] | [setup_blood_feeding_rates]
#' + \eqn{q} --- [setup_q] | [setup_human_fraction]
#' + \eqn{g} --- [setup_g] | [setup_mosquito_mortality]
#' + \eqn{\sigma} --- [setup_sigma] | [setup_mosy_emigration]
#' + \eqn{\mu} --- [setup_mu] | [setup_emigration_loss]
#' + \eqn{\nu} --- [setup_nu] | [setup_egg_batch_rates]
#' + \eqn{\tau} --- [setup_eip]
#' 
#' @section Weather:
#' 
#' 
#' + [ramp.forcing::setup_temperature] 
#' + 
#' 
#' @section Resources:
#' 
#' [xds_info_resources]
#' 
#' @section Habitats and Egg Laying: 
#' 
#' + [xds_info_aquatic_habitats]
#' + [xds_port_ovitraps]
#' 
#' @name xds_advanced_setup
#' @aliases xds_info_advanced_setup
NULL