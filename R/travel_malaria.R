#' @title Travel Malaria 
#' 
#' @description
#' Travel malaria is defined as exposure to malaria
#' acquired outside of the spatial domain represented
#' by the patches. The user can configure two ports:  
#' 
#' + time spent away / here: see [xds_port_time_away]
#' + the travel EIR: see [xds_port_travel_eir] 
#' 
#' Travel malaria is implemented in the **Exposure**
#' 
#' @seealso [xds_port_travel_eir] | [xds_port_time_away] | [xds_info_exposure] | [xds_info_malaria_importation]
#' 
#' @name xds_info_travel_malaria 
NULL

#' @title Travel EIR 
#' 
#' @description
#' Travel malaria is defined as exposure to malaria
#' acquired outside of the spatial domain represented
#' by the patches. The user can configure two ports:  
#' + time spent away / here 
#' + the travel EIR 
#'  
#' @name xds_port_travel_eir
#' 
NULL

#' @title Time Away 
#' 
#' @description
#' A variable `time_away` is used in computing 
#' [Transmission] to weight
#' `time_spent` and in [Exposure] to weight the local *vs.* 
#' travel EIR  
#'  
#' @name xds_port_time_away 
NULL

#' @title Malaria Importation 
#' 
#' @description
#' Importation of malaria in **`ramp.xds`** is defined as any
#' malaria acquired outside the spatial domain represented by
#' the patches. Imported malaria can be modeled in two ways:
#' + residents of the spatial domain acquire malaria while
#' traveling; 
#' + infected visitors  
#' 
#'  
#' @name xds_info_malaria_importation
NULL