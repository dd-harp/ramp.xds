
#' @title Set up XH ports 
#'
#' @description
#' Adds port objects and default values for the time spent matrix, 
#' time away, and blood feeding search weights.  
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_XH_ports = function(xds_obj, i){
  nStrata = xds_obj$nStrata[i] 
  
  # The time spent matrix and time away are managed by timespent_obj 
  xds_obj$XH_obj[[i]]$timespent <- get_residence_matrix(xds_obj, i) 
  xds_obj$XH_obj[[i]]$time_away <- rep(0, nStrata)
  timespent_obj <- list()
  class(timespent_obj) <- "static"
  xds_obj$XH_obj[[i]]$timespent_obj <- timespent_obj
  
  # Blood feeding search weights: one vector per vector species, dispatched by search_obj
  xds_obj$XH_obj[[i]]$search_weights <- list()
  xds_obj$XH_obj[[i]]$search_weights[[1]] <- rep(1, nStrata)
  
  search_obj <- list()
  class(search_obj) <- "static"
  xds_obj$XH_obj[[i]]$search_obj <- search_obj
  
  return(xds_obj)
}