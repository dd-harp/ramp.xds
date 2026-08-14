
#' saveRDS for `xds` Objects
#'
#' @description
#' Removes forcing functions (to reduce the file size)
#' and stores the `xds` object using `saveRDS`
#'
#' @note
#' Forcing functions (*e.g.* F_trend) are stored
#' on the `xds` object in a form that
#' takes up enormous space. This removes the functions
#' before saving. 
#'
#' @param xds_obj an **`xds`** model object
#' @param filename the file name
#' 
#' @note The function `readRDS` in `ramp.trace` 
#' provides one method for saving the parameter
#' sets for trace functions, and rebuilds the 
#' functions
#' 
#' @seealso `ramp.trace`
#' 
#' @return invisible()
#' @export
saveXDS = function(xds_obj, filename){
  UseMethod("saveXDS", xds_obj$forced_by)
}

#' saveRDS for `xds` Objects
#'
#' @description
#' Removes forcing functions from the `EIR_obj` (to reduce the size)
#' and stores the `xds` object using `saveRDS`
#'
#' @inheritParams saveXDS
#' @keywords internal
#'
#' @return invisible()
#' @export
saveXDS.eir = function(xds_obj, filename){
  xds_obj$EIR_obj$F_season = list()
  xds_obj$EIR_obj$F_trend = list()
  xds_obj$EIR_obj$F_shock = list()
  saveRDS(xds_obj, file=filename)
  return(invisible())
}

#' Save `xds` Object
#'
#' @description
#' Removes forcing functions from the `L_obj` (to reduce the size)
#' and stores the `xds` object using `saveRDS`
#'
#' @inheritParams saveXDS
#' @keywords internal
#'
#' @return invisible()
#' @export
saveXDS.Lambda = function(xds_obj, filename){
  for(ix in 1:xds_obj$nVectorSpecies){
    xds_obj$L_obj[[ix]]$F_season = list()
    xds_obj$L_obj[[ix]]$F_trend = list()
    xds_obj$L_obj[[ix]]$F_shock = list()
  }
  saveRDS(xds_obj, file=filename)
  return(invisible())
}

#' saveRDS for `xds` Objects
#'
#' @description
#' Removes forcing functions from the `EIR_obj` (to reduce the size)
#' and stores the `xds` object using `saveRDS`
#'
#' @inheritParams saveXDS
#' @keywords internal
#'
#' @return invisible()
#' @export
saveXDS.XH = function(xds_obj, filename){
  for(ix in 1:xds_obj$nHostSpecies){
    xds_obj$XH_obj[[ix]]$F_season = list()
    xds_obj$XH_obj[[ix]]$F_trend = list()
    xds_obj$XH_obj[[ix]]$F_shock = list()
    xds_obj$XH_obj[[ix]]$H_trend = list()
  }
  saveRDS(xds_obj, file=filename)
  return(invisible())
}

#' saveRDS for `xds` Objects
#'
#' @description
#' Removes forcing functions from the `EIR_obj` (to reduce the size)
#' and stores the `xds` object using `saveRDS`
#'
#' @inheritParams saveXDS
#' @keywords internal
#'
#' @return invisible()
#' @export
saveXDS.MY = function(xds_obj, filename){
  for(ix in 1:xds_obj$nHostSpecies){
    xds_obj$MY_obj[[ix]]$F_season = list()
    xds_obj$MY_obj[[ix]]$F_trend = list()
    xds_obj$MY_obj[[ix]]$F_shock = list()
  }
  saveRDS(xds_obj, file=filename)
  return(invisible())
}

