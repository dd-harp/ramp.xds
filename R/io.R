
#' saveRDS for `xds` Objects 
#'
#' @description
#' Removes forcing functions (to reduce the file size) 
#' and stores the `xds` object using `saveRDS`
#'  
#' @seealso [readXDS()]
#' @note
#' Forcing functions (*e.g.* F_trend) are stored 
#' on the `xds` object in a form that 
#' takes up enormous space. 
#' 
#' Since they are created
#' by calling [make_function()] from the 
#' stored *function objects* 
#' (*e.g.* `trend_par`), 
#' they are redundant.    
#'  
#' @param xds_obj an **`xds`** model object
#' @param filename the file name 
#'
#' @returns invisible() 
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

#' @returns invisible() 
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
#' @returns invisible() 
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
#' @returns invisible() 
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
#' @returns invisible() 
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


#' readRDS for `xds` Objects 
#' 
#' @description
#' Read the `xds` object using `readRDS` and rebuild the forcing functions
#' 
#' @param filename the file name 
#'
#' @returns an **`xds`** model object
#' @export
readXDS = function(filename){
  xds_obj <- readRDS(filename)
  for(s in 1:xds_obj$nVectorSpecies)
    xds_obj <- rebuild_forcing_functions(xds_obj, s) 
  return(xds_obj)
}
