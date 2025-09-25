
#' Shrink an xds xds_obj object
#'
#' @param xds_obj an **`xds`** model object
#'
#' @returns invisible() 
#' @export
xds_shrink = function(xds_obj){
  UseMethod("xds_shrink", xds_obj$frame)
}


