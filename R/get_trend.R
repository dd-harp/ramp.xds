
#' @title Get the trend parameters 
#' 
#' @description
#' Get the interpolating points 
#' for a spline function.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return trend parameters 
#'   
#' @export
get_trend = function(xds_obj, s=1){
  UseMethod("get_trend", xds_obj$forced_by) 
}

#' @title Get the trend parameters 
#' 
#' @description
#' Get the interpolating points 
#' for a spline function.
#' 
#' @inheritParams get_trend
#' 
#' @return trend parameters 
#' 
#' @export
get_trend.none = function(xds_obj, s=1){
  return(c())
}

#' @title Get the trend parameters 
#' 
#' @description
#' Get the interpolating points 
#' for a spline function.
#' 
#' @inheritParams get_trend
#' 
#' @return trend parameters 
#' 
#' @export
get_trend.Lambda= function(xds_obj, s=1){
  return(xds_obj$Lpar[[s]]$trend_par)
}

#' @title Get the trend parameters 
#' 
#' @description
#' Get the interpolating points 
#' for a spline function.
#' 
#' @inheritParams get_trend
#' 
#' @return trend parameters 
#' 
#' @export
get_trend.eir= function(xds_obj, s=1){
  return(xds_obj$EIRpar$trend_par)
}

#' @title Get spline interpolation points 
#' 
#' @description
#' Get the interpolating points 
#' for a spline function.
#' 
#' @param xds_obj an **`xds`** model object
#' 
#' @export
get_spline = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1) {
    get_spline_s(xds_obj, 1)
  } else {
    spline <- list() 
    for(i in 1:length(xds_obj$nVectorSpecies)) 
      spline[[i]] <- get_spline_s(xds_obj, i) 
    return(spline)
  }
}

#' @title Get spline interpolation points 
#' 
#' @description
#' Get the interpolating points 
#' for a spline function.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @export
get_spline_s = function(xds_obj, s){
  UseMethod("get_spline_s", xds_obj$forced_by) 
}

#' @title Get spline interpolation points 
#' 
#' @description
#' Return the value that sets mean forcing
#' for forced models. 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @export
get_spline_s.none = function(xds_obj, s){
  return(c()) 
}


#' @title Get spline interpolation points 
#' 
#' @description
#' Return the 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return the interpolation points, as a list
#' 
#' @export
get_spline_s.Lambda = function(xds_obj, s=1){
  tt <- xds_obj$Lpar[[s]]$trend_par$tt  
  yy <- xds_obj$Lpar[[s]]$trend_par$yy  
  return(list(tt=tt, yy=yy)) 
}

#' @title Get spline interpolation points 
#' 
#' @description
#' Return the 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @export
get_spline_s.eir = function(xds_obj, s=1){
  tt <- xds_obj$EIRpar$trend_par$tt
  yy <- xds_obj$EIRpar$trend_par$yy
  return(list(tt=tt, yy=yy))
}
