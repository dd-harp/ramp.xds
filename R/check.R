#' @title Check the model components
#' @description Run consistency checks before
#' solving or analyzing a model.  
#' @note This dispatches on `xds_obj$frame`
#' @param xds_obj an **`xds`** model object
#' @param t a vector of times
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_models <- function(xds_obj, t=0){
  UseMethod("check_models", xds_obj$frame)
}

#' @inherit check_models title description params return
#' @keywords internal
#' @export
check_models.full <- function(xds_obj, t=0){
  for(i in 1:xds_obj$nHostSpecies)
    xds_obj <- check_XH(xds_obj, t, i)
  
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- check_MY(xds_obj, t, s)
    xds_obj <- check_L(xds_obj, t, s)
  }
  xds_obj <- check_blood_feeding(xds_obj, t)
  
  return(xds_obj)
}

#' @inherit check_models title description params return
#' @keywords internal
#' @export
check_models.mosy <- function(xds_obj, t=0){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- check_MY(xds_obj, t, s)
    xds_obj <- check_L(xds_obj, t, s)
  }
  xds_obj <- check_blood_feeding(xds_obj, t)
  return(xds_obj)
}

#' @inherit check_models title description params return
#' @keywords internal
#' @export
check_models.aquatic <- function(xds_obj, t=0){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- check_L(xds_obj, t, s)
    xds_obj <- check_MY(xds_obj, t, s)
  }
  return(xds_obj)
}

#' @inherit check_models title description params return
#' @keywords internal
#' @export
check_models.human <- function(xds_obj, t=0){
  for(i in 1:xds_obj$nHostSpecies)
    xds_obj <- check_XH(xds_obj, t, i)
  for(s in 1:xds_obj$nVectorSpecies)
    xds_obj <- check_MY(xds_obj, t, s)
  xds_obj <- check_blood_feeding(xds_obj, t)
  return(xds_obj)
}

#' @inherit check_models title description params return
#' @keywords internal
#' @export
check_models.eir <- function(xds_obj, t=0){
    xds_obj <- check_XH(xds_obj, t, 1)
  return(xds_obj)
}
