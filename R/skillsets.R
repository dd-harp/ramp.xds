
#' @title: Get the skill set
#' 
#' @param module "all" or "XY" or "MY" or "L"
#' @param xds_obj an **`xds`** object
#' @param ix the species index
#'
#' @returns the skill set, a list
#' @export
skills = function(module, xds_obj, ix){
  class(module) = module
  UseMethod("skills", module)
}

#' Show the skill set
#'
#' @inheritParams skills
#'
#' @returns the skill set, a list
#' @export
skills.XH = function(module, xds_obj, ix=1){
  skills = xds_obj$XH_obj[[ix]]$skill_set
  skills$Xname = xds_obj$Xname 
  return(skills)
}

#' Show the skill set
#'
#' @inheritParams skills
#'
#' @returns the skill set, a list
#' @export
skills.MY = function(module, xds_obj, ix=1){
  skills = xds_obj$MY_obj[[ix]]$skill_set
  skills$Lname = xds_obj$MYname 
  return(skills)
}

#' Show the skill set
#'
#' @inheritParams skills
#'
#' @returns the skill set, a list
#' @export
skills.L = function(module, xds_obj, ix=1){
  skills = xds_obj$L_obj[[ix]]$skill_set
  skills$Lname = xds_obj$Lname 
  return(skills)
}

#' Show the skill set
#'
#' @inheritParams skills
#'
#' @returns the skill set, a list
#' @export
skills.all = function(module, xds_obj, ix=1){
  return(
    list(
      XH = skills("XH", xds_obj, ix),
      MY = skills("MY", xds_obj, ix),
      L = skills("L", xds_obj, ix))
)}

