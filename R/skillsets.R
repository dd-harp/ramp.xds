
#' @title Get the skill set
#' 
#' @param module "all" or "XY" or "MY" or "L"
#' @param xds_obj an **`xds`** object
#' @param ix the species index
#' 
#' @returns the skill set, a list
#' @export
get_skills = function(module, xds_obj, ix){
  class(module) = module
  UseMethod("get_skills", module)
}

#' @title Get the skill set
#'
#' @inheritParams get_skills
#'
#' @returns the skill set, a list
#' @keywords internal
#' @export
get_skills.XH = function(module, xds_obj, ix=1){
  get_skills = xds_obj$XH_obj[[ix]]$skill_set
  get_skills$Xname = xds_obj$Xname 
  return(get_skills)
}


#'@title Get the skill set
#'
#'@inheritParams get_skills 
#'
#'@returns the skill set, a list
#'@keywords internal
#'@export
get_skills.MY = function(module, xds_obj, ix=1){
  skills = xds_obj$MY_obj[[ix]]$skill_set
  skills$Lname = xds_obj$MYname 
  return(get_skills)
}

#' @title Get the skill set
#' 
#' @inheritParams get_skills
#'
#' @returns the skill set, a list
#' @keywords internal
#' @export
get_skills.L = function(module, xds_obj, ix=1){
  skills = xds_obj$L_obj[[ix]]$skill_set
  skills$Lname = xds_obj$Lname 
  return(get_skills)
}

#' Show the skill set
#'
#' @inheritParams get_skills
#'
#' @returns the skill set, a list
#' @keywords internal
#' @export
skills.all = function(module, xds_obj, ix=1){
  return(
    list(
      XH = get_skills("XH", xds_obj, ix),
      MY = get_skills("MY", xds_obj, ix),
      L = get_skills("L", xds_obj, ix))
)}

