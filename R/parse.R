
#' @title parse the output of an object returned by deSolve
#' @param y a [vector] with the variables
#' @param xds_obj a **`xds`** model object
#' @return varslist a [list]
#' @export
parse_y <- function(y, xds_obj){

  vals = list()

  s = length(xds_obj$L_obj)
  if(s>0){
    vals$L = list()
    for(ix in 1:s)
      vals$L[[ix]] = get_L_vars(y, xds_obj, ix)
  }

  s = length(xds_obj$MY_obj)
  if(s>0){
    vals$MY = list()
    for(ix in 1:s)
      vals$MY[[ix]] = get_MY_vars(y, xds_obj, ix)
  }

  i = length(xds_obj$XH_obj)
  if(i>0){
    vals$XH  = list()
    for(ix in 1:i)
      vals$XH[[ix]] = get_XH_vars(y, xds_obj, ix)
  }
  return(vals)
}

#' @title parse the outputs of an object created by xde_solve or dts_solve
#' @param xds_obj a [list]
#' @param outputs a [matrix] of _orbits returned by deSolve
#' @return varslist a [list]
#' @export
parse_orbits <- function(outputs, xds_obj){
  varslist = list()

  s = length(xds_obj$L_obj)
  if(s>0){
    varslist$L = list()
    for(ix in 1:s)
      varslist$L[[ix]] = parse_L_orbits(outputs, xds_obj, ix)
  }

  s = length(xds_obj$MY_obj)
  if(s>0){
    varslist$MY = list()
    for(ix in 1:s)
      varslist$MY[[ix]]= parse_MY_orbits(outputs, xds_obj, ix)
  }

  s = length(xds_obj$XH_obj)
  if(s>0){
    varslist$XH = list()
    for(ix in 1:s)
      varslist$XH[[ix]] = parse_XH_orbits(outputs, xds_obj, ix)
  }

  return(varslist)
}


#' @title Make Outputs
#' @param xds_obj an `xds` object
#' @param de_vars the solutions [matrix]
#' @param tm the time
#' @return an **`xds`** object
parse_outputs = function(xds_obj, de_vars, tm){
  UseMethod("parse_outputs", xds_obj$frame)
}

#' @title Make Outputs
#' @inheritParams parse_outputs
#' @return an **`xds`** object
parse_outputs.full = function(xds_obj, de_vars, tm){
  xds_obj$outputs$time <- tm
  xds_obj$outputs$last_y <- tail(de_vars, 1)
  #  xds_obj$outputs$bionomics <- get_bionomics(tm, de_vars, xds_obj)
  xds_obj$outputs$orbits <- parse_orbits(de_vars, xds_obj)
  for(i in 1:xds_obj$nHostSpecies) xds_obj = parse_XH_terms(xds_obj, i)
  for(s in 1:xds_obj$nVectorSpecies) xds_obj = parse_MY_terms(xds_obj, s)
  for(s in 1:xds_obj$nVectorSpecies) xds_obj = parse_L_terms(xds_obj, s)
  return(xds_obj)
}

#' @title Make Outputs
#' @inheritParams parse_outputs
#' @return an **`xds`** object
parse_outputs.mosy = function(xds_obj, de_vars, tm){
  xds_obj$outputs$time <- tm
  xds_obj$outputs$last_y <- tail(de_vars, 1)
  #xds_obj$outputs$bionomics <- get_bionomics(tm, de_vars, xds_obj)
  xds_obj$outputs$orbits <- parse_orbits(de_vars, xds_obj)
  for(s in 1:xds_obj$nVectorSpecies) xds_obj = parse_MY_terms(xds_obj, s)
  for(s in 1:xds_obj$nVectorSpecies) xds_obj = parse_L_terms(xds_obj, s)
  return(xds_obj)
}

#' @title Make Outputs
#' @inheritParams parse_outputs
#' @return an **`xds`** object
parse_outputs.aquatic = function(xds_obj, de_vars, tm){
  xds_obj$outputs$time <- tm
  xds_obj$outputs$last_y <- tail(de_vars, 1)
  xds_obj$outputs$orbits <- parse_orbits(de_vars, xds_obj)
  for(s in 1:xds_obj$nVectorSpecies) xds_obj = parse_L_terms(xds_obj, s)
  return(xds_obj)
}

#' @title Make Outputs
#' @inheritParams parse_outputs
#' @return an **`xds`** object
parse_outputs.human = function(xds_obj, de_vars, tm){
  xds_obj$outputs$time <- tm
  xds_obj$outputs$last_y <- tail(de_vars, 1)
  xds_obj$outputs$orbits <- parse_orbits(de_vars, xds_obj)
  for(i in 1:xds_obj$nHostSpecies) xds_obj = parse_XH_terms(xds_obj, i)
  for(s in 1:xds_obj$nVectorSpecies) xds_obj = parse_MY_terms(xds_obj, s)
  return(xds_obj)
}

#' @title Make Outputs
#' @inheritParams parse_outputs
#' @return an **`xds`** object
parse_outputs.eir = function(xds_obj, de_vars, tm){
  xds_obj$outputs$time <- tm
  xds_obj$outputs$last_y <- tail(de_vars, 1)
  xds_obj$outputs$orbits <- parse_orbits(de_vars, xds_obj)
  for(i in 1:xds_obj$nHostSpecies) xds_obj = parse_XH_terms(xds_obj, i)
  return(xds_obj)
}


#' @title Pull XH Terms 
#' 
#' @description Pull the stored values of the 
#' EIR and the FoI and compute NI
#' 
#' @param xds_obj an **`xds`** model object 
#' @param i host species index
#'
#' @return an **`xds`** model object 
#' @export
parse_XH_terms <- function(xds_obj, i=1) {
  eir = c()
  foi = c()
  ni = c()
  x  = c()
  tm <- xds_obj$outputs$deout[,1]
  for(ix in 1:length(tm)){
    y_ix <- xds_obj$outputs$deout[ix,-1]
    xds_obj <- xds_compute_terms(tm[ix], y_ix, xds_obj)
    vars = get_XH_vars(y_ix, xds_obj, i)
    eir = rbind(eir, xds_obj$terms$EIR[[i]])
    foi = rbind(foi, xds_obj$terms$FoI[[i]])
    x   = rbind(x, F_prevalence(vars, xds_obj$XH_obj[[i]]))
    ni  = rbind(ni, F_ni(vars, xds_obj$XH_obj[[i]]))
  }
  xds_obj$outputs$orbits$XH[[i]]$eir = eir
  xds_obj$outputs$orbits$XH[[i]]$foi = foi
  xds_obj$outputs$orbits$XH[[i]]$ni = ni
  xds_obj$outputs$orbits$XH[[i]]$x = x
  return(xds_obj)
}

#' @title Pull MY Terms 
#' 
#' @description Pull the stored values of the 
#' Lambda, G, fqZ, and kappa
#' 
#' @param xds_obj an **`xds`** model object 
#' @param s vector species index
#' 
#' @return an **`xds`** model object 
#' @export
parse_MY_terms <- function(xds_obj, s=1) {
  
  Lambda = c()  
  kappa = c()  
  G = c()  
  fqZ = c()  
  fqM = c()  
  
  tm <- xds_obj$outputs$deout[,1]
  for(ix in 1:length(tm)){
    y_ix <- xds_obj$outputs$deout[ix,-1] 
    xds_obj <- xds_compute_terms(tm[ix], y_ix, xds_obj) 
    vars = get_MY_vars(y_ix, xds_obj, s)
    Lambda = rbind(Lambda, xds_obj$terms$Lambda[[s]])
    kappa  = rbind(kappa, xds_obj$terms$kappa[[s]])
    G      = rbind(G, xds_obj$terms$G[[s]])
    fqZ    = rbind(fqZ, xds_obj$terms$fqZ[[s]])
    fqM    = rbind(fqM, F_fqM(tm[ix], y_ix, xds_obj, s))
  }
  xds_obj$outputs$orbits$MY[[s]]$fqZ=fqZ
  xds_obj$outputs$orbits$MY[[s]]$fqM=fqM
  xds_obj$outputs$orbits$MY[[s]]$G=G
  xds_obj$outputs$orbits$MY[[s]]$Lambda=Lambda
  xds_obj$outputs$orbits$MY[[s]]$kappa=kappa
 
  return(xds_obj) 
}

#' @title Pull bionomic parameters 
#' 
#' @description Pull the stored values of the 
#' baseline and modified values of parameters. 
#' Note that the total effect size of control 
#' is the ratio. 
#' 
#' @param xds_obj an **`xds`** model object 
#' @param s vector species index
#' 
#' @return an **`xds`** model object 
#' @export
parse_bionomics <- function(xds_obj, s=1) {
  ft = c()  
  f = c()  
  qt = c()  
  q = c()  
  gt = c()  
  g = c()  
  tm <- xds_obj$outputs$deout[,1]
  for(ix in 1:length(tm)){
    y_ix <- xds_obj$outputs$deout[ix,-1] 
    xds_obj <- xds_compute_terms(tm[ix], y_ix, xds_obj)
    ft     = rbind(ft, xds_obj$MY_obj[[s]]$ft)
    f      = rbind(f, xds_obj$MY_obj[[s]]$f)
    qt     = rbind(qt, xds_obj$MY_obj[[s]]$qt)
    q      = rbind(q, xds_obj$MY_obj[[s]]$q)
    gt     = rbind(gt, xds_obj$MY_obj[[s]]$gt)
    g      = rbind(g, xds_obj$MY_obj[[s]]$g)
  }
  xds_obj$outputs$orbits$MY[[s]]$ft=ft
  xds_obj$outputs$orbits$MY[[s]]$f=f
  xds_obj$outputs$orbits$MY[[s]]$es_f=ft/f
  xds_obj$outputs$orbits$MY[[s]]$qt=qt
  xds_obj$outputs$orbits$MY[[s]]$q=q
  xds_obj$outputs$orbits$MY[[s]]$es_q=qt/q
  xds_obj$outputs$orbits$MY[[s]]$gt=gt
  xds_obj$outputs$orbits$MY[[s]]$g=g
  xds_obj$outputs$orbits$MY[[s]]$es_g=gt/g
  return(xds_obj)
}

#' @title Pull L Terms 
#' 
#' @description Pull the stored values of the 
#' alpha, eta 
#' 
#' @param xds_obj an **`xds`** model object 
#' @param s vector species index 
#' 
#' @return an **`xds`** model object 
#' @export
parse_L_terms <- function(xds_obj, s=1) {
  alpha = c() 
  eta = c() 
  tm <- xds_obj$outputs$deout[,1]
  for(ix in 1:length(tm)){
    y_ix <- xds_obj$outputs$deout[ix,-1] 
    xds_obj <- xds_compute_terms(tm[ix], y_ix, xds_obj) 
    vars = get_L_vars(y_ix, xds_obj, s)
    alpha = rbind(alpha, xds_obj$terms$alpha[[s]])
    eta   = rbind(eta, xds_obj$terms$eta[[s]])
  }
  xds_obj$outputs$orbits$L[[s]] = list(alpha=alpha, eta=eta)
  return(xds_obj)
}