
#' @title Plot adult mosquito population density
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_M = function(xds_obj, s=1, clrs="darkblue", llty=1, add=FALSE){
  MY = get_MY_out(xds_obj, s) 
  
  if(add == FALSE) 
    with(MY, plot(time, 0*time, type = "n", ylim = range(0,M),
                  ylab = "Mosquito Density", xlab = "Time"))
  
  xds_lines_M(MY, xds_obj, clrs, llty)
}

#' Add lines for adult mosquito population density
#'
#' @param MY xds_objed ouptuts
#' @param xds_obj an **`xds`** model object
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_M = function(MY, xds_obj, clrs="darkblue", llty=1){
  with(MY,{
    if(xds_obj$nPatches==1) lines(time, M, col=clrs[1], lty = llty[1])
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)
      for(i in 1:xds_obj$nPatches){
        lines(time, M[,i], col=clrs[i], lty = llty[i])
        
      }
    }
  })}

#' Plot the density of infected and infective mosquitoes
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a vector of colors for infected mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Y = function(xds_obj, s=1, clrs = "purple", llty=1, add=FALSE){
  MY = get_MY_out(xds_obj, s) 
  
  if(add == FALSE)
    with(MY, plot(time, 0*time, type = "n", ylim = range(0,Y),
              ylab = "Mosquito Density", xlab = "time"))
  
  xds_lines_Y(MY, xds_obj, clrs, llty)
}

#' Add lines for the density of infected and infective mosquitoes
#'
#' @param MY xds_objed ouptuts
#' @param xds_obj an **`xds`** model object
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Y = function(MY, xds_obj, clrs="purple", llty=1){
  with(MY,{
    if(xds_obj$nPatches==1){
      lines(time, Y, col=clrs[1], lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)
      
      for(i in 1:xds_obj$nPatches){
        lines(time, Y[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

#' Plot the density of infected and infective mosquitoes
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a vector of colors for infective mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Z = function(xds_obj, s=1,  clrs="darkred", llty=1, add=FALSE){
  MY = get_MY_out(xds_obj, s) 
  
  if(add == FALSE)
    with(MY,
         plot(time, 0*time, type = "n", ylim = range(0,Z),
              ylab = "Mosquito Density", xlab = "time"))
  
  xds_lines_Z(MY, xds_obj, clrs, llty)
}

#' Add lines for the density of infected and infective mosquitoes
#'
#' @param MY xds_objed ouptuts
#' @param xds_obj an **`xds`** model object
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Z = function(MY, xds_obj, clrs = "darkred", llty=1){
  with(MY,{
    if(xds_obj$nPatches==1){
      lines(time, Z, col=clrs[1], lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)
      
      for(i in 1:xds_obj$nPatches){
        lines(time, Z[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

#' Plot the fraction of infected and infective mosquitoes
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a vector of colors for infected mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Y_fracs = function(xds_obj, s=1, clrs = "purple", llty=1,  add=FALSE){
  MY = get_MY_out(xds_obj, s) 
  
  if(add == FALSE)
    with(MY, plot(time, 0*time, type = "n", ylim = range(0,y),
                  ylab = "Fraction Infected", xlab = "time"))
  
  xds_lines_Y_fracs(MY, xds_obj, clrs, llty)
}

#' Add lines for the fraction of infected and infective mosquitoes
#'
#' @param MY xds_objed ouptuts
#' @param xds_obj an **`xds`** model object
#' @param clrs a vector of colors for infected mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Y_fracs = function(MY, xds_obj, clrs="purple", llty=1){
  with(MY,{
    if(xds_obj$nPatches==1) {
      lines(time, y, col=clrs, lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)
      
      for(i in 1:xds_obj$nPatches){
        lines(time, y[,i], col=clrs[i], lty = llty[i])
      }
    }
  })
}

#' Plot the fraction infective
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param clrs a vector of colors for infective mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add do not plot axes if TRUE
#'
#' @export
xds_plot_Z_fracs = function(xds_obj, s=1, clrs = "darkred", llty=1, add=FALSE){
  MY = get_MY_out(xds_obj, s) 
  
  if(add == FALSE)
    with(MY,
         plot(time, 0*time, type = "n", ylim = range(0,z),
              ylab = "Fraction Infected", xlab = "time"))
  
  xds_lines_Z_fracs(MY, xds_obj, clrs, llty)
}

#' Add lines for the fraction of infected and infective mosquitoes
#'
#' @param MY xds_objed outputs
#' @param xds_obj an **`xds`** model object
#' @param clrs a vector of colors for infective mosquitoes
#' @param llty an integer (or integers) to set the `lty` for plotting
#'
#' @export
xds_lines_Z_fracs = function(MY, xds_obj, clrs="darkred", llty=1){
  with(MY,{
    if(xds_obj$nPatches==1) {
      lines(time, z, col=clrs, lty = llty[1])
    }
    if(xds_obj$nPatches>1){
      if (length(clrs)==1) clrs=rep(clrs, xds_obj$nPatches)
      if (length(llty)==1) llty=rep(llty, xds_obj$nPatches)
      
      for(i in 1:xds_obj$nPatches){
        lines(time, z[,i], col=clrs[i], lty = llty[i])
      }
    }
  })}

