################################################################################
# Description:
#' This function calculates active power infeed by priotized reactive power        
#'
#' @title         build_solution_space
#' 
#                   name         type                   description  
#' @param  \strong{s_max}       'maximal apparent power
#' @param  \strong{q_val}      'reactive power infeed 
#' @param  \strong{p_act}      'active power infeed 
#'                             consider.  trafo and line are possible
#' @param  \strong{verbose}    'verbosity level 

#' @return \strong{p_val}      'active power output
#'@keywords reactive power, statcom , Q(U)
#'@author        Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################


calc_p_infeed <- function(s_max, q_val, p_act){
  
  # calculating potential p 
  p_potential <- sqrt(Mod(s_max^2 - q_val^2))
  p_sign <- sign(p_act)
  p_abs <- abs(p_act)
  p_val <- p_abs
  if (any(p_abs > p_potential)) {
    p_val[p_abs > p_potential] <- p_potential[p_abs > p_potential]}   

  p_val <- p_val * p_sign
 
  return(p_val) 
}