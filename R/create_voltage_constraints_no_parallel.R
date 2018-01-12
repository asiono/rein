################################################################################
# Description:
#' this function creates the constraints given by the voltage of the original grid without parallel lines        
#'
#' @title         create_voltage_constraints_no_parallel
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{lines}                'lines data of the grid 
#' @param  \strong{allowed_deviation}    'uncertainity of voltage calculation that is taken as security margin 
#' @param  \strong{allowed_voltage}      'allowed voltage rise 
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and c. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener/Gunther Gust         wolfgang.biener(at)ise.fraunhofer.de
################################################################################


create_voltage_constraints_no_parallel <- function(solution_space, big_M, allowed_voltage, verbose = 0){
  ### normal lines
  A1_1 <- t(solution_space$A[,grep('Path', colnames(solution_space$A))]*solution_space$A$dU)
  # by this the equations are deactivated when the corresponding parallel line is added  
  A1_2 <- t(solution_space$P[,grep('Path', colnames(solution_space$P))]*big_M*(-1))
  
  #### putting it together
  A1 <- cbind(A1_1,A1_2)
  b1 <- rep(allowed_voltage, nrow(A1))   
  const.dir <- rep('<=', nrow(A1))
  
  ## add new rows for voltage swing under zero
  A <- matrix( rep( t(A1) , 2 ) , ncol = ncol(A1) , byrow = TRUE )
  b2 <- rep(allowed_voltage*-1, nrow(A1))
  const.dir <- c(const.dir, rep('>=', nrow(A1)))

  matrices <- list(A = A, b1 = b1, b2 = b2, const.dir = const.dir)
  
  return(matrices)
  
}