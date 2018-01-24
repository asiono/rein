################################################################################
# Description:
#' this function creates input matrices and vectors for the optimizer       
#'
#' @title         build_optimization_matrices
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{lines}                'lines data of the grid 
#' @param  \strong{allowed_deviation}    'uncertainity of voltage calculation that is taken as security margin in pu
#' @param  \strong{allowed_voltage}      'allowed voltage rise in pu
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and c. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener/Gunther Gust         wolfgang.biener(at)ise.fraunhofer.de
################################################################################

#todo see if it is possible to build a funktion that prepares less if the same grid with different loads is calculated again. 
build_optimization_matrices <- function(solution_space, iteration_count, lines, oltc.trigger, allowed_voltage = 0.03, verbose = 0){
  source('R/create_voltage_constraints_update.R')
  source('R/create_current_constraints.R')
  
  if (iteration_count <= 3 ) {
    allowed_voltage <- allowed_voltage 
  } else {
    allowed_voltage <- allowed_voltage  - (iteration_count - 3)/650
  }
  
  # parameter to acitvate deactivate side conditions, see big_M Method 
  big_M_current <- 10000
  big_M_voltage <- 0.5
  
  library(plyr)
  #Objective function coeficients (c)
  cT <- solution_space$T$cost
  c1 <- solution_space$A$cost
  c2 <- solution_space$P$cost 
  
  matrices_I <- create_current_constraints(solution_space, big_M = big_M_current)
  matrices_U <- create_voltage_constraints(solution_space, big_M = big_M_voltage, allowed_voltage, iteration_count, oltc.trigger)
 
  matrices_one_expansion <- create_one_expansion_constraint(solution_space,
                                                            big_M = big_M_voltage)
  # putting all together 
  A <- rbind(matrices_I$A/1000,matrices_U$A,matrices_one_expansion$A)
  b <- c(matrices_I$b/1000,matrices_U$b,matrices_one_expansion$b)
  
  if (length(b) != dim(A)[[1]]) {
    message('Dim A: ',dim(matrices_I$A)[[1]], '  length(b): ',
            length(matrices_I$b))
  }
  
  const.dir <- c(matrices_I$const.dir,matrices_U$const.dir,matrices_one_expansion$const.dir)  
  #View(cbind(A, const.dir, b))
  
#debug no I 
#   A <- rbind(matrices_U$A,matrices_one_expansion$A)
#   b <- c(matrices_U$b,matrices_one_expansion$b)
#   const.dir <- c(matrices_U$const.dir,matrices_one_expansion$const.dir)  
#   
# 
#   debug no U
#   A <- rbind(matrices_I$A,matrices_one_expansion$A)
#   b <- c(matrices_I$b,matrices_one_expansion$b)
#   const.dir <- c(matrices_I$const.dir,matrices_one_expansion$const.dir)  
  
#   
  # no one constraint restriction 
  #geht
#   A <- rbind(matrices_I$A,matrices_U$A)
#   b <- c(matrices_I$b,matrices_U$b)
#   const.dir <- c(matrices_I$const.dir,matrices_U$const.dir)      
#   
#   # I only
  #geht
#   A <- rbind(matrices_I$A)
#   b <- c(matrices_I$b)
#   const.dir <- c(matrices_I$const.dir)     
  
  cost <- c(cT, c1, c2)

  matrices <- list(A = A, b = b, cost = cost, const.dir = const.dir)

  return(matrices)
  
}