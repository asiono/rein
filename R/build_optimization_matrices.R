################################################################################
#' @title         build_optimization_matrices
#' @description   This function creates the side conditions and objective function for the optimization problem.
#' 
#' @param solution_space   list of possible reinforcement measures for lines, transformator and parallel lines.
#' @param iteration_count   counter for reinforcement iteration.
#' @param lines   parameters of original grid assets.
#' @param oltc.trigger   indication for OLTC transformator usage.
#' @param allowed_voltage   limit of voltage deviation.
#' @param verbose   Value greater than zero to display step by step of reinforcement.
#' @return List of right hand, operatot and left hand side of optimization equations
#' @keywords optimization , solution space
################################################################################

#todo see if it is possible to build a funktion that prepares less if the same grid with different loads is calculated again. 

build_optimization_matrices <- function(solution_space, iteration_count, lines, oltc.trigger, allowed_voltage = 0.03, verbose = 0){
  
  if (iteration_count <= 2 ) {
    allowed_voltage <- allowed_voltage 
  } else {
    allowed_voltage <- allowed_voltage  - (iteration_count - 3)/450
  }
  
  # parameter to acitvate deactivate side conditions, see big_M Method 
  big_M_current <- 10000
  big_M_voltage <- 0.5
  
  #Objective function coeficients (c)
  cT <- solution_space$T$cost
  c1 <- solution_space$A$cost
  c2 <- solution_space$P$cost 
  
  matrices_I <- create_current_constraints(solution_space, big_M = big_M_current)
  
  matrices_U <- create_voltage_constraints(solution_space, big_M = big_M_voltage, allowed_voltage, iteration_count, oltc.trigger)
 
  matrices_one_expansion <- create_one_expansion_constraint(solution_space)
  
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