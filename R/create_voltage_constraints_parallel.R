################################################################################
#' @title         create_voltage_constraints_parallel
#' @description creates the constraints given by the voltage over the existing line, 
#' when a parallel line is added next to it.
#' @param solution_space  dataframe containing possible cable and transformer types in the grid and its specifications
#' @param big_M  big M value for voltage limit optimization
#' @param allowed_voltage  allowed voltage deviatipn limit
#' @param verbose  Value greater than zero to display step by step of reinforcement.
#' @return This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and c. 
################################################################################

create_voltage_constraints_parallel <- function(solution_space, big_M, allowed_voltage, verbose = 0){
  
  parallel_lines_places <- (grepl('Parallel_', colnames(solution_space$A))
                            & !grepl('I_b_', colnames(solution_space$A)))
  # parallel lines 
  A1_1 <- (t(solution_space$A[,parallel_lines_places]
               * solution_space$A[, grepl('dU_I_b', colnames(solution_space$A))]))
  
  A1_2 <- t(solution_space$P[,grepl('Parallel_', colnames(solution_space$P))]*big_M)
  
  # putting all togehter
  A <- cbind(A1_1, A1_2)
  b1 <- rep(allowed_voltage,nrow(A1_2)) + big_M
  const.dir <- rep('<=', nrow(A1_2))
  
  ## add new rows for voltage swing under zero
  A <- matrix( rep( t(A) , 2 ) , ncol = ncol(A) , byrow = TRUE )
  b2 <- rep(allowed_voltage*-1, nrow(A1_2)) - big_M
  const.dir <- c(const.dir, rep('>=', nrow(A1_2)))
  
  matrices <- list(A = A, b1 = b1, b2 = b2, const.dir = const.dir)
  
  return(matrices)
  
}