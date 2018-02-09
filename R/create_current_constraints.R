################################################################################
#' @title         create_current_constrains
#' @description   this function creates the constraints given by the current
#' @param solution_space   dataframe containing possible cable and transformer types in the grid and its specifications
#' @param big_M   big M value for current limit optimization
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @return   This function creates the side conditions concernig the rated current for non parallel lines. 
#' The ouput is a list. That contains A, b and const.dir. 
################################################################################

create_current_constraints <- function(solution_space,big_M, verbose = 0){

  matrices_no_parallel_lines <- create_current_constraints_no_parallel_lines(solution_space, big_M = big_M)

  dim(matrices_no_parallel_lines$A)[[1]] - length(matrices_no_parallel_lines$b)
  
  #View(cbind(matrices_no_parallel_lines$A, matrices_no_parallel_lines$b))
  matrices_parallel_lines <- create_current_constrains_parallel_lines(solution_space, big_M = big_M)

  # View(cbind(matrices_parallel_lines$A, matrices_parallel_lines$b))  
  matrices_transformer <- create_current_constrain_trafo(solution_space, big_M = big_M)
  #View(cbind(matrices_transformer$A, matrices_transformer$b))  
  AI <- rbind(matrices_no_parallel_lines$A,matrices_parallel_lines$A,matrices_transformer$A)
  bI <- c(matrices_no_parallel_lines$b,matrices_parallel_lines$b,matrices_transformer$b)
  const.dir_I <- c(matrices_no_parallel_lines$const.dir,
                   matrices_parallel_lines$const.dir,
                   matrices_transformer$const.dir)

  if (verbose > 3) {
    print('AI, const.dir_I, bI')
    print(cbind(AI, const.dir_I, bI))
  }

  matrices_I <- list(A = AI, b = bI, const.dir = const.dir_I)
  return(matrices_I)
  
}
