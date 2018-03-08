################################################################################
#' @title         create_one_expansion_constraint
#' @description This function creates conditions to ensure that only one expansion measure 
#' is chosen for every part of the grid  
#' @param solution_space   dataframe containing possible cable types in the grid and its specifications
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @return   This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and const.dir. 
################################################################################

create_one_expansion_constraint <- function(solution_space, verbose = 0) {
  
  one_measure_per_slot <- constraint_one_measure_per_slot(solution_space, 
                                                          verbose = verbose)

  one_measure_per_p_line <- constraint_one_measure_per_pline(solution_space, 
                                                             verbose = verbose)

  one_p_line_per_branch <- constraint_one_pline_per_branch(solution_space, 
                                                           verbose = verbose)

  # putting all together 
  A = rbind(one_measure_per_slot$A,one_measure_per_p_line$A,one_p_line_per_branch$A)
  b = c(one_measure_per_slot$b,one_measure_per_p_line$b,one_p_line_per_branch$b)
  const.dir <- c(one_measure_per_slot$const.dir,one_measure_per_p_line$const.dir,
                 one_p_line_per_branch$const.dir)
  
  matrices <- list( A = A, b = b, const.dir = const.dir)
  return(matrices)
}