################################################################################
# Description:
#' This function creates conditions to ensure that only one expansion measure
#'  is chosen for every part of the grid          
#'
#' @title         create_one_expansion_constraint
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and const.dir. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener         wolfgang.biener(at)ise.fraunhofer.de
################################################################################


create_one_expansion_constraint <- function(solution_space,big_M, verbose = 0){
  
  # browser()
  
  one_measure_per_slot <- constraint_one_measure_per_slot(solution_space, 
                                                          verbose = verbose)

#   View(cbind(one_measure_per_slot$A,one_measure_per_slot$const.dir, one_measure_per_slot$b))
  
  one_measure_per_p_line <- constraint_one_measure_per_pline(solution_space, 
                                                             verbose = verbose)
 # View(cbind(one_measure_per_p_line$A,one_measure_per_p_line$const.dir, one_measure_per_p_line$b))
  
  one_p_line_per_branch <- constraint_one_pline_per_branch(solution_space, 
                                                           verbose = verbose)
  # View(cbind(one_p_line_per_branch$A,one_p_line_per_branch$const.dir, one_p_line_per_branch$b))
  

  # putting all together 
  
 
  dim(one_measure_per_slot$A)
  dim(one_measure_per_p_line$A)
  dim(one_p_line_per_branch$A)
  A = rbind(one_measure_per_slot$A,one_measure_per_p_line$A,one_p_line_per_branch$A)
  b = c(one_measure_per_slot$b,one_measure_per_p_line$b,one_p_line_per_branch$b)
  const.dir <- c(one_measure_per_slot$const.dir,one_measure_per_p_line$const.dir,
                 one_p_line_per_branch$const.dir)
  
  if (dim(A)[2] != length(b))
  dim(A)
  length(b)
  length(const.dir)
  
  matrices <- list( A = A, b = b, const.dir = const.dir)
  return(matrices)
}