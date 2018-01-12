################################################################################
# Description:
#' This function creates conditions to ensure that only one expansion measure
#'  is chosen for every parallel line           
#'
#' @title         constraint_one_measure_per_pline
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
#'@keywords optimization , solution space, constraints 
#'@author        Wolfgang Biener         wolfgang.biener(at)ise.fraunhofer.de
################################################################################

constraint_one_measure_per_pline <- function(solution_space, verbose = 0){
  
  grid_edges = unique(solution_space$P[,c("begin", "end")])
  grid_edges_I = cbind(grid_edges, diag(nrow(grid_edges)))
  row.names(grid_edges_I) <- NULL
  mapping_EA_GE = merge(solution_space$P[,c("begin", "end", "I_b")], grid_edges_I, sort = F)
  
  A2_2 <- t(mapping_EA_GE[,-c(1,2,3)])
  A2_1 <- matrix(0, nrow = nrow(A2_2), ncol = nrow(solution_space$A))

  A2_T <- matrix(0,nrow = nrow(A2_2), ncol = nrow(solution_space$T))
  A2 <- cbind(A2_T,A2_1, A2_2) 
  rownames(A2)[] <- 'one_measure_per_pline'
  
  const.dir_2 <- rep('<=', nrow(A2))
  b_2          <- rep(1, nrow(A2)) 
  
  matrices <- list( A = A2, b = b_2, const.dir = const.dir_2)
  return(matrices)
}