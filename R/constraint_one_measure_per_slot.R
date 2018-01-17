################################################################################
# Description:
#' This function creates conditions to ensure that only one expansion measure
#'  per slot in the original grid is chosen            
#'
#' @title         constraint_one_measure_per_slot
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

constraint_one_measure_per_slot <- function(solution_space, verbose = 0){
  # Trafos 
  n_A_col_wo_trafo <- nrow(solution_space$A) + nrow(solution_space$P)
  AT <- c(rep(1, nrow(solution_space$T)), rep(0, n_A_col_wo_trafo))
  const.dir_T <- rep('=', 1)
  b_T          <- 1 
  
  # old grid lines   
  #Mapping of expansion alternatives to grid edges
  grid_edges = unique(solution_space$A[,c("begin", "end")])
  grid_edges_I = cbind(grid_edges, diag(1, nrow(grid_edges), nrow(grid_edges)))
  row.names(grid_edges_I) <- NULL
  mapping_EA_GE = merge(solution_space$A[,c("begin", "end", "I_b")], grid_edges_I, sort = F)
  
  A1_1 <- t(mapping_EA_GE[,-c(1,2,3)])
  A1_2 <- matrix(0, nrow = nrow(A1_1), ncol = nrow(solution_space$P))
  A1_T <- matrix(0,nrow = nrow(A1_1), ncol = nrow(solution_space$T))
  A1 <- cbind(A1_T,A1_1, A1_2) 
  const.dir_1 <- rep('=', nrow(A1))
  b_1          <- rep(1, nrow(A1)) 
  
  A <- rbind(AT, A1)
  rownames(A)[] <- 'one_measure_per_slot'
  
  b <- c(b_T, b_1)
  const.dir <- c(const.dir_T, const.dir_1)
  
  matrices <- list( A = A, b = b, const.dir = const.dir)
  return(matrices)
}