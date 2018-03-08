################################################################################
#' @title         constraint_one_pline_per_branch
#' @description   This function creates conditions to ensure that only one parallel lines 
#' is chosen per branch of the grid 
#' @param solution_space   dataframe containing possible cable types in the grid and its specifications
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @return   This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and const.dir. 
#################################################################################

constraint_one_pline_per_branch <- function(solution_space, verbose = 0){
  
  branches <- solution_space$P[,grep('Branch', colnames(solution_space$P))]
  A3_2 <- t(branches)
  A3_1 <- matrix(0,nrow = nrow(A3_2), ncol = nrow(solution_space$A))
  A3_T <- matrix(0,nrow = nrow(A3_2), ncol = nrow(solution_space$T))
  A3   <- cbind(A3_T,A3_1, A3_2)
  
  rownames(A3)[] <- 'one_pline_per_branch'
  const.dir_3 <- rep('<=', nrow(A3))
  b_3          <- rep(1, nrow(A3)) 

  matrices <- list(A = A3, b = b_3, const.dir = const.dir_3)

  return(matrices)
}
  