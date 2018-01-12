################################################################################
# Description:
#' this function creates the current constrains for a grid without pararallel lines      
#'
#' @title         create_current_constrain_trafo
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{big_M}                'Parameter to activate deactive side conditions  
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions concernig the rated current for the transformers 
#' The ouput is a list. That contains A, b and const.dir. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener         wolfgang.biener(at)ise.fraunhofer.de
################################################################################

create_current_constrain_trafo <- function(solution_space, big_M, verbose = 0){
  grid_edges = unique(solution_space$T[,c("begin", "end")])
  grid_edges_I = cbind(grid_edges, diag(nrow(grid_edges)))

  row.names(grid_edges_I) <- NULL

  mapping_EA_GE <- merge(solution_space$T[,c("begin", "end", "max_I",'I_b')], grid_edges_I, sort = F)
  
  solution_space$T$max_I <- solution_space$T$max_I - big_M
  AI_T_1 <- diag(solution_space$T$max_I,nrow = length(solution_space$T$max_I))
  
  if (is.null(nrow(solution_space$P))) {
    nrow_matrix_old <- 0 + nrow(solution_space$A)
  } else {
  nrow_matrix_old <- nrow(solution_space$P) + nrow(solution_space$A) }
  
  AI_T_2 <- matrix(0, nrow = nrow(AI_T_1) , ncol = nrow_matrix_old) 
  AI_T <- cbind(AI_T_1, AI_T_2)
  bI_T   <- abs(mapping_EA_GE$I_b) - big_M

  const.dir_I_T <-  rep('>=', nrow(AI_T_1))
  
  matrices_current_trafo <- list(A = AI_T, b = bI_T,const.dir = const.dir_I_T)
  
  
  dim(AI_T)
  
  
  return(matrices_current_trafo)
}
