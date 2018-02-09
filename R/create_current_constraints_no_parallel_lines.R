################################################################################
#' @title   create_current_constraints_no_parallel_lines
#' @description   this function creates the current constraints for the original grid
#' @param solution_space   dataframe containing possible cable types in the grid and its specifications
#' @param big_M   big M value for current limit optimization
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @return   This function creates the side conditions concernig the rated current for non parallel lines. 
#' The ouput is a list. That contains A, b and const.dir. 
################################################################################

create_current_constraints_no_parallel_lines <- function(solution_space, big_M,verbose = 0){
  
  grid_edges <- unique(solution_space$A[,c("begin", "end")])
  grid_edges_I = cbind(grid_edges, diag(nrow(grid_edges)))
  mapping_EA_GE <- merge(solution_space$A[,c("begin", "end", "max_I",'I_b')], 
                         grid_edges_I, sort = F)
  
  # writing the maximal currents of the lines in the matrix 
  AI_1_1 = t(mapping_EA_GE[,-c(1,2,3,4)] * solution_space$A[,"max_I"])
  edge_names <- paste(grid_edges[,'begin'], grid_edges[,'end'], sep = '_')
  rownames(AI_1_1) <- paste('I_', edge_names,sep = '')
  
  parallel_lines_places <- (grepl('Parallel_', colnames(solution_space$A))
                            & !grepl('I_b_', colnames(solution_space$A)))
  
  AI_1_T <- matrix(0, ncol = nrow(solution_space$T), nrow = nrow(AI_1_1))
  # if there are parallel lines in the grid
  if (any(parallel_lines_places)) {
    parallel_lines_places_begin_end <- (parallel_lines_places 
                                        | grepl('begin|end', colnames(solution_space$A)))
    
    parallel_opportunities <- unique(solution_space$A[,parallel_lines_places_begin_end])
    parallel_opportunities <- parallel_opportunities[,-c(1,2)]
    merged_parallel_opportunities <- merge(unique(solution_space$A$model),t(parallel_opportunities))
    merged_parallel_opportunities <- merged_parallel_opportunities[,-1]
    AI_1_2 <- matrix(big_M, nrow = ncol(merged_parallel_opportunities),ncol = nrow(merged_parallel_opportunities))

    colnames(AI_1_2) <- solution_space$P$end
    

    AI_1 <- cbind(AI_1_T,AI_1_1, AI_1_2)
  }else{
    #todo:
    warning('this options is not yet tested')
    AI_1 <- cbind(AI_1_T,AI_1_1)
  }

  
  unique_assets = which(!duplicated(mapping_EA_GE[,c('begin','end')]))
  bI_1 = Mod(mapping_EA_GE[unique_assets,"I_b"])
  
  #todo talk about adaption factor with gunther
  #bI_1 = unique(mapping_EA_GE[,'I_b'])*1.1
  
  const.dir_I_1 <- rep('>=', nrow(AI_1))

  matrices_no_parallel_lines <- list(A = AI_1, b = bI_1, const.dir = const.dir_I_1)
  
  return(matrices_no_parallel_lines)
  
}