################################################################################
# Description:
#' this function creates the current constrains for a grid with pararallel lines      
#'
#' @title         create_current_constrains_parallel_lines
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{big_M}                'Parameter to activate deactive side conditions  
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions concernig the rated current for non parallel lines. 
#' The ouput is a list. That contains A, b and const.dir. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener/Gunther Gust         wolfgang.biener(at)ise.fraunhofer.de
################################################################################

create_current_constrains_parallel_lines <- function(solution_space, 
                                                        big_M,verbose = 0){
  #Mapping of expansion alternatives to grid edges
  grid_edges <- unique(solution_space$A[,c("begin", "end")])
  
  grid_edges_I = cbind(grid_edges, diag(nrow(grid_edges)))
  
  
  mapping_EA_GE <- merge(solution_space$A[,c("begin", "end", "max_I",'I_b')],
                         grid_edges_I, sort = F)
  
  #raising the maximal current to deactivaite the side condition 
  solution_space$A[,"max_I"] <- solution_space$A[,"max_I"] + big_M
  
  # writing the maximal currents of the lines in the matrix 
  AI_2_1 = t(mapping_EA_GE[,-c(1,2,3,4)] * solution_space$A[,"max_I"])
  
  # rbinding the matrix to for each possible parallel line 
  Nr_parallel_lines <- length(grep('Parallel_', colnames(solution_space$P)))
  AI_2_1_temp <- AI_2_1
  #todo faster by direct matrix creation

  for (i in 1:(Nr_parallel_lines - 1)) {
    AI_2_1 <- rbind(AI_2_1, AI_2_1_temp)
  }
 
  # creating the the right part to activate the side condition if the concerning parallel line is chosen  
  Nr_line_types <- length(unique(solution_space$P$model))
  AI_2_2 <- matrix(0, nrow = nrow(AI_2_1), ncol = length(solution_space$P$ID)) 
  i = 1

  for (i in 1:Nr_parallel_lines) {
    begin <-  (i - 1)*(nrow(AI_2_1_temp)) + 1
    end   <-  i*nrow(AI_2_1_temp)
    begin_col <- (i - 1)*(Nr_line_types) + 1
    end_col <- i*Nr_line_types
    AI_2_2[begin:end, begin_col:end_col] <- -big_M    
  }
  
  # combining left and right side of the matrix 
  AI_2_T <- matrix(0, ncol = nrow(solution_space$T), nrow = nrow(AI_2_1))
  AI_2 <- cbind(AI_2_T,AI_2_1, AI_2_2)
  
  # concatenating the flowing currents in dependence of the chosen parallel line
  parallel_current_place <- (grepl('Parallel_', colnames(solution_space$A))
                             & grepl('I_b_', colnames(solution_space$A))
                             & !grepl('dU', colnames(solution_space$A)))
  
  parallel_current_place <- which(parallel_current_place)
  length(parallel_current_place)
  
  bI_2 <- c()
  
  for (i in parallel_current_place) {
    bI_2 <- c(bI_2, solution_space$A[seq(1, nrow(solution_space$A), Nr_line_types),i]) 
  }

  # deactivating the side condition b< big_M
  bI_2 <- abs(bI_2)
  const.dir_I_2 <- rep('>=', nrow(AI_2))

  # adding the side conditions for the newly added parallel line 
  AI_3_2 <- diag(solution_space$P$max_I)
  
  # the side conditions need to be deactivated by default 
  AI_3_2[which(AI_3_2 > 0)] <- AI_3_2[which(AI_3_2 > 0)] - big_M
  length(solution_space$A[,i])
  length(unique(solution_space$A[,i]))  
  AI_3_1  <- matrix(0, nrow = nrow(AI_3_2), ncol = ncol(AI_2_1))
  AI_3_T <- matrix(0, ncol = nrow(solution_space$T), nrow = nrow(AI_3_1))
  AI_3 <- cbind(AI_3_T,AI_3_1, AI_3_2)

  # as well needs the right hand side to be deactivated 
  bI_3 <- abs(solution_space$P$I_b) - big_M  
  const.dir_I_3 <- rep('>=', nrow(AI_3))

  # putting al togehter
  AI <- rbind(AI_2, AI_3)
  bI <- c(bI_2,bI_3)
  const.dir <- c(const.dir_I_2, const.dir_I_3)

   if (dim(AI)[[1]] != length(bI) | length(bI) !=  length(const.dir)) {
        warning('There is a problem with the size of matrices')
   }
  
  matrices_I_parallel <- list(A = AI, b = bI, const.dir = const.dir)     

  return(matrices_I_parallel)
}
