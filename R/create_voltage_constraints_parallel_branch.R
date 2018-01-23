################################################################################
# Description:
#' this function creates the constraints given by the voltage over the newly added parallel lines         
#'
#' @title         create_voltage_constraints_parallel_branch
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{lines}                'lines data of the grid 
#' @param  \strong{allowed_deviation}    'uncertainity of voltage calculation that is taken as security margin 
#' @param  \strong{allowed_voltage}      'allowed voltage rise 
#' @param  \strong{verbose}    'verbosity level 

# #@details 
#' 
#' @return 
#' This function creates the side conditions and objective function for the optimization problem. 
#' The ouput is a list. That contains A, b and c. 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener/Gunther Gust         wolfgang.biener(at)ise.fraunhofer.de
################################################################################


create_voltage_constraints_parallel_branch <- function(solution_space, big_M, allowed_voltage, verbose = 0){
  #todo this function won't work without parallel_lines or with a single parallel line 
  #problems will occur when a dataframe is expected but only a vector is available

  parallel_lines_places <- (grepl('Parallel_', colnames(solution_space$A))
                            &!grepl('I_b_', colnames(solution_space$A)))
  
  # paths starting after a parallel line 
  orgininal_paths <- solution_space$A[, grep('Path', colnames(solution_space$A))]
  parallel_to_lines <- solution_space$A[, parallel_lines_places, drop = FALSE]
  
  # find the endings of the parallel lines 
  # this is done by subtracting the elements that are parallel to the other path
  
  parallel_endings_storage <- NULL
  for(i in 1:ncol(orgininal_paths)){
    parallel_endings  <- orgininal_paths[,i] -parallel_to_lines
    common_path       <- parallel_endings != -1 
    common_path       <- apply(common_path, 2, all)
    if(!any(common_path)) next
    if(is.null(parallel_endings_storage)){
      parallel_endings_storage <- data.frame(parallel_endings[, common_path]) 
      colnames(parallel_endings_storage) = colnames(parallel_endings)[common_path]
    }else{
      cln = colnames(parallel_endings_storage)
      parallel_endings_storage <- cbind(parallel_endings_storage,parallel_endings[, common_path])
      colnames(parallel_endings_storage) =  c(cln, colnames(parallel_endings)[common_path])
    }
    
    #parallel_to_lines <- parallel_to_lines[, !common_path]
  }

  #browser()
  A1_1 <- t(parallel_endings_storage*solution_space$A$dU)
  d_U_temp <- solution_space$P$dU +big_M
  A1_2 <- t(solution_space$P[,colnames(parallel_endings_storage)]*d_U_temp)  
  A1 <- cbind(A1_1, A1_2)  
  
  rownames(A1)[] <-'parallel_line'
  b1 <- rep(allowed_voltage,nrow(A1)) + big_M
  const.dir <- rep('<=', nrow(A1))
  
  
  ## add new rows for voltage swing under zero
  A <- matrix( rep( t(A1) , 2 ) , ncol = ncol(A1) , byrow = TRUE )
  b2 <- rep(allowed_voltage*-1, nrow(A1)) - big_M
  const.dir <- c(const.dir, rep('>=', nrow(A1)))
  
  rownames(A)[] <-'parallel_line'
  
  matrices <- list(A = A, b1 = b1, b2 = b2, const.dir = const.dir)
  
  return(matrices)
}