
################################################################################
# Description:
#' this function creates the constraints given by the current       
#'
#' @title         create_current_constrains
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

create_current_constraints <- function(solution_space,big_M, verbose = 0){
  source('~/Documents/rein2/rein.git1/reIn/R/create_current_constraints_no_parallel_lines.R')
  source('~/Documents/rein2/rein.git1/reIn/R/create_current_constrains_parallel_lines.R')
  source('~/Documents/rein2/rein.git1/reIn/R/create_current_constrain_trafo.R')
  
  matrices_no_parallel_lines <- create_current_constraints_no_parallel_lines(solution_space,
                                                                            big_M = big_M)

  dim(matrices_no_parallel_lines$A)[[1]] - length(matrices_no_parallel_lines$b)
  
  #View(cbind(matrices_no_parallel_lines$A, matrices_no_parallel_lines$b))
  matrices_parallel_lines <- create_current_constrains_parallel_lines(solution_space, big_M = big_M)
 

  # View(cbind(matrices_parallel_lines$A, matrices_parallel_lines$b))  
  matrices_transformer <- create_current_constrain_trafo(solution_space,
                                                         big_M = big_M)
  #View(cbind(matrices_transformer$A, matrices_transformer$b))  
  AI <- rbind(matrices_no_parallel_lines$A,matrices_parallel_lines$A,matrices_transformer$A)
  bI <- c(matrices_no_parallel_lines$b,matrices_parallel_lines$b,matrices_transformer$b)
  const.dir_I <- c(matrices_no_parallel_lines$const.dir,
                   matrices_parallel_lines$const.dir,
                   matrices_transformer$const.dir)
  
  
#   #no parallel constraints
#   AI <- rbind(matrices_no_parallel_lines$A,matrices_transformer$A)
#   bI <- c(matrices_no_parallel_lines$b,matrices_transformer$b)
#   const.dir_I <- c(matrices_no_parallel_lines$const.dir,
#                    matrices_transformer$const.dir)
#   
# 
  #no parallel constraints no trafo constraints
#   AI <- rbind(matrices_no_parallel_lines$A)
#   bI <- c(matrices_no_parallel_lines$b)
#   const.dir_I <- c(matrices_no_parallel_lines$const.dir)
  
  if (verbose > 3) {
    print('AI, ,const.dirI,bI')
    print(cbind(AI,const.dir_I,bI))
  }

  if (dim(AI)[[1]] != length(bI) | length(bI) !=  length(const.dir_I)) {
    #stop('There is a problem with the size of matrices')
  }
matrices_I <- list(A = AI, b = bI, const.dir = const.dir_I)
  return(matrices_I)
  
}
