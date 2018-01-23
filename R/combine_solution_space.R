################################################################################
# Description:
#' This function combines the the solution_spaces        
#'
#' @title         combine_solution_space
#' 
#                   name         type                   description  
#' @param  \strong{solution_space}       'data frame containing information for 
#' possible expansion alternatives. 
#' is permissiple for the linear approximation
#' @param  \strong{chosen_expansions}       'vector given from the optimizer containing the chosen expansions. 
#' @param  \strong{verbose}    'verbosity level 
# #@details 
#' 
#' @return 
#' This function returns a solution space in a single matrix containing only important parameters. 
#' Moreover the chose expansion alternatives are integrated 
#'@keywords optimization , solution space
#'@author        Wolfgang Biener         wolfgang.biener(at)ise.fraunhofer.de
################################################################################

combine_solution_space <- function(solution_space, chosen_expansions, verbose = 0){
  

# calulating the voltage at each node taken into account by the solver
solution_space$T$measure <- 'trafo'
solution_space$A$measure <- 'line'
solution_space$P$measure <- 'p-line'
needed_cols <- c('begin','end','dU','model','length_km', 'cost', 'measure')
needed_cols %in% colnames(solution_space$P)

solution_space_calc <- rbind(solution_space$T[,needed_cols],
                             solution_space$A[,needed_cols],
                             solution_space$P[,needed_cols])
solution_space_calc$chosen_alternative <- chosen_expansions

return(solution_space_calc[which(solution_space_calc$chosen_alternative == "1"),])


}
