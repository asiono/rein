################################################################################
# Description:
#' this function creates input matrices and vectors for the optimizer       
#'
#' @title         evaluate_optimization_result
#' 
#                   name         type                   description  
#' @param  \strong{result}       'result vector of the solver  
#' @param  \strong{solution_space_combined}       'data frame containing information for 
#' possible expansion alternatives. 
#' @param  \strong{grid}       'SimTOOL container of grid data
#' @param  \strong{allowed_deviation}     'voltage deviation that 
#' is permissiple for the linear approximation
#' @param  \strong{verbose}    'verbosity level 


# #@details 
#' 
#' @return 
#' This function returns a list with:
#'  a grid that incorporates all the changes
#'  a vector delta_U_solver that gives the approximated voltages  
#' the diffences between the approximated voltages and the calculated ones 
#' and a flag giving a result if the approximation is good enough
#'@keywords optimization , solution space
#'@author        Wolfgang Biener/Gunther Gust         wolfgang.biener(at)ise.fraunhofer.de
################################################################################

evaluate_optimization_result <- function(result, solution_space_combined, grid, 
                                         allowed_deviation = 1e-4, 
                                         verbose = 0 ){
 
   # adding changes to the grid and caluclating voltages by SimTOOL
  grid_solved <- create_resulting_grid(solution_space_combined,
                                       grid = grid,verbose = 3 )

  U_real_delta <- (Mod(grid_solved$U/grid$Vref/sqrt(3)) - 1)*100

  #todo look if a previous version of the script fits to our actual needs 
  delta_U_solver <- calculate_delta_voltage(grid = grid, 
                                            result = result,
                                            solution_space = solution_space_combined)
  delta_U_solver <- (Mod(delta_U_solver) - 1)*100
  delta_U_solver <- delta_U_solver[names(U_real_delta)]
  
  # calculating the difference between the voltages
  U_diff <- Mod(delta_U_solver - U_real_delta)
  
  if (verbose > 5) {
    voltage_comparison <- cbind(U_real_delta, delta_U_solver,U_diff)
    colnames(voltage_comparison) <- c('SimTOOL', 'Solver', 'Difference')
    print('Real voltage and Voltage used by the solver')
    print(round(voltage_comparison, digits = 2))
    }
  
  if (verbose > 2) {
    print(paste('maximal voltage difference in %:', round(max(U_diff),3)))
  }
  
  evaluation_list <- list()
  evaluation_list$grid <- grid_solved
  evaluation_list$U_diff <- U_diff
  evaluation_list$delta_U_solver <- delta_U_solver
  evaluation_list$U_real_delta <- U_real_delta
  
  return(evaluation_list)
}