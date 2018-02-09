################################################################################
#' @title  calculate_impedances
#' @description   function to calculate impedances for the assets within the solution space.
#' @param grid   List containing initial grid data.
#' @param solution_space   dataframe containing possible cable types in the grid and its specifications
#' @param type   value is line or trafo to calculate impedance of respective asset type
#' @return   solution_space data with impedance 
################################################################################

calculate_impedances <- function(grid, solution_space, type){
  
  if (type == 'line') {
    # multiplying per km data with their length
    solution_space[, c("R",  "L",	"G",	"C", 'cost')] <- solution_space[, c("R",  "L",	"G",	"C", 'cost')]*solution_space$length_km
    solution_space$X <- solution_space$L*2*pi*50/1e3
    
    solution_space[, c("R",  "L",	"G",	"C", 'X')] <- 
      solution_space[, c("R",  "L",	"G",	"C", 'X')]/solution_space$transmissio_ratio^2
    
    } else if (type == 'trafo') {
      S_n <- solution_space$S
      u_k <- solution_space$uk
      P_cu <- solution_space$PCu
      U_1n <- solution_space$trafo_U1
      Vref <- grid$Vref
      
      #calculating normal current for further calculation
      I_1n <- S_n*1000/(sqrt(3)*U_1n)
      # if that is all ok from a normalizing point of view?
      I_ref <- I_1n * U_1n/Vref
      
      ############# calculating transformer Parameters 
      ########### seriel element
      Z_k <- u_k*(Vref*sqrt(3))^2/(S_n * 1000)
      R_k <- P_cu*1000/1/(I_ref^2)
      X_k <- sqrt(Z_k^2 - R_k^2)
      
      # writing results 
      solution_space$R <- R_k
      solution_space$X <- X_k 
  }
 
  return(solution_space)
   
}