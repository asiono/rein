################################################################################
#' @title   calc_trafo_impedance
#' @description   function that calculates the impedance of transformers   
#' @param solution_space   dataframe containing possible transformer types in the grid and its specifications
#' @param Vref   reference voltage in the grid, default value is 400 V for low voltage distribution grid
#'
#' @return   solution_space data with impedance 
################################################################################

calc_trafo_impedance <- function(solution_space, Vref = 400){
  S_n <- solution_space$S
  u_k <- solution_space$uk
  P_cu <- solution_space$PCu
  U_1n <- solution_space$trafo_U1

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
  
  return(solution_space)
}
