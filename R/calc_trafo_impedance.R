################################################################################
# Description:
#' the function calculates the impedance of transformers    
#'
#' @title         calc_trafo_impedance
#' 
#                   name         type                   description  
#' @param  \strong{Vref}       'Reference voltage of the grid 
#' @param  \strong{solution_space}  'data frame containing information for possible expansion alternatives.  
#' @return 
#' \strong{solution_space} including trafo impedance 
#'@keywords voltage drop, solution space
#'@author   Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################

calc_trafo_impedance <- function(solution_space, Vref ){
  S_n <- solution_space$S
  u_k <- solution_space$uk
  P_cu <- solution_space$PCu
  #finding the place of U1n and U2n
  #nrow_solution_space <- nrow(solution_space)
  #length_element <- length(unlist(strsplit(solution_space$element[which(solution_space$element != 'NA')], ",")))
  #U1_places <- seq(from=7, to=nrow_solution_space*length_element, by=length_element)
  #U_1n <- as.numeric(unlist(strsplit(solution_space$element, ","))[U1_places])
  #U2_places <- seq(from=8, to=nrow_solution_space*length_element, by=length_element)
  #U_2n <- unlist(strsplit(solution_space$element, ","))[U2_places]
  #U_2n <- as.numeric( sub(")", "", U_2n))
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
