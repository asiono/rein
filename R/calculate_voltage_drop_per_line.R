################################################################################
# Description:
#' the function calculates the voltage drop per element (line or transformer) linear.      
#'
#' @title         calculate_voltage_drop_per_line
#' 
#                   name         type                   description  
#' @param  \strong{Vref}       'Reference voltage of the grid
#' @param  \strong{R,X}  'Resistance, Reactance 
#' @param  \strong{I}     'Flowing current, a data frame with multiple currents per element can be handled, too.
#' @return 
#' \strong{delta_U_pu} data frame (of the same dimensions as \strong{I}) containing the voltage drops
#'@keywords voltage drop
#'@author   Wolfgang Biener/Gunther Gust             wolfgang.biener(at)ise.fraunhofer.de
################################################################################


calculate_voltage_drop_per_line <- function(Vref, R, X, I){

  I = data.frame(I) 
 
  I_active = numeric()
  I_reactive = numeric()
  
  #column-wise extraction of real an imaginary currents
  for (i in (1:ncol(I))) { 
    I_active = cbind(I_active,  Re(I[,i]))
    I_reactive = cbind(I_reactive,  Im(I[,i]))
  }
  
  #voltage drop according to VDE AR-N 4105 (and sign change)
  delta_U_pu <- -(R*I_active + X*I_reactive)/Vref

  #Column names for parallel lines (if any exist)
  colnames(delta_U_pu) = paste0("dU_",colnames(I))
  #Column name for original asset (has to be in the first column)
  colnames(delta_U_pu)[1] = "dU"
 
#   # caclulating delta U using power 
#   if(F){
#     # setting to zero because it might already exist and then cause problems
#     solution_space$P_W <- NULL
#     solution_space$Q_Var <- NULL
#     
#     
#     # based on power 
#     complex_power_melt        <- melt(grid$transm_power, value.name = 'S_kW')
#     complex_power_melt$Q_Var <- Im(complex_power_melt$S_kW)*1000
#     complex_power_melt$P_W   <- Re(complex_power_melt$S_kW )*1000
#     complex_power_melt$S_kW <-NULL
#     solution_space <-  merge(solution_space, complex_power_melt, 
#                              by.x = c("begin", "end"), by.y= c("Var1", "Var2"))
#     
#     # calculation by VDE AR-N 4105
#     R <- solution_space$R
#     P_W <- solution_space$P_W
#     Q_Var <- solution_space$Q_Var
#     X <- solution_space$X
#     
#     # todo transmission ratio
#     # sign change because of definition of flow direction
#     solution_space$delta_U_pu <- -(R*P_W+X*Q_Var)/(grid$Vref*sqrt(3))^2
#   }

  return(delta_U_pu)
 
}