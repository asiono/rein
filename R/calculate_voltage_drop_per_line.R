################################################################################
#' @title  calculate_voltage_drop_per_line
#' @description   function to calculate the voltage drop per element (line or transformer) linear
#' @param Vref  voltage reference of the grid
#' @param R   Resistance
#' @param X   Impedance
#' @param I   Current
#' @return  voltage drop of the specific lines or transformer
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
 
  return(delta_U_pu)
 
}