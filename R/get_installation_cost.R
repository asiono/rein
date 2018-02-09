################################################################################
#' @title         get_installation_cost
#' @description   calculate the installation cost for a line 
#' @param line lines_data frame of package SimTOOL
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @return  installation cost of a specific segment. Material costs are in €/km in the costs data. 
#' Where installation costs are given in €/m. 
################################################################################

get_installation_cost <- function(line, verbose = 0){
  nrow_line <- nrow(line)  
  install_cost <- rep(0,nrow_line)

  for (i in 1:nrow_line) {
    line_i <- line[i,]
    
    if (line_i$type == 'line') {
      if (!exists('cable_installation_costs')) data(costs)
      
      if (is.null(line_i$settlement_class) | is.null(line_i$installation_type)) {
        specific_install_costs <- 100        
        
      } else{
        install_costs <- cable_installation_costs[
          cable_installation_costs$installation_type == line_i$installation_type, ]  
        
        install_costs <- install_costs[
          install_costs$settlement_class == line_i$settlement_class, ]
        install_factor <- line_i$install_factor 
        specific_install_costs <- as.numeric(install_costs[paste0('X', install_factor)])
      }
    
      line_i_length <- get_line_length(line_i$element, verbose = verbose)
      install_cost[i] <- specific_install_costs*line_i_length*1000
      
    }
  }
  
  return(install_cost)
}