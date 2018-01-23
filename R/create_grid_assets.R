################################################################################
# Description:
#' the functions creates a data frame with the grid assets    
#'
#' @title         create_grid_assets
#' 
#                   name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid
#' alternatives.  
#' @param  \strong{type}       'type of asset to be considered, possibel are trafo or line
#' @return 
#' \strong{grid_assets} dataframe including linedata that can be merged with 
#' expansion alternatives 
#'@keywords lines, grid expansion 
#'@author   Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################

create_grid_assets <- function(grid, type = c('trafo', 'line')){
  
  # create list with actual grid lines 
  grid_assets <-  grid$lines[grid$lines$type == type,]
  names(grid_assets)[names(grid_assets) == 'model'] <- 'model_old'
  names(grid_assets)[names(grid_assets) == 'line_l'] <- 'length_km'
  
  # mark category (Asset)
  grid_assets[,"Category"] = "A"
  
  # add transmission ratio to the lines
  transm_ratio = data.frame(grid$transm_ratio)
  colnames(transm_ratio) = "transmissio_ratio"
  grid_assets = merge(grid_assets,transm_ratio , by.x = "end", by.y = "row.names")
  
  #adding the flowing current of the grid
  currents = melt(grid$current, value.name = 'I_b')
  grid_assets <-  merge(grid_assets, currents, by.x = c("begin", "end"), by.y = c("Var1", "Var2"))
  grid_assets$I_b <- grid_assets$I_b/grid_assets$transmissio_ratio
  
  return(grid_assets)
}