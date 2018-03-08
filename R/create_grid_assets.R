################################################################################
#' @title         create_grid_assets
#' @description   the functions creates a data frame with the grid assets
#' @param grid  List, data frame containing information of grid to be optimized.
#' @param type  select grid assets type to be generated, default is all asset types
#' @return   data frame with the grid assets combined with transmission ratio and current
################################################################################

create_grid_assets <- function(grid, type = c('trafo', 'line')){
  
  # create list with actual grid lines 
  grid_assets <-  grid$lines[grid$lines$type == type,]
  names(grid_assets)[names(grid_assets) == 'model'] <- 'model_old'
  names(grid_assets)[names(grid_assets) == 'line_l'] <- 'length_km'
  
  # mark category (Asset)
  grid_assets[,"Category"] = "A"
  
  # add transmission ratio to the lines
  transm_ratio <- data.frame(grid$transm_ratio)
  colnames(transm_ratio) <- "transmissio_ratio"
  grid_assets <- merge(grid_assets,transm_ratio , by.x = "end", by.y = "row.names")
  
  #adding the flowing current of the grid
  currents <- reshape2::melt(grid$current, value.name = 'I_b')
  grid_assets <-  merge(grid_assets, currents, by.x = c("begin", "end"), by.y = c("Var1", "Var2"))
  grid_assets$I_b <- grid_assets$I_b/grid_assets$transmissio_ratio
  
  return(grid_assets)
}