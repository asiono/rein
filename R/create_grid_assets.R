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
  grid_assets = grid$lines[grid$lines$type == type,]
  grid_assets$model_old = grid_assets$model
  grid_assets$model = NULL
  
  grid_assets$length_km  <- get_line_length(grid_assets$element)
  
  # mark category (Asset)
  grid_assets[,"Category"] = "A"
  
  # add transmission ratio to the lines
  transm_ratio = data.frame(grid$transm_ratio)
  colnames(transm_ratio) = "transmissio_ratio"
  grid_assets = merge(grid_assets,transm_ratio , by.x = "end", by.y = "row.names")
  
#   grid_assets$transmissio_ratio <- NA
#   for(i in unique(grid$transm_ratio)){
#     names_trans <-names(grid$transm_ratio[which(grid$transm_ratio == i)])
#     grid_assets$transmissio_ratio[ which(grid$lines$type == type & grid$lines$begin%in%names_trans)] <- i
#   }
  
  #adding the flowing current of the grid
  currents = melt(grid$current, value.name = 'I_b')
  grid_assets <-  merge(grid_assets, currents, by.x = c("begin", "end"), by.y = c("Var1", "Var2"))
  grid_assets$I_b <- grid_assets$I_b/grid_assets$transmissio_ratio
  
  
#   
#   # todo improve this getting the complex current using grid$current
#   I_A        <- melt(grid$maxI*grid$usage, value.name = 'I_A')
#   I_A        <- cbind(I_A, type)
#   colnames(I_A)[4] <- 'type'
#   grid_assets <-  merge(grid_assets, I_A, by.x = c("begin", "end","type"),
#                         by.y= c("Var2", "Var1", "type"))
#   
#   # avoiding doubling 
#   grid_assets$max_I <- NULL 
#   grid_assets$model <- NULL 
  
  return(grid_assets)
  
}