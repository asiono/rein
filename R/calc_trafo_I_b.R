calc_trafo_I_b <- function(grid, replacement_trafo, verbose = 3) {
  require(reshape2)
  replacement_trafo$I_b <- 0
  for (i in 1:nrow(replacement_trafo)) {
    grid_calc <- grid
    start_node = replacement_trafo[i,"begin"]
    end_node = replacement_trafo[i,"end"]
    chosen_trafo_type = replacement_trafo[i,"model"]
    grid_calc <- replace_transformer(grid_calc,start_node,end_node,
                                chosen_trafo_type, verbose = 0)

    for (j in 1:nrow(grid_calc$lines)) {
      if (grid_calc$lines$type[j] == 'trafo') {
        trafo_voltages <- get_trafo_voltages(trafo = grid_calc$lines[j, 'element'])
        grid_calc$lines$element[j] <- paste0('trafo(',grid_calc$lines$model[j], ',', trafo_voltages$V1, ',', trafo_voltages$V2,')')
      }
    }
    grid_calc$lines[,grepl('trafo',names(grid_calc$lines))] <- NULL
    grid_calc$lines[,c('trafo_U1', 'trafo_U2')] <- grid$lines[,c('trafo_U1', 'trafo_U2')]
    
    source('~/Documents/rein2/rein.git1/reIn/R/wrapper.prepare.grid.R')
    grid_calc <- wrapper.prepare.grid(grid_calc)
    
    #adding the trans ratio of the grid
    transm_ratio = data.frame(grid_calc$transm_ratio)
    colnames(transm_ratio) = "transmissio_ratio"
    replacement_trafoa <- merge(replacement_trafo,transm_ratio , by.x = "end", by.y = "row.names")

    #adding the flowing current of the grid
    currents = melt(grid_calc$current, value.name = 'I_b')
    replacement_trafoa$I_b <- NULL
    replacement_trafoa <-  merge(replacement_trafoa, currents,
                          by.x = c("begin", "end"), by.y = c("Var1", "Var2"))
    replacement_trafo[i, 'I_b'] <- replacement_trafoa$I_b[i]/replacement_trafoa$transmissio_ratio[i]
  }
  
  return(replacement_trafo)
}