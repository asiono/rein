calc_trafo_I_b <- function(grid, replacement_trafo, verbose = 3) {
  replacement_trafo$I_b <- 0
  
#replace transformer type in the grid and perform load flow calculation to get actual current
  for (i in 1:nrow(replacement_trafo)) {
    grid_calc <- grid
    
    line_nr <- which(grid_calc$lines$begin == replacement_trafo[i,"begin"] & grid_calc$lines$end == replacement_trafo[i,"end"])
    grid_calc$lines$model[line_nr] <- replacement_trafo[i,"model"]

    grid_calc$lines[,c('trafo_U1', 'trafo_U2')] <- grid$lines[,c('trafo_U1', 'trafo_U2')]
    
    source('R/wrapper.prepare.grid.R')
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