merge_lines_transmission_current <- function(grid){
  
  # merge transmission ratio and grid lines
  trans.ratio <- as.data.table(grid$transm_ratio, keep.rownames = T)
  setnames(trans.ratio, old = 'V2', new = 'trans_ratio')
  joined_lines <- as.data.table(merge(grid$lines, trans.ratio , by.x = "end", by.y = "V1", sort = F))
  
  # remove unnecessary columns
  joined_lines[,-c('element', 'comment', 'add_x', 'add_y', 'element_type')]
  setnames(joined_lines, old = c('model', 'line_l'), new = c('model_old', 'length_km'))
  
  # there is bug in data.table not allowing table combination with complex number
  # therefore, the joined_lines is converted again to data frame
  joined_lines <- as.data.frame(joined_lines)
  
  #add the flowing current
  currents <- melt(grid$current, value.name = 'I_b')
  joined_lines <-  merge(joined_lines, currents, by.x = c("begin", "end"), by.y = c("Var1", "Var2"))
  joined_lines$I_b <- joined_lines$I_b/joined_lines$trans_ratio
  
  return(joined_lines)
}