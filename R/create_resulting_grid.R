###############################################################################
#' @title  create_resulting_grid
#' @description This function create resulting grid from optimization result.
#' @param grid   List, data frame containing information of grid to be optimized.
#' @param solution_space_combined   Selected solution based on optimization result
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @return  rebuilt grid but not yet calculated
################################################################################

create_resulting_grid <- function(grid, solution_space_combined,  verbose = 0 ) {

  newgridlines <- merge(x = grid$lines[,!colnames(grid$lines) %in% c("ID", "ID_dgs")], 
                        y = solution_space_combined[solution_space_combined$measure != "p-line", 
                                                    names(solution_space_combined) != "chosen_alternative"], 
                        by.x = c("begin", "end", "type", "model", "line_l"), 
                        by.y = c("begin", "end", "measure", "model", "length_km"), all.y = TRUE)
  newgridlines[, c("trafo_U1", "trafo_U2")] <- grid$lines[, c("trafo_U1", "trafo_U2")]
  grid$lines <- newgridlines

  #addition of bypasses############################
  {
    p_line_place <- which(solution_space_combined$measure == 'p-line')
    if (length(p_line_place) > 0) {
      
      old_node_names <- solution_space_combined$end[p_line_place]
      
      for (i in old_node_names) {
        # renaming p-line start nodes to create new grids
        solution_space_combined$begin[solution_space_combined$begin == i] <- paste0(i,'_p')
        solution_space_combined$end[solution_space_combined$end == i 
                                 & solution_space_combined$measure == 'p-line'] <- paste0(i,'_p')
      }
      no_pline_place <- which(solution_space_combined$measure != 'p-line' & solution_space_combined$measure != 'trafo')
      solution_space_combined$element <- sprintf("%s(%s,%s)", solution_space_combined$measure, solution_space_combined$model, solution_space_combined$length_km)
      solution_space_combined$ID <- sprintf("%s_%s_%s", solution_space_combined$measure, solution_space_combined$begin, solution_space_combined$end)
      #sort solution_space_combined to match grid$lines
      solution_space_combined[order(match(paste(solution_space_combined[, 'begin'], solution_space_combined[, 'end']), paste(grid$lines[ ,'begin'],grid$lines[ ,'end']))),]
      grid$lines[no_pline_place,c('begin','end', 'element', 'model')] <- solution_space_combined[no_pline_place, c('begin','end', 'element', 'model')]
     
      # adding new line 
      i = p_line_place
      for (i in p_line_place) {
        grid <- add.node(grid = grid,name = solution_space_combined$end[i])
        grid <- add.connection(grid = grid, begin = solution_space_combined$begin[i], 
                               end = solution_space_combined$end[i], type = "line", 
                               par = list(type = solution_space_combined$model[i],
                                          length = solution_space_combined$length_km[i]),
                               verbose = 0 )
      }
      grid$lines[p_line_place,]$cost_installation <-  0
      grid$lines[p_line_place,]$model <- solution_space_combined$model[p_line_place]
      grid$lines[p_line_place, 'model_old'] <- 'p-line'
      grid$lines$cost_material <- solution_space_combined$cost
      grid$lines$changed[grid$lines$cost_material != 0 ] <- T 
      grid$lines <- subset(grid$lines, select = -c(11:24))
      }
  }

  return(grid)  
}

