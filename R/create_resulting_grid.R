###############################################################################
#  Description:
#' This function create resulting grid from optimization result and reanalyze it with SimTOOL.  
#' \code{chosen_line_type}. 
#'  
#' @title         calculate_delta_voltage
#' 
#                 name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' @param  \strong{solution_space_combined}    'solution space of the optimization problem. consisting of only one matrix. 
#' @param  \strong{expansion_measures}'from the optimizer chosen solution 

#'
#'
# @details 
#' the function calculates the delta U that is taken into account from the optimizer. 
#' This is done for every node in the grid. It is only viable for branched grids. 
#' 
#' @seealso
#' \code{\link{grid.to.igraph}} 
#' @author       Wolfgang Biener      wolfgang.biener(at)ise.fraunhofer.de
################################################################################

create_resulting_grid <- function(grid, solution_space_combined,  verbose = 0 ) {
  source('R/replace_transformer.R')
  
  S_save <- grid$S_cal
  resulting_delta_grid  <- solution_space_combined[solution_space_combined$chosen_alternative == 1,]  
  #trafo reinforcements############################

  reinforced_trafo = resulting_delta_grid[resulting_delta_grid$measure == "trafo" &
                                            resulting_delta_grid$cost > 0 ,] 
  if (nrow(reinforced_trafo) == 1) {
    start_node = reinforced_trafo[1,"begin"]
    end_node = reinforced_trafo[1,"end"]
    chosen_trafo_type = reinforced_trafo[1,"model"]
    
    grid <- replace_transformer(grid,start_node,end_node,
                                    chosen_trafo_type, verbose = 0)
  }else{
    if (nrow(reinforced_trafo) > 1) stop('more than one trafo shall be changed')
  }
  
  #line reinforcements#############################
  {grid$lines$cost_installation <- numeric(nrow(grid$lines))
    reinforced_lines = resulting_delta_grid[resulting_delta_grid$measure == "line" &
                                              resulting_delta_grid$cost > 0 ,]   
    if (nrow(reinforced_lines) > 0) {
      for (i in 1:nrow(reinforced_lines)) {
        start_node = reinforced_lines[i,"begin"]
        end_node = reinforced_lines[i,"end"]
        chosen_line_type = reinforced_lines[i,"model"]
        
        source('R/replace_line_by_bigger.R')
        grid = replace_line_by_bigger(grid, start_node,end_node,
                                      chosen_line_type, verbose = 0)
      }

    }
  }      

  #addition of bypasses############################
  {
    p_line_place <- which(resulting_delta_grid$measure == 'p-line')
    if (length(p_line_place) > 0) {
      
      old_node_names <- resulting_delta_grid$end[p_line_place]
      
      for (i in old_node_names) {
        # renaming p-line start nodes to create new grids
        resulting_delta_grid$begin[resulting_delta_grid$begin == i] <- paste0(i,'_p')
        resulting_delta_grid$end[resulting_delta_grid$end == i 
                                 & resulting_delta_grid$measure == 'p-line'] <- paste0(i,'_p')
      }
      no_pline_place <- which(resulting_delta_grid$measure != 'p-line' & resulting_delta_grid$measure != 'trafo')
      resulting_delta_grid$element <- sprintf("%s(%s,%s)", resulting_delta_grid$measure, resulting_delta_grid$model, resulting_delta_grid$length_km)
      resulting_delta_grid$ID <- sprintf("%s_%s_%s", resulting_delta_grid$measure, resulting_delta_grid$begin, resulting_delta_grid$end)
      #sort resulting_delta_grid to match grid$lines
      resulting_delta_grid[order(match(paste(resulting_delta_grid[, 'begin'], resulting_delta_grid[, 'end']), paste(grid$lines[ ,'begin'],grid$lines[ ,'end']))),]
      grid$lines[no_pline_place,c('begin','end', 'element', 'model', 'ID')] <- resulting_delta_grid[no_pline_place, c('begin','end', 'element', 'model', 'ID')]
     
      # adding new line 
      i = p_line_place
      for (i in p_line_place) {
        grid <- add.node(grid = grid,name = resulting_delta_grid$end[i])
        grid <- add.connection(grid = grid, begin = resulting_delta_grid$begin[i], 
                               end = resulting_delta_grid$end[i], type = "line", 
                               par = list(type = resulting_delta_grid$model[i],
                                          length = resulting_delta_grid$length_km[i]),
                               verbose = 0 )
      }
      grid$lines[p_line_place,]$cost_installation <-  0
      grid$lines[p_line_place,]$model <- resulting_delta_grid$model[p_line_place]
      grid$lines[p_line_place, 'model_old'] <- 'p-line'
      grid$lines$cost_material <- resulting_delta_grid$cost
      grid$lines$changed[grid$lines$cost_material != 0 ] <- T 
      grid$lines <- subset(grid$lines, select = -c(11:24))
      }
  }
  # processing the grid 
  grid$lines$max_I <- NA
  grid$S_cal[names(grid$S_cal) %in% names(S_save)] <- S_save

  #prepare element in lines for non-replaced assets
  for (j in 1:nrow(grid$lines)) {
    if (grid$lines$type[j] == 'trafo') {
    trafo_voltages <- get_trafo_voltages(trafo = grid$lines[j, 'element'])
    grid$lines$element[j] <- paste0('trafo(',grid$lines$model[j], ',', trafo_voltages$V1, ',', trafo_voltages$V2,')')
  }
  if (grid$lines$type[j] == 'line') {
    line_length <- get_line_length(line_element = grid$lines$element[j], verbose = verbose)
    grid$lines$element[j] <- paste0('line(',grid$lines$model[j], ',',line_length,')')
  }
  }
  source('~/Documents/rein2/rein.git1/reIn/R/wrapper.prepare.grid.R')
  grid_wrapper <- grid[c('SimTOOL_version', 'description', 'creation', 'Nref', 'Vref', 'frequency', 'power', 'coordinates', 'cal_node', 'lines')]
  grid_wrapper$S_cal <- S_save
  
  grid_wrapper <- wrapper.prepare.grid(grid_wrapper)
  return(grid_wrapper)  
}

