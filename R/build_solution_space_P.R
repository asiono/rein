################################################################################
#' @title         build_solution_space_P
#' @description   Function to create possible solution for grid reinforcement on parallel lines
#' 
#' @param grid   List containing initial grid data.
#' @param expansion_alternatives   data frame containing available grid assets for either line or transformator reinforcement
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' 
#' @return Output is a dataframe containing possible cable types for each possible 
#' parallel lines in the grid with its specifications
################################################################################

build_solution_space_P <- function(grid, expansion_alternatives, verbose = 0){

  grid_paths = get_grid_paths_and_branches(grid$lines)
  
  #build parallel lines
  build_parallel_lines <- find_parallel_lines_update(grid$lines)
  
  #get all assets of type i of the grid
  grid_assets <- create_grid_assets(grid, type = 'line')
  transm_ratio_assets <- grid_assets[,c("end", "transmissio_ratio")]
  transm_ratio_assets <- unique(transm_ratio_assets)
  
  parallel_lines_data = build_parallel_lines$parallel_lines      
  parallel_lines_data = merge(parallel_lines_data,transm_ratio_assets,
                              by = c('end'), sort = F)
  
  #add potentially flowing current 
  # to do: this is not robust aiganst a change of begin and end of lines ...
  #(which is the current of the subsequent lines starting at the end node of the parallel line)
  #Sum up current of edges leaving each node
  current_parallel = plyr::ddply(grid_assets, "begin", summarize, I_b = sum(I_b)) 
  #Merging the dataframes 
  parallel_lines_data = merge(parallel_lines_data, current_parallel, 
                              by.x = "end", by.y = "begin", sort = F)
 
  #construct solution space for parallel lines
  solution_space_P = merge(expansion_alternatives$line, parallel_lines_data, sort = F)
  #add impedances
  solution_space_P <- calculate_impedances(grid = grid, solution_space = solution_space_P, type = 'line')   
  #Make parallel lines more expensive to prefer expansion of original lines
  solution_space_P$cost = solution_space_P$cost + 100
  
  #calculate voltage drops
  U_P = calculate_voltage_drop_per_line(
    grid$Vref, solution_space_P$R, solution_space_P$X, 
    solution_space_P$I_b*solution_space_P$transmissio_ratio)
  solution_space_P = cbind(solution_space_P, U_P) 
  
  #Assigning corresponding grid paths and branches to the assets (and parallel lines)
  solution_space_P_backup <- solution_space_P
  solution_space_P = merge(solution_space_P_backup, grid_paths[,-1], by.x = "end", 
                          by.y = "to", all.x = T, sort = F)
  
  # ordering the solution spaces 
  first_values <- c('begin', 'end', 'max_I')
  additional_values <- colnames(solution_space_P)[!colnames(solution_space_P) %in% first_values]
  solution_space_P <- solution_space_P[, c(first_values,additional_values)]
  
  return(solution_space_P)
}