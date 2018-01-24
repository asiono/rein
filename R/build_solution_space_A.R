################################################################################
# Description:
#'the function creates the solutions space for which the grid expansion      
#'
#' @title         build_solution_space
#' 
#                   name         type                   description  
#' @param  \strong{grid}       'SimTOOL container of grid data
#' @param  \strong{types}      'character vector specifying which assets to 
#'                             consider.  trafo and line are possible
#' @param  \strong{verbose}    'verbosity level 

#' @details 
#' \strong{type} is a    
#' @return 
#' Output is a dataframe. Rows contain the lines, the expansion alternatives for 
#' the lines and the voltage drop for the expansion alternatives. 
#'@keywords paths grid_paths, solution space
#'@author        Wolfgang Biener             wolfgang.biener(at)ise.fraunhofer.de
################################################################################


build_solution_space_A <- function(grid, expansion_alternatives, verbose = 0){
  source('R/find_parallel_lines_update.R')
  source('R/get_grid_paths_and_branches.R')
  
  grid_paths <- get_grid_paths_and_branches(grid$lines)
  #get all assets of type i of the grid
  grid_assets <- create_grid_assets(grid, type = 'line')

  #add transmission ratio
  transm_ratio_assets <- grid_assets[,c("end", "transmissio_ratio")]
  transm_ratio_assets <- unique(transm_ratio_assets)
  
  #build parallel lines
  build_parallel_lines <- find_parallel_lines_update(grid$lines)
  
  parallel_lines_data <- build_parallel_lines$parallel_lines      
  
  #check if there is any parallel line possible
  if (nrow(parallel_lines_data) != 0) {
    parallel_lines_data <- merge(parallel_lines_data,transm_ratio_assets,
                              by = c('end'), sort = F)

    #add potentially flowing current 
    # to do: this is not robust aiganst a change of begin and end of lines ...
    #(which is the current of the subsequent lines starting at the end node of the parallel line)
    #Sum up current of edges leaving each node
    current_parallel <- ddply(grid_assets, "begin", summarize, I_b = sum(I_b)) 
    #Merging the dataframes 
    parallel_lines_data <- merge(parallel_lines_data, current_parallel, 
                              by.x = "end", by.y = "begin", sort = F)
  
    ####Determination of currents of original lines when parallel line installed
  
    #get assignment original lines to parallel lines
    parallel_to <- build_parallel_lines$parallel_to
    
    #initialize data frame for loop
    parallel_to_current <- parallel_to[,c("begin", "end")] 
    #for each parallel line bring current into matrix form
    for (ID in parallel_lines_data$ID) {
      current_temp <- parallel_to[,ID]*parallel_lines_data[parallel_lines_data$ID == ID,"I_b"]
      parallel_to_current <- cbind(parallel_to_current,current_temp )
      colnames(parallel_to_current)[ncol(parallel_to_current)] <- paste0('I_b_',ID)
    }
    
    #merge current matrix to original lines
    grid_assets_temp <- merge(grid_assets[,c("begin","end","I_b")], parallel_to_current, 
                             by = c("begin","end"), sort = F)

    #substract current of parallel lines from original current
    grid_assets_temp = grid_assets_temp$I_b - grid_assets_temp[,seq(4,ncol(grid_assets_temp)), drop = F]
    grid_assets = cbind(grid_assets,grid_assets_temp)
  }
  
  # input : expansion_alternatives , grid_assets, grid,  parallel_lines , parallel_to

  #construct solution space for original lines
  solution_space_A <- merge(expansion_alternatives$line, grid_assets[,!colnames(grid_assets) %in% c("model", "max_I", "cost")] ,  
                            by = NULL,sort = F)
  
  #add impedances
  solution_space_A <- calculate_impedances(grid = grid,
                                           solution_space = solution_space_A,
                                           type = 'line') 
  
  #calculate voltage drops
  source('R/calculate_voltage_drop_per_line.R')
  U_A <- calculate_voltage_drop_per_line(
    Vref = grid$Vref, R = solution_space_A$R, X = solution_space_A$X, 
    I = (solution_space_A[,grepl('I_b', colnames(solution_space_A))] * solution_space_A$transmissio_ratio))
  
  solution_space_A <- cbind(solution_space_A, U_A) 
 
  solution_space_A <- merge(solution_space_A, grid_paths[,-1], by.x = "end", 
                          by.y = "to", all.x = T, sort = F)

  # adding the parallel_to lines to the Solution_space
  if (nrow(parallel_lines_data) != 0) {
    solution_space_A <- merge(solution_space_A, parallel_to, by = c('begin','end'),
                            all = F )
  }
  
  # set cost to zero for existing lines
  solution_space_A$cost[ solution_space_A$model == solution_space_A$model_old] <- 0
  
  # ordering the solution spaces 
  first_values <- c('begin', 'end', 'max_I')
  
  additional_values <- colnames(solution_space_A)[!colnames(solution_space_A) %in% first_values]
  solution_space_A <- solution_space_A[, c(first_values,additional_values)]
  
  
  return(solution_space_A)
  
}