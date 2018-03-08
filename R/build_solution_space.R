################################################################################
#' @title   build_solution_space
#' @description   function to collect possible asset types for lines, transformer and possible parallel lines
#' in the grid.
#' @param grid   List containing initial grid data.
#' @param avail_asset_types   List of available line and trafo types for replacement
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' @param line_types   database containing all line types and its specification
#' @param trafo_types    database containing all line types and its specification
#' @return Output is list containing 3 dataframe containing possible asset types in the grid and its specifications
################################################################################

build_solution_space <- function(grid, avail_asset_types, verbose = 0, line_types = NA, trafo_types = NA) {

  #checking if data(types) has already been executed 
  if (is.na(line_types) | is.na(trafo_types)) lazyLoad('types')
  
  #Assigning corresponding grid paths and branches to the assets (and parallel lines)
  grid_volt_lv <- as.character(grid$lines$trafo_U2[1]/1000)
  
  #This function is to build data.table containing possible cable and transformer types and their specification for reinforcement
  get_expansion_alternatives <- function(type, avail_asset_types, grid_volt_lv) {
    #get possible cable data based on grid voltage level and available asset type
    if (type == 'line') {
      line_types <- as.data.table(line_types)
      expansion_alternatives <- line_types[U == grid_volt_lv & type %in% avail_asset_types$line & cost > 0, -c('comment','creation','code')]
    }
    
    #get possible transformer data based on available asset type
    if (type == 'trafo') {
      trafo_types <- as.data.table(trafo_types)
      expansion_alternatives <- trafo_types[type %in% avail_asset_types$trafo & cost > 0, -c('comment','creation')]
    }
    
    # rename coloumn 'type' into 'model' to match grid$lines coloumn name
    setnames(expansion_alternatives, old = "type", new = "model")

    return(expansion_alternatives)
  }
  
  expansion_alternatives <- list(line = as.data.frame(get_expansion_alternatives(type = 'line', avail_asset_types, grid_volt_lv)), 
                                 trafo = as.data.frame(get_expansion_alternatives(type = 'trafo', avail_asset_types, grid_volt_lv)))
  
  solution_space_P <- build_solution_space_P(grid, expansion_alternatives, verbose = verbose)
  solution_space_A <- build_solution_space_A(grid, expansion_alternatives, verbose = verbose)
  solution_space_T <- build_solution_space_T(grid, expansion_alternatives, verbose = verbose)
  solution_space <- list("A" = solution_space_A, "P" = solution_space_P, "T" = solution_space_T)

  
  #todo verlegekosten 
  return(solution_space)
}