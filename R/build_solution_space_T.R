################################################################################
#' @title         build_solution_space_T
#' @description   Function to create possible solution for grid reinforcement on transformer
#' 
#' @param grid   List containing initial grid data.
#' @param expansion_alternatives   data frame containing available grid assets for either line or transformator reinforcement
#' @param verbose   Value greater than zero to display step by step of reinforcement
#' 
#' @return Output is a dataframe containing possible transformator types in the grid with its specifications
################################################################################

build_solution_space_T <- function(grid, expansion_alternatives, verbose = 0){

  #get all assets of type i of the grid
  grid_assets <- create_grid_assets(grid, type = 'trafo')
  
  ### create solution space for transformer
  if (is.null(grid_assets$comment) || grid_assets$comment == '') {
    grid_assets$comment <- grid_assets$model
  }
  grid_assets$model = NULL
  #grid_assets$max_I = NULL
  #if there is more than 1 trafo in the original grid
  expansion_alternatives$trafo <- expansion_alternatives$trafo[rep(seq_len(nrow(expansion_alternatives$trafo)), nrow(grid_assets)), ]
  
  solution_space_T <- merge(y = grid_assets, x = expansion_alternatives$trafo , by.x = c("model", "S", "con", "uk", "PCu", "i0", "PFe", "max_I"), 
                            by.y = c("comment", "trafo_Sn", "trafo_group", "trafo_uk", "trafo_Pcu", "trafo_i0", "trafo_Pfe", "max_I"), all = T)
  solution_space_T[,'type'] <- 'trafo'
  solution_space_T[,'model_old'] <- grid_assets$comment
  
  #add additional data, 
    solution_space_T[,'trafo_U1'] <- grid_assets$trafo_U1
    solution_space_T[,'trafo_U2'] <- grid_assets$trafo_U2
    solution_space_T[,'transmissio_ratio'] <- grid_assets$transmissio_ratio
    
    solution_space_T[,c('begin', 'end', 'type', 'length_km', 'Category')] <- grid_assets[,c('begin', 'end', 'type', 'length_km', 'Category')]

  #calculate I_b for each trafo type
  replacement_trafo <- as.data.frame(solution_space_T[which(solution_space_T$model != 
                                                              grid_assets$model_old),c('begin','end', 'model')])
  replacement_trafo <- calc_trafo_I_b(grid, replacement_trafo)

  for (j in 1:nrow(replacement_trafo)) {
    solution_space_T[(which(solution_space_T$model == replacement_trafo$model[j])), 'I_b'] <- replacement_trafo$I_b[j]
  }
  #impedances
  solution_space_T <- calculate_impedances(grid = grid,
                                           solution_space = solution_space_T,
                                           type = 'trafo')  
  #Voltage drops
  U_T <- calculate_voltage_drop_per_line(
    Vref = grid$Vref, R = solution_space_T$R, X = solution_space_T$X, 
    I = solution_space_T$I_b*solution_space_T$transmissio_ratio)
  solution_space_T <- cbind(solution_space_T, U_T) 
  
  #todo : throw an error if there is no old version of the line
  # set cost to zero for existing trafo
  solution_space_T$cost[solution_space_T$model == solution_space_T$model_old] <- 0
  
  # ordering the solution spaces 
  first_values <- c('begin', 'end', 'max_I')
  additional_values <- colnames(solution_space_T)[!colnames(solution_space_T) %in% first_values]
  solution_space_T <- solution_space_T[ ,c(first_values, additional_values)]
  solution_space_T <- solution_space_T[order(solution_space_T[,'max_I']), ]
  #solution_space_T[,which(grepl('line', colnames(solution_space_T)))] <- NULL
  
  #todo verlegekosten 
  return(solution_space_T)
  
}