################################################################################
#' @title         get_grid_paths
#' @description   For each line the corresponding path in the grid is determined. It is based on
#' a shortest paths analysis from each end node of the grid to the low-voltage side of the transformer.  
#' @param lines lines_data frame of package SimTOOL
#' @return Output is a dataframe. Rows contain the lines. For each path a column indicates whether 
#' the corresponding line is contained (1) or not (0).
################################################################################

get_grid_paths_and_branches <- function(lines) {

  #generate igraph object
  #graph <- grid.to.igraph(lines, directed = T)
  graph = graph.data.frame(lines, directed = T)
  #debug
  graph = graph.data.frame(lines, directed = F)

  #determine root node (transformer)
  root_node <- lines$begin[grep(lines$trafo, pattern = "trafo")]
  #create the output data frame
  edges_df = get.data.frame(graph)  
  edges_df = edges_df[,c(1,2)]
  
  feeder_ends <- find_leaves(lines = lines)
  
  #for each path from the root node to an end note the lines contained are determined
  for (feeder_nb in 1:length(feeder_ends)) {
    #select current path
    feeder_end <- feeder_ends[feeder_nb]
    #determine lines on the path
    path_edges <- get.shortest.paths(graph, from = root_node, to = feeder_end, output = c("epath"))$epath[[1]]
    #build the column vector to store the lines on the path
    current_path = rep(0, nrow(edges_df))
    #only for the edges on the path the vector values are set to 1
    current_path[path_edges] = 1
    #add vector to output data frame
    edges_df = cbind(edges_df, current_path)
    #name the path
    colnames(edges_df)[length(colnames(edges_df))] = paste0("Path", feeder_nb)
  }
  
  #determine grid branches
  #todo: this will probably not work for grids with multiple transformers / busbars
    #get the lines connected to the main busbar (bb). Each of them is the beginning of a branch
  bb_node = lines[lines$type == "trafo",  "end"]
  lines_con_to_bb = lines[lines$begin == bb_node, c("begin",  "end")]
  
  #the paths assigned to the lines are together in a branch
  line_paths = merge(lines_con_to_bb, edges_df, by.x = c("begin",  "end"), by.y = c("from", "to") )
  
  #assign the branches to the other assets
  #if the value of the matrix multiplication is greater than 1, the asset is in the branch
  branch_assignment = data.matrix(edges_df[,-c(1,2)]) %*% data.matrix(t(line_paths[,-c(1,2)]))
  branch_assignment = data.frame(branch_assignment)
  #reduce positive values to get 0 and 1 values again for FALSE and TRUE
  branch_assignment <- sign(branch_assignment)

  colnames(branch_assignment) = gsub("X", "Branch",colnames(branch_assignment))
  
  edges_df = cbind(edges_df, branch_assignment)

  edges_df <- sort_begin_end_by_grid_order(lines, edges_df, cols = c('from','to'))
  
  return(edges_df)
}

