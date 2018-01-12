################################################################################
# Description:
#'Identifies possible locations for parallel lines for grid expansion    
#'
#' @title         find_parallel_lines
#' 
#                   name         type                   description  
#' @param  \strong{lines}     'lines of the grid (part of SimTOOL container of grid data)
#' @details 
#' Starting point for all parallel lines is the busbar at the transformer. Possible end points are all nodes except leaf nodes.   
#' @return 
#' Returns a dataframe with possible begin and end nodes of parallel lines together with the line length 
#'@keywords parallel_lines
#'@author        Gunther Gust             gunther.gust(at)is.uni-freiburg.de
################################################################################


#todo: Update documentation
#todo : update length calculation nur bis zum vorletzten knoten. 
#todo : spannungsabfall ueber letzten Stueck bleibt gleich
#todo: strom durch alten leitungen verringert sich um den der durch das stueck geht. 

find_parallel_lines_update <- function(lines){
  source('~/Documents/rein2/rein.git1/reIn/R/check_number_parallel_lines.R')
  require(igraph, quietly = T)
  graph <- grid.to.igraph(lines, directed = T)
  #Get root node (todo: What if a grid has multiple transformers / Sammelschienen?)
  root_node <- lines$end[grep(lines$element, pattern = "trafo")]
  
  distance_in_edges = melt(distances(graph, v = root_node, to = V(graph), 
                                     weights = NA), value.name = "Edge_count")
  distance_in_meters = melt(distances(graph, v = root_node, to = V(graph)), 
                            value.name = "Distance_m")
  
  parallel_lines = merge(distance_in_edges, distance_in_meters)
  
  parallel_lines$Distance_m = parallel_lines$Distance_m/1000  
  colnames(parallel_lines) = c("begin", "end","Edge_count", "length_km")
  
  #remove loops and line parallel to the transformer
  parallel_lines = parallel_lines[(parallel_lines$Edge_count > 0) & (parallel_lines$end != "GRID") ,] 
  
  #remove parallel lines that are connected to a leaf node (degree = 1)
  degree = data.frame(degree(graph))
  colnames(degree) = "Degree"
  parallel_lines = merge(parallel_lines, degree, by.x = "end", by.y = "row.names", sort = F)
  parallel_lines  = parallel_lines[(parallel_lines$Degree > 1),]
  
  #remove unnecessary columns
  parallel_lines[,"Edge_count"] = NULL
  parallel_lines[,"Degree"] = NULL
  rownames(parallel_lines) = NULL
  
  #Create ID for parallel lines
  parallel_lines = cbind(parallel_lines,  paste0("Parallel_", 
                                                 seq(1:nrow(parallel_lines))))
  colnames(parallel_lines)[ncol(parallel_lines)] = "ID"
  
  parallel_lines_names <- paste0("Parallel_", seq(1:nrow(parallel_lines)))
  parallel_lines_chosen <- diag(nrow(parallel_lines))
  colnames(parallel_lines_chosen ) <- parallel_lines_names
  
  parallel_lines <- cbind(parallel_lines, parallel_lines_chosen)
  
  #Create Category
  parallel_lines$Category = "P" 
  
  #determine to which original lines the parallel lines are parallel to
  {
  
  #data frame to store original lines and parallel lines
  graph <- as.undirected(graph)
  parallel_to = lines[,c("begin", "end")]
  parallel_to <- data.frame(get.edgelist(graph))
  colnames(parallel_to) = c("begin", "end")

  end_nodes_parallel_lines = V(graph)[as.character(parallel_lines$end)]

  parallel_path = shortest_paths(graph, from = root_node,
                                 to = end_nodes_parallel_lines,
                                 weights = NULL, output = "vpath")$vpath
  
  #for each parallel path...
  for (path in (1:length(parallel_path))) {
    
    #create a subgraph containing only the path 
    subgraph = induced_subgraph(graph,unlist(parallel_path[path]))
    
    #get edgelist of subgraph
    edgelist = data.frame(get.edgelist(subgraph))
    colnames(edgelist) = c("begin", "end")
    
    #assign the corresponding parallel line
    current_parallel_line = as.character(parallel_lines$ID[path])
    edgelist[,current_parallel_line] = 1   
    
    parallel_to = merge(parallel_to, edgelist, by = c("begin", "end"), all.x = T
                        , sort = F)
  }
  colnames(parallel_to)[3:ncol(parallel_to)] <- paste0('Parallel_',
                                                       1:length(parallel_path))
  }
  
  parallel_to[is.na(parallel_to)] <- 0
  
  #   # resorting parallel_to to fit with lines
  #   parallel_to_begin_end <- parallel_to[,c('begin','end')]
  #   lines_begin_end <- lines[,c('begin','end')]
  #   
  #   for(i in 1:nrow(parallel_to)){
  #     parallet_to_i <- parallel_to_begin_end[i,]
  #     for(j in 1:nrow(lines_begin_end)){
  #        if(all(parallet_to_i %in% lines_begin_end[j,])){
  #         parallel_to_begin_end[i,] <- lines_begin_end[j,]
  #       }
  #     }
  #   }
  #   parallel_to[, c('begin', 'end')] <- parallel_to_begin_end
  
  
  parallel_to <- sort_begin_end_by_grid_order(lines, parallel_to)
  
  check_number_parallel_lines(parallel_lines, lines , root_node)

  return(list("parallel_lines" = parallel_lines, "parallel_to" = parallel_to))
}