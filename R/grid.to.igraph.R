################################################################################
#  Description:
#'  function makes an igraph out of grid$lines. as result various functions of the large package "igraph" are applicable. e.g. different plots or shortest path calculations
# 
#' @title         grid.to.igraph
#' 
#                 name         type                   description  
#' @param  \strong{lines}       'line_container of SimTOOL 
#' @param  \strong{directed}    'logical giving if the graph shall be handled as 
#' F is the default value and signifies undirected. 
#' 
#' @details 
#' 
#' possible extensions:
#'  solutions for the different data formats provided by igraph --> additional argument "format"
#' printing: new vertice shape for Grid and Trafo nodes (manual page 279+)
#' use argument layout to generate reproduceable plots (coordinates are recently random)

#'  
#' @return  igraph graph object
# @seealso
#' @author        Benjamin Krug            Benjamin.Krug(at)ise.fraunhofer.de
################################################################################



grid.to.igraph <- function(lines, directed = F) {
  library(igraph)
  relations <- data.frame(begin = lines$begin, end = lines$end, ncol = 2)
  graph <- graph.data.frame(relations, directed = directed)
  graph <- igraph.line.lengths(graph, lines)
  return(graph)
}

### set line lengths as edge weights
igraph.line.lengths <- function(graph,lines) {
  library(igraph)
  line_lengths <- get_line_length(lines$element)
  if (length(E(graph)) != length(line_lengths)) {
    stop("igraph.line.lengths: Number of edges of the graph is unequal 
         to the numer of lines of the grid. Line lengths can't be determined")
  }
  

    E(graph)$weight <-  line_lengths*1000
  
  return(graph)
  }

#################################################################################################
#### plotting routine - not perfect for every number of nodes(verticies) ########################
#################################################################################################

plot.grid.igraph <- function(graph, grid, coords = NA) {
  library(igraph)
  #library(png)
  V(graph)$size <- 5
  V(graph)$color <- "black"
  V(graph)$shape <- "circle"
  V(graph)$label <- NA
  #V(graph)$label.color<- "black"
  #V(graph)$label.dist <- 0.25 
  E(graph)$width <- 5
  E(graph)$color <- "darkgrey"
  E(graph)$lty <- 1 # 1...solid lty...line type
  
  # mark Nref
  
  ##########################################################
  #################       grid icon     ####################
  ##########################################################
  #grid_icon <- readPNG(source = "/home/bkrug/GRID.png")
  #V(graph)[grid$Nref]$raster <- replicate(1,grid_icon,simplify = F)
  #V(graph)$raster <- replicate(vcount(graph), grid_icon, simplify=FALSE)
  #V(graph)[grid$Nref]$shape <- "raster"
  #V(graph)$shape <- "raster"
  ##########################################################
  ##########################################################
  ##########################################################
  
  V(graph)[grid$Nref]$shape <- "csquare"
  V(graph)[grid$Nref]$color <- "blue"
  V(graph)[grid$Nref]$size <- 6
  V(graph)[grid$Nref]$label.dist <- 0.4

  # mark voltage trespass
  trespass_nodes <- V_trespass(grid,verbose = 0)$nodes
  V(graph)[trespass_nodes]$color <- "red" # '#... stands for RGB colors e.g. #763939
  
  # mark overusage
  if (!is.null(grid$thermal_problems)) {
    grid$thermal_problems <- grid$thermal_problems[-(1:length(grid$thermal_problems[,1])),]
  }

  if (length(find_thermal_problems(grid)) != 0) {
    thermal_problems <- find_thermal_problems(grid)$thermal_problems
    thermal_problems_line_nb <- c()
    for (i in seq_along(thermal_problems)) {
      line_nb <- which(grid$lines$begin == thermal_problems[ ,"begin_nodes"][i] &
                         grid$lines$end == thermal_problems[ ,"end_nodes"][i])
      if (length(line_nb) == 1) {
        thermal_problems_line_nb[length(thermal_problems_line_nb) + 1] <- line_nb
      }else if (length(line_nb) > 1) {
        print("plot.grid.igraph: overused lines could not be determined")
        # maybe extra error handling
      }
    }
    if (length(thermal_problems_line_nb) > 0) {
      graph <- set.edge.attribute(graph, "color", index = thermal_problems_line_nb, value = "red")
    }
    
  }
  
  
  # mark load and generation centres
  S <- Mod(grid$S_cal)
  S.range.names <- function(S,lower, upper) {
    names(S[which(S >= lower*max(S) & S < upper*max(S))])
  }

  for (i in 1:11) {
    lower = 0.1*(i - 1)
    upper = 0.1*i
    V(graph)[S.range.names(S,lower, upper)]$size <- 1.5 + i*0.5
  }
  
  # mark generators
  V(graph)[names(grid$S_cal[Re(grid$S_cal) > 0])]$shape <- "square"
  
  # hide inactive loads and generators
  if (!is.null(grid$coordinates)) {
    gen_load_labels <- c("Load","Gen","ElmGenstat","ElmLod")
    gen_load_nodes <- as.character(grid$coordinates$name[grid$coordinates$type %in% gen_load_labels])
    no_load_nodes <- names(grid$S_cal[Mod(grid$S_cal) == 0])
    gen_load_nodes_no_S <- intersect(no_load_nodes, gen_load_nodes)
  }
  #graph <- delete.vertices(graph,v = gen_load_nodes_no_S) # does not work wth igraph.line.lengths
  V(graph)[gen_load_nodes_no_S]$shape <- "none"
  
  # mark trafo "line"
  trafo_line <- grep(pattern = "trafo", x = grid$lines$type)
  E(graph)[trafo_line]$lty <- 3 # 3=dotted
  E(graph)[trafo_line]$width <- 3
  
  # mark busbars
  trafo_nodes <- c(grid$lines$begin[trafo_line], grid$lines$end[trafo_line])
  if (!is.null(grid$coordinates)) {
    busbar_nodes <- as.character(grid$coordinates$name[grid$coordinates$type == "BB"])
  } else {
    busbar_nodes <- c()
  }
  busbar_nodes <- c(trafo_nodes, busbar_nodes)
  
  V(graph)[busbar_nodes]$shape <- "vrectangle"
  V(graph)[busbar_nodes]$size <-  2 #width
  V(graph)[busbar_nodes]$size2 <- 5 # height
  V(graph)[busbar_nodes]$label.dist <- 5#0.4

  # mark size of the lines
  switch_line_nb <- grep(pattern = "switch", x = grid$lines$type)
  max_max_I <- as.numeric(max(grid$lines$max_I[-c(trafo_line,switch_line_nb)]))

  for (i in 1:6) { 
    lower = 0.2*(i - 1)
    upper = 0.2*i
    edge_numbers <- which(grid$lines$max_I >= lower*max_max_I & grid$lines$max_I < upper*max_max_I)
    graph <- set.edge.attribute(graph, "width",index = edge_numbers, value = (1.5 + i*2)) #(1.5+i*2)) #(2+i*1.5)
  }
  
  if (is.null(E(graph)$weight)) {
    graph <- igraph.edge.weights(graph, grid)
  }
  
  # mark switches
  if (!is.null(grid$switch_lines)) {
    additional_switch_line_nb <- which(grid$lines$end %in% grid$switch_lines[,"begin"]) # not really switches - only for visualization   
    switch_line_nb <- c(switch_line_nb, additional_switch_line_nb)
  }
    # E(graph)[switch_line_nb]$lty <- 4 #"dotdash"  #old solution - color is better
    # E(graph)[switch_line_nb]$width <- 2           #old solution - color is better
  E(graph)[switch_line_nb]$color <- "black" #can't be overused(extrem max_I) --> only allowed element with other color
  
  # mark overhead lines
  if (!exists('line_types') | !exists('trafo_types')) lazyLoad('types')
  overhead_line_types <- line_types$type[line_types$code == "OL1"]
  overhead_line_numbers <- which(grid$lines$model %in% overhead_line_types) # remember! line and edge numbers are identical
  E(graph)[overhead_line_numbers]$lty <- 2 # 2=dashed

  # random generation of coordinates under consideration of edge weights("line lengths") for distance between nodes (!not proportional)
  if (is.na(coords)) {
    coords <- layout.fruchterman.reingold(graph, weights = E(graph)$weight)
  } 
  par(mar = c(0,0,0,0))

  plot(graph, layout = coords) 
}

### determine edge weights fitting for layout.fruchterman.reingold
igraph.edge.weights <- function(graph,grid) {

  if (length(E(graph)) != length(grid$lines[,1])) {
    stop("igraph.line.lengths: Number of edges of the graph is unequal 
         to the numer of lines of the grid. Line lengths can't be determined")
  }
  
  line_lengths <- c()

  for (line_i in seq_along(grid$lines[,1])) {
    
    if (grid$lines$type == "line") {
      length <- grid$lines$line_l
    }else if (grid$lines$type == "trafo") {
      length <- 0
    }else if (grid$lines$type == "switch") {
      length <- 0
    }
    line_lengths <- c(line_lengths,length)
  }#for
  
  # transform line_lengths to edge weights 
  nb_no_lines <- which(line_lengths == 0)
  edge_weights <- (c(rep(1,times = length(E(graph)))) - line_lengths/(max(line_lengths)/0.9))*10
  edge_weights[nb_no_lines] <- max(edge_weights[-nb_no_lines])*10
  E(graph)$weight <-  edge_weights
  
  return(graph)
}




