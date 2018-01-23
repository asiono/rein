#to do documentation

plot_grid_rein = function(grid, U_set = 400, vertex_label = T, edge_label = T, allowed_voltage = .03){
  
  require(igraph)
  #grid = grid_overloaded
  #grid = grid_solved

  grid_data = grid$lines
  
  # add transmission ratio to the lines
  transm_ratio = data.frame(grid$transm_ratio)
  colnames(transm_ratio) = "transmissio_ratio"
  grid_data = merge(grid_data,transm_ratio , by.x = "end", by.y = "row.names")
  
  #add flowing currents
  currents = melt(grid$current, value.name = 'I_a')
  grid_data <-  merge(grid_data, currents,
                      by.x = c("begin", "end"), by.y = c("Var1", "Var2"))
  grid_data$I_a <- grid_data$I_a/grid_data$transmissio_ratio
  grid_data$mod_I_a = Mod(grid_data$I_a)
  grid_data$Usage = grid_data$mod_I_a / grid_data$max_I 
  source('~/Documents/rein2/rein.git1/reIn/R/sort_begin_end_by_grid_order.R')
  grid_data <- grid_data[match(grid$lines$end, grid_data$end),]
  
  grid_graph = graph.data.frame(grid_data, directed = F)
  
  #add vertex data
  
  #Voltage
  V(grid_graph)$U <- as.complex(NA)
  ord <- match(names(grid$U), V(grid_graph)$name)
  V(grid_graph)$U[ord] <- grid$U_res[match(V(grid_graph)$name,names(grid$U_res))]
  V(grid_graph)$Mod_U = Mod(V(grid_graph)$U[ord])
  #Voltage in p.u. 
  #modified for oltc because grid$U_res/trafo_U2 and grid$U/grid$Vref*sqrt(3) in OLTC are different
  V(grid_graph)$u <- c(V(grid_graph)$Mod_U[which(names(V(grid_graph)) == "GRID")]/grid$Vref, 
                       V(grid_graph)$Mod_U[which(names(V(grid_graph)) != "GRID")]/U_set)
  
  #Loads
  V(grid_graph)$S <- as.complex(NA)
  ord <- match(names(grid$S), V(grid_graph)$name)
  V(grid_graph)$S[ord] <- grid$S[match(V(grid_graph)$name,names(grid$S))]
  V(grid_graph)$Mod_S = Mod(V(grid_graph)$S)
  
  
  
  #Plotting#############################################
  
  #layout
  root = V(grid_graph)[name == grid$Nref]
  layout = layout_as_tree(grid_graph, root = root) #Das Layout des Graphen wird definiert

  #edges
  model = paste0("Model: ", E(grid_graph)$model)
  I_a = paste0("I_a: ", E(grid_graph)$mod_I_a)
  I_r = paste0("I_r: ", E(grid_graph)$max_I)
  usage = paste0("Usage: ", round(E(grid_graph)$Usage*100, 2), "%")
  usage_small <- Mod(E(grid_graph)$Usage*100) < 0
  usage[usage_small] <- ''
  model[usage_small] <- ''
  
  edge.label = paste(model, I_a ,I_r, usage, sep = '\n')
  edge.label = paste(model, usage, sep = '\n')
  #edge.label = paste(model, sep='\n')
  
  
  #Color edges depending on usage
  scale <- colorRamp(c('green','yellow',"red"))
  usage_scale <- E(grid_graph)$Usage
  usage_scale[usage_scale > 1] <- 1
  
  E(grid_graph)$color = apply(scale(usage_scale)
                              , 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255))
  
  
  
  #vertices
  name = paste0("Name: ", V(grid_graph)$name)
  u = paste0("u: ", round((V(grid_graph)$u - 1)*100,4), '%')
  Mod_S = paste0("S: ", round(V(grid_graph)$Mod_S,0), " [kVA]")
  u_small <- which(Mod((V(grid_graph)$u - 1)*100) < 2)
  u[u_small] <- ''
  
  vertex.label = paste(name, u, Mod_S, sep = '\n')
  vertex.label = paste(name, u, sep = '\n')
  
    
  #Color vertices depending voltage level
  u_scale <- (Mod((V(grid_graph)$u - 1)/allowed_voltage))^5
  u_scale[u_scale > 1] <- 1
  V(grid_graph)$color = apply(scale(u_scale)
                              , 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )
  
  if (!vertex_label) vertex.label <- NA
  if (!edge_label) edge.label <- NA

  
  tkplot(grid_graph, layout = layout,
         vertex.label = vertex.label, edge.label = edge.label, edge.width = 3,
         margin = .5,  rescale = FALSE, canvas.width = 800, canvas.height = 1000)
  
  # plot(grid_graph, layout=layout, rescale=FALSE, xlim=range(layout[,1]),
  #      ylim=range(layout[,2]), vertex.label=vertex.label,
  #      edge.label = edge.label, edge.width=3, 
  #       margin = .1)
       
       
}
