###############################################################################################
#' @title   check.lines
#' @description prepares lines and trafos for load flow calculation
#' @param grid  List containing information of grid to be optimized.
#' @param verbose   Verbosity level. value greater than zero to display step by step of reinforcement
#' @return grid data with transmission ratio and converted lines
###############################################################################################

convert.lines <- function(grid, verbose = 1) {
  
  if (verbose >= 1) message("### call: convert_lines  data.frame lines")
  
  ##################### setting variables for further calculations##############################
  
  if (is.null(grid$lines$trafo_Sn)) grid$lines <- replace_trafo_types(grid$lines)

  grid <- input.check.convert.lines(grid, verbose)
  
  lines <- grid$lines
  Vref <- grid$Vref
  Nref <- grid$Nref
  frequency <- grid$frequency
  cal_node <- grid$cal_node
  
  # setting lines parameters and assuring the right format
  lines$begin <- as.character(lines$begin)
  lines$end <- as.character(lines$end)
  lines$seriel <- NA
  lines$parallel <- NA
  lines$element_map <- ""
  
  if (verbose >= 2) print(summary(lines))
  if (verbose >= 2) print(sprintf("Vref: %f V", Vref))

  lines <- convert.trafos(lines,Vref,verbose)
  
  transm_ratio <- create.transmission.ratio(Vref, Nref, cal_node, lines)
  
  grid <- convert.lines.switches(grid, verbose)
  
  lines <- convert.lines.lines(lines,transm_ratio,frequency, verbose)
  
  maxI <- create.maxI(lines, Nref, cal_node, verbose)
  
  
  ############################# writing results #######################
  
  
  # writing results to list grid 
  # lines
  grid$lines_cal <- lines
  grid$transm_ratio <- transm_ratio
  # maxI 

  if (!is.null(maxI)) {

    grid$maxI <- maxI
    nodes <- dimnames(maxI)[[1]]
    maxI_cal <- maxI*grid$transm_ratio[nodes]
    grid$maxI_cal <- maxI_cal
  } 
  
  
  return(grid)
} 

create.maxI <- function(lines, Nref, cal_node, verbose){

  if (!is.null(lines$max_I)) {
    if (verbose >= 1) message(sprintf("create.maxI(grid, verbose=%d)", verbose))
    
    # asigning nodes vektor 
    nodes <- c(Nref, cal_node)

    if (verbose >= 1) message("###     CREATE maxI MATRIX")
    
    maxI <- matrix(0, nrow = length(nodes), ncol = length(nodes), dimnames = list(nodes, nodes))
    
    lines$max_I <- as.numeric(as.character(lines$max_I)) 
    
    # NON DIAGONAL ELEMENTS (only connect admittances)

    for (i in 1:length(lines$begin)) {
     
      if (lines$type[i] == 'trafo') {
        # assumption maximal current is always for the voltage at the high voltage side.  
        n_os <- lines$trafo_U1[i]
        n_us <- lines$trafo_U2[i]
        power_trafo <- lines$trafo_Sn[i]
        
        # calculating trafo power by rated current and rated voltage at both sides
        # of the transformer. the problem is that there is only one max current given, 
        # where there a in reality two at each side of the transformer

        power_calc_us  <- n_us * lines$max_I[i] * sqrt(3)/1000
        if (power_calc_us > 0.9*power_trafo && power_calc_us < 1.1*power_trafo) {
          line_max_I_os <- lines$max_I[i]*n_us/n_os
          maxI[lines$begin[i],lines$end[i]] <- maxI[lines$begin[i],lines$end[i]] + line_max_I_os
          maxI[lines$end[i],lines$begin[i]] <- maxI[lines$end[i],lines$begin[i]] + lines$max_I[i]  
        }else{
          line_max_I_us <- lines$max_I[i]*n_os/n_us
          maxI[lines$begin[i],lines$end[i]] <- maxI[lines$begin[i],lines$end[i]] + lines$max_I[i]  
          maxI[lines$end[i],lines$begin[i]] <- maxI[lines$end[i],lines$begin[i]] + line_max_I_us
        }
      }else{
        maxI[lines$begin[i],lines$end[i]] <- maxI[lines$begin[i],lines$end[i]] + lines$max_I[i]
        maxI[lines$end[i],lines$begin[i]] <- maxI[lines$end[i],lines$begin[i]] + lines$max_I[i]
      }
    }
    return(maxI)  
  }else{
    return(NULL)
  }
}


input.check.convert.lines <- function(grid, verbose = 1){
  

  if (is.null(grid$lines)) stop("convert.lines: list 'grid' does not contain lines --> stop") 
  
  
  if (is.null(grid$Vref) ) {
    warning("convert.lines: list 'grid' does not define Vref: set it to 20e3 (phase - phase)") 
    if (verbose > 0) warning("convert.lines: list 'grid' does not define Vref: set it to 20e3 (phase - phase)") 
    grid$Vref <- 20e3/sqrt(3)
  }
  
  if (is.null(frequency) ) {
    warning("convert.lines: list 'grid' does not define frequency: set to 50 Hz") 
    if (verbose > 0) warning("convert.lines: list 'grid' does not define frequency: set to 50 Hz") 
    grid$frequency <- 50
  }
  
  if (is.null(grid$Nref)) {
    stop("convert.lines: list 'grid' does not contain a slack node") 
    grid$Nref <- "GRID"
  }

  if (is.null(grid$cal_node) ) {
    if (verbose > 0) message(sprintf("convert.lines: list 'grid' does not define
                                 cal_node: will be defined, slack set to
                                 grid$Nref:'%s'", grid$Nref)) 
    
    # constructing calculations from grid$power$name ::: problem with switch nodes
    cal_node <- as.character(grid$power$name)[which(as.character(grid$power$name) != grid$Nref)]
    grid$cal_node <- cal_node
    #update type
    grid$power[which(grid$power$name == grid$Nref),"type"] <- "Slack"
  }
  
  return(grid)
}

create.transmission.ratio <- function(Vref,Nref,cal_node, lines){
  
  nodes_names <- c(Nref,cal_node)
  lines_without_trafo <- lines[grep('trafo',lines$type, invert = T), ]
  
  trafo_places <- grep('trafo', lines$type)  
  
  # setting variables
  connected <- c()
  transm_ratio <- c()
  
  U_1n <- lines$trafo_U1
  U_2n <- lines$trafo_U2
  ### Transformation Ratio ###
  
  
  U_1str <- U_1n
  U_2str <- U_2n
  
  w_os <- U_1str/Vref/sqrt(3)
  w_us <- U_2str/Vref/sqrt(3)
  
  for (n in trafo_places) {
    connected_os <- SimTOOL::search_connected_points(lines_without_trafo,lines$begin[n])
    connected_us <- SimTOOL::search_connected_points(lines_without_trafo,lines$end[n])
    # by the first transformer connected is empty. For the first one the condition is always true. 

    if ((lines$begin[n] %in% connected) == FALSE) {
      connected <- c(connected,connected_os)
      transm_ratio <- c(transm_ratio, rep(w_os[n], length(connected_os)))
    }
    
    if ((lines$end[n] %in% connected) == FALSE) {
      
      connected <- c(connected,connected_us)
      w_os_real <- transm_ratio[connected == connected_os[1]][1]
      transm_ratio <- c(transm_ratio, rep(w_os_real/w_os[n]*w_us[n], length(connected_us))) 
      
    }
    
    # if factors have been created they are substituted here
    connected <- as.character(connected)
    node_parameter <- (data.frame(connected, transm_ratio))
  }
  if ( length(trafo_places) == 0) {
    transm_ratio <- rep(1,length(cal_node) + 1)
    names(transm_ratio) <- nodes_names
  }else{
    
    #defining the name of the column 1 as "name"
    names(node_parameter)[1] <- "name"
    
    # sorts list alphabetically by its names 
    node_parameter <- node_parameter[match(unique(node_parameter$name), node_parameter$name), ]
    
    #creates matrix from transm_ratio. 
    transm_ratio <- matrix(node_parameter$transm_ratio, ncol = 1, dimnames = list(node_parameter$name,"transm_ratio"))
    # adding slacknode to vector with transmission ratio
    # brings transm_ration in the order Nref, cal_node 
    transm_ratio <- transm_ratio[nodes_names,]
  }
  return(transm_ratio)
}

convert.lines.lines <- function(lines, transm_ratio,frequency,verbose){
  if (verbose >= 1) message("###    CALCULATE LINE PARAMETERS")
  lines_places <- grep('line', lines$type)
  R <- lines$line_R[lines_places]
  L <- lines$line_L[lines_places]
  G <- lines$line_G[lines_places]
  C <- lines$line_C[lines_places]
  l <- lines$line_l[lines_places]

  transm_ratio_line <- transm_ratio[lines$begin[lines_places]]
  
  lines$seriel[lines_places] <-  1/(R*l + (L/1000*2*pi*frequency*l)*1i)*transm_ratio_line^2
  lines$parallel[lines_places] <- (G/1000000/2*l + 1i*2*pi*frequency*C/1000000000/2*l)*transm_ratio_line^2
  lines$element_map[lines_places] <- "line"
  
  return(lines)
  browser()
}

convert.lines.switches <- function(grid, verbose){
  if (verbose >= 1) message("###    REPLACE SWITCHES")
  places <- grep('switch', grid$lines$type)
  
  for (n in places) {
    state <- as.numeric(substring(grid$lines$element[n],8 , 8))
    if (state == 1) {
      transm_ratio_line <- grid$transm_ratio[grid$lines$begin[n]]
      grid$lines$seriel[n] <-  1/(0.00001 + 0*1i)*transm_ratio_line^2
      grid$lines$parallel[n] <- (0 + 1i*0)*transm_ratio_line^2
    } else {
      grid$lines$seriel[n] <- 0
      grid$lines$parallel[n] <- 0
    }
    grid$lines$type[n] <- "switch"
    grid$lines$element_map[n] <- "switch"
  }
  
  return(grid)
  
}

convert.trafos <- function(lines, Vref, verbose){
  
  trafo_places <- grep('trafo', lines$type)
  
  # allocating variables for further calculation, could be done easier if variables were net converted into a string before 
  S_n <- lines$trafo_Sn
  u_k <- lines$trafo_uk
  P_cu <- lines$trafo_Pcu
  i_0 <- lines$trafo_i0
  U_1n <- lines$trafo_U1

  #calculating normal current for further calculation
  I_1n <- S_n*1000/(sqrt(3)*U_1n)

  # if that is all ok from a normalizing point of view?
  I_ref <- I_1n * U_1n/Vref
  
  ############# calculating transformer Parameters 
  ########### seriel element
  Z_k <- u_k*(Vref*sqrt(3))^2/(S_n * 1000)
  R_k <- P_cu*1000/1/(I_ref^2)
  X_k <- sqrt(Z_k^2 - R_k^2)
  
  lines$seriel[trafo_places] <- 1/(R_k[trafo_places] + 1i * X_k[trafo_places])
  
  # debuging output 
  if (verbose >= 2) print("serienelement")
  if (verbose >= 2) print( lines$seriel[trafo_places])

  ########### parallel element
  # old version 

  Z_m <- (Vref/(sqrt(3) * i_0) * I_1n)# problem, dass immer V_ref benützt wird und ein festes I_1n. Somit wird der widerstand nicht richtig transformiert.

  lines$parallel[trafo_places] <- 1/as.complex(0 + 1i*Z_m[trafo_places])
  lines$element_map[trafo_places] <- "trafo"
  
  return(lines)
}

