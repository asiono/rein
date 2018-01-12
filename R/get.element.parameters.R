### get.element.parameters      
### function extracts the single parameters of an element out of grid$lines$element

## debug
## parameters <- get.element.parameters(grid$lines$element[1])

get.element.parameters <- function(line_element) {
 # browser()
  element <- as.character(line_element)
  parameters <- list()
  split <- strsplit(x = element, split = ",")

  #type <- strsplit(x=split[[1]][1], split="[(]")[[1]][1]
  type <- get.element.type(line_element)  

  if (type == "trafo") {
    parameters$S <- as.numeric(strsplit(x = split[[1]][1], split = "[(]")[[1]][2])
    parameters$con <- (split[[1]][2])
    parameters$uk <- as.numeric(split[[1]][3])
    parameters$PCu <- as.numeric(split[[1]][4])
    parameters$i0 <- as.numeric(split[[1]][5])
    parameters$PFe <- as.numeric(split[[1]][6])
    parameters$V1 <- as.numeric(split[[1]][7])
    parameters$V2 <- as.numeric(strsplit(x = split[[1]][8], split = "[)]")[1][1])
    
    if (is.na(parameters$S) | is.na(parameters$con) | is.na(parameters$uk) |
       is.na(parameters$PCu) | is.na(parameters$i0) | is.na(parameters$PFe) |
       is.na(parameters$V1) | is.na(parameters$V2)) {
    }
    
    
  }else if (type == "line") {
    parameters$R <- as.numeric(strsplit(x = split[[1]][1], split = "[(]")[[1]][2])
    
    L <- strsplit(x = split[[1]][2], split = "/")
    if (length(L[[1]]) > 1 ) {
      parameters$L <- as.numeric(L[[1]][1])
    } else{
      parameters$L <- as.numeric((split[[1]][2]))  
    }
    parameters$G <- as.numeric((split[[1]][3]))
    parameters$C <- as.numeric((split[[1]][4]))
    parameters$l <- as.numeric(strsplit(x = split[[1]][5], split = "[)]")[[1]][1])

    if (is.na(parameters$L) | is.na(parameters$G) | is.na(parameters$C) | is.na(parameters$l) | is.na(parameters$R)) {
      parameters
    }

  }else if (type == "switch") {
    parameters$close <- as.numeric(strsplit(x = (strsplit(x = split[[1]][1], split = "[(]")[[1]][2]), split = "[)]")[[1]][1])
  }
  parameters$type <- type
  
  
  return(parameters)
}

get.element.type <- function(lines_element){
  element <- as.character(lines_element)
  split <- strsplit(x = element, split = "[(]")
  type <- c()
  for (i in seq_along(split)) {
    type <- c(type,split[[i]][1])
  }  
  return(type)
}

