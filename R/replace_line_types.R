######################################################################################################################################################
#############################################   replace_line_types   #################################################################################
######################################################################################################################################################
# the function replaces linetypes specified by a name like NAYY 4x 150 by its parameterset. The parameterset is stored in data(types)


replace_line_types <- function(lines, line_types = NA, verbose = 1) {
  #local copies
  lines$element <- as.character(lines$element)

  if (verbose >= 1) message("replace_line_types  data.frame lines")
  if (verbose >= 3) print(lines$element) 
  
  #checking if data(types) has already been executed 
  if (is.na(line_types)) lazyLoad('types')
  
  #loop line type rows
  for (i in nrow(line_types):1) {
    #searching matching elements in table
    line_match <- which(lines$model == line_types$type[i])
    
    if (length(line_match) > 0) {
      if (is.null(lines$line_l)) {
        lines$line_l <- NA
        for (j in line_match) {
          type_list <- strsplit(x = lines$element[j],split = ",")

          type_vector0 <- unlist(type_list)
          type_vector0 <- sub(pattern = '[)]', x = type_vector0, replacement = '')
          lines$line_l[j] <- as.numeric(type_vector0[length(type_vector0)])
        }
      }

      if (verbose >= 2) message(sprintf("%s: %s", line_types$type[i], paste(line_match, collapse = ", ")))
      
      #replacing the RLGCl data
      lines[line_match, 'line_R'] <- line_types$R[i]
      lines[line_match, 'line_L'] <- line_types$L[i]
      lines[line_match, 'line_G'] <- line_types$G[i]
      lines[line_match, 'line_C'] <- line_types$C[i]
      lines[line_match, "max_I"] <- line_types$max_I[i]
    }
  }

  return(lines)
}

