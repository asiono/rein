######################################################################################################################################################
#############################################   replace_line_types   #################################################################################
######################################################################################################################################################
# the function replaces linetypes specified by a name like NAYY 4x 150 by its parameterset. The parameterset is stored in data(types)


replace_line_types <- function(lines, line_types = NA, verbose = 1) {
  #local copies
  lines$element <- as.character(lines$element)
  lines$line_l <- NA

  if (verbose >= 1) message("replace_line_types  data.frame lines")
  if (verbose >= 3) print(lines$element) 
  
  #checking if data(types) has already been executed 
  if (is.na(line_types)) lazyLoad('types')
  
  #loop line type rows
  for (i in nrow(line_types):1) {
    #searching matching elements in table
    line_match <- which(lines$model == line_types$type[i])
    
    if (length(line_match) > 0) {
      for (j in line_match) {
        type_list <- strsplit(x = lines$element[j],split = ",")

        type_vector0 <- unlist(type_list)
        type_vector0 <- sub(pattern = '[)]', x = type_vector0, replacement = '')
        lines$element[j] <- sprintf("%s%s,%s%s", "line(", lines$model[j], type_vector0[length(type_vector0)], ")")
        lines$line_l[j] <- as.numeric(type_vector0[length(type_vector0)])
      }

      if (verbose >= 2) message(sprintf("%s: %s", line_types$type[i],
                                    paste(line_match, collapse = ", ")))
      
      # making a string out of line data: RLGC
      sub_text <- sprintf("%s,%s,%s,%s", line_types$R[i], line_types$L[i], 
                          line_types$G[i] , line_types$C[i])
      
      # replacing the linetype in lines$element by its parameterset (sub_text)
      lines$element[line_match] <- sub(as.character(line_types$type[i]),
                                       sub_text, lines$element[line_match])
      #replacing the RLGCl data
      lines[line_match, 'line_R'] <- line_types$R[i]
      lines[line_match, 'line_L'] <- line_types$L[i]
      lines[line_match, 'line_G'] <- line_types$G[i]
      lines[line_match, 'line_C'] <- line_types$C[i]
      
      # if there is no column for max_I in lines it is created and the values are set to NA 
      if (!("max_I" %in% names(lines))) lines$max_I <- NA
      
      #select lines with no max_I entry and paste from line_types
      sel <- line_match[which(line_match  %in% which(is.na(lines$max_I)))]
      # setting the max_I specified in line_type
      lines[sel, "max_I"] <- line_types$max_I[i]
      
      #store empty comments
      #if (!("comment" %in% names(lines))) lines$comment <- "" 
      #unnecessary to put the comments into data frame

      # setting line_types comments to lines comments
      #lines[sel, "comment"] <- line_types$comment[i]
      
      #select lines without model description and paste
      #add model line if non-existant
      # model<-unlist(strsplit(lines$element[line_match], ","))[1] #sieht relativ unnötig aus.

      if (!("model" %in% names(lines))) lines$model <- NA
      sel <- line_match[which(line_match  %in% which(is.na(lines$model)))]
      lines[sel, "model"] <- line_types$type[i]
    }
  }

  return(lines)
}

