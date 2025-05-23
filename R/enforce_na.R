#' Add missing values where values should be missing according to _NA variables
#'
#' @param data A synthetic data frame with _NA columns
#'
#' @return A synthetic data frame with _NA columns that converts values that 
#' are labelled missing in an _NA variable to missing in the corresponding 
#' variable
#' 
#' @export
#' 
enforce_na <- function(data) {
  
  # create vectors with variable names
  #   var_names: all variable names
  #   var_names_NA: var_names that end in _NA
  #   var_names_missing_values: var_names for variables with missing values
  var_names <- names(data)
  
  var_names_NA <- var_names[stringr::str_detect(var_names, pattern = "_NA$")]
  
  # stop if there are no _NA variables
  if (length(var_names_NA) < 1) return(data)
  
  var_names_missing_values <- stringr::str_remove(var_names_NA, pattern = "_NA$")
  
  # stop if the relevant variable isn't present yet
  if (any(!var_names_missing_values %in% var_names)) return(data)
  
  # create a helper function to inject NA into locations where the _NA var says 
  # there should be an NA
  inject_na <- function(data, x) {
    
    x_NA <- paste0(x, "_NA")
    
    for (row in 1:nrow(data)) {
      
      data[row, x] <- dplyr::if_else(
        condition = data[row, x_NA, drop = TRUE] == "missing value", 
        true = NA, 
        false = data[row, x, drop = TRUE]
      )
      
    }
    
    return(data)
    
  }
  
  # iterate inject_na over variables that should have NA
  for (var in var_names_missing_values) {
    
    data <- inject_na(data = data, x = var)
    
  }
  
  return(data)
  
}
