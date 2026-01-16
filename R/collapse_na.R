#' Collapse data frames with _NA variables to coerce related variables to 
#' include NA
#'
#' @param data A data frame with columns ending in _NA
#'
#' @return A data frame with no _NA columns and NA values
#' 
#' @examples
#' 
#' example_na_expanded <- expand_na(data = example_na)
#'
#' collapse_na(data = example_na_expanded)
#' 
#' @export
collapse_na <- function(data) {
  
  # create vectors with variable names
  #   var_names: all variable names
  #   var_names_NA: var_names that end in _NA
  #   var_names_missing_values: var_names for variables with missing values
  var_names <- names(data)
  
  var_names_NA <- var_names[stringr::str_detect(var_names, pattern = "_NA$")]
  
  var_names_missing_values <- stringr::str_remove(var_names_NA, pattern = "_NA$")
  
  # create a helper function inject NA into locations where the _NA var says 
  # there should be an NA
  inject_na <- function(data, x) {
    x_NA <- paste0(x, "_NA")
    
    data[[x]] <- dplyr::if_else(
      condition = data[[x_NA]] == "missing value", 
      true = NA, 
      false = data[[x]]
    )
    
    return(data)
    
  }
  
  # iterate inject_na over variables that should have NA
  for (var in var_names_missing_values) {
    
    data <- inject_na(data = data, x = var)
    
  }
  
  # remove _NA variables
  data <- data |>
    dplyr::select(-dplyr::any_of(var_names_NA))
  
  return(data)
  
}
