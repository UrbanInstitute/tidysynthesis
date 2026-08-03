#' Add missing values where values should be missing according to _NA variables
#'
#' @param data A synthetic data frame with _NA columns
#'
#' @return A synthetic data frame with _NA columns that converts values that 
#' are labelled missing in an _NA variable to missing in the corresponding 
#' variable
#' 
#' @examples
#' 
#' example_na_expanded <- expand_na(data = example_na)
#' 
#' enforce_na(data = example_na_expanded)
#' 
#' @export
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
  
  # Process:
  #(1) Create a mask matrix with nrow(data) rows and columns for each of the
  #.      columns with NA values. This is mask
  #(2) Extract columns with relevant values to be replaced with NA. This is values 
  #(3) Use mask to update values to NA where mask says so
  #(4) Update data's relevant columns with values
  
  
  # Step 1: Create mask
  mask <- as.matrix(data[var_names_NA] == "missing value")
  
  # Step 2: Extract columns 
  values <- data[var_names_missing_values]
  
  # Step 3: Apply the mask to update values to NA
  values[mask] <- NA
  
  # Step 4: Write the updated columns back
  data[var_names_missing_values] <- values
  
  return(data)
  
}