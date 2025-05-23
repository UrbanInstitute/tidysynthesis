#' Add new variables that indicate if a value is "missing" or "not missing" for
#' original variables that contain NA 
#'
#' @param data A data frame
#' @param types A vector of variables types to expand
#' @param skip_vars A character vector of variables that shouldn't be expanded
#'
#' @return An augmented data frame with the original variables and new 
#' variables that contain the missingness patterns of variables with NA
#' 
#' @export
expand_na <- function(data, 
                      types = c("chr", "dbl", "fct", "lgl", "int", "ord"),
                      skip_vars = NULL) {
  
  # test if any variable names already end in _NA because we are going to add 
  # variables that end in _NA
  is_na_var_match <- stringr::str_detect(string = names(data), pattern = "_NA$")
  
  if (any(is_na_var_match)) {
    
    bad_vars <- names(data)[is_na_var_match]
    
    stop("If using incomplete data, variable names cannot end in '_NA'. \n",
         "Invalid variables: ", 
         paste0(bad_vars, collapse = ", "))
    
  }
  
  # create a vector of variables to keep because they are the correct types
  var_types <- data %>% 
    purrr::map_chr(.f = ~ pillar::type_sum(.x))
  
  keep_vars <- names(var_types[var_types %in% types])
  
  # exclude skip_vars
  keep_vars <- keep_vars[!keep_vars %in% skip_vars]
  
  # create a subset of variables with at least one NA
  var_names <- names(data)
  
  contain_na_lgl <- purrr::map_lgl(data, ~ sum(is.na(.x)) > 0)
  
  contain_na_chr <- var_names[contain_na_lgl]
  
  if (length(contain_na_chr) == 0) return(data) 
  
  # remove drop vars from contain_na_chr
  contain_na_chr <- contain_na_chr[contain_na_chr %in% keep_vars]
  
  if (length(contain_na_chr) == 0) return(data) 
  
  data_with_na <- dplyr::select(data, dplyr::all_of(contain_na_chr))
  
  # create new columns with a factor that indicates NA or not NA and then update
  # the names
  # 
  # these variables will end in _NA
  data_na <- purrr::map_dfc(
    .x = data_with_na, 
    .f = ~factor(ifelse(is.na(.x), "missing value", "nonmissing value"))
  )
  
  names(data_na) <- paste0(names(data_na), "_NA")
  
  # append new variables to the original data
  data_combined <- dplyr::bind_cols(data, data_na)
  
  # sort the new data frame based on a vector of names
  # 
  # the _NA variables should come immediately before their corresponding 
  # original variable (e.g. var1_NA, var1, var2, var3_NA, var3
  names_index <- vctrs::vec_interleave(paste0(var_names, "_NA"), var_names) |>
    factor()
  
  data_combined <- data_combined |>
    dplyr::select(dplyr::any_of(names_index))
    
  return(data_combined)
  
}
