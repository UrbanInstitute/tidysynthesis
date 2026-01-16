#' Redefine `NA` value for a dataset.
#'  
#' @param data A `data.frame` object 
#' @param col_schema A col_schema from a `schema` object 
#'
#' @return A `data.frame`
#'  
#' @examples
#'
#' # create custom NA filter
#' example_na_custom <- example_na |>
#'   tidyr::replace_na(
#'     list("wages" = -999)
#'   )
#'
#' example_na_expanded_custom <- enforce_custom_na(
#'   data = example_na_custom,
#'   col_schema = list(
#'     "wages" = list(
#'       dtype = "dbl",
#'       na_value = -999
#'      )
#'    )
#'  )
#' 
#' @export
enforce_custom_na <- function(data, col_schema) {
  
  # for each column in col_schema
  for (col in names(col_schema)) {
    
    na_value <- col_schema[[col]][["na_value"]]
    
    # if custom value isn't currently `NA` and column exists in data...
    if (!is.na(na_value) & (col %in% names(data))) {
      
      if (col_schema[[col]][["dtype"]] == "fct") {
        
        # if factor, recode level to count as `NA`
        data <- data |>
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(c(col)), 
              \(x) { forcats::fct_recode(x, NULL = na_value) }
            )
          )
        
      } else {
        
        # replace all instances of the new value with `NA`
        data <- data |>
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(c(col)),
              \(x) { dplyr::if_else(x == na_value, NA, x) }
            )
          )
        
      }
      
      
      
    }
    
  }
  
  return(data)
  
}
