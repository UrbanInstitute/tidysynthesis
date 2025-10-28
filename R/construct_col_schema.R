#' Construct a list of `col_schema`s that aligns with a visit sequence.
#' 
#' @param roadmap A `roadmap` S3 object
#' 
#' @return A named list of lists mapping variable names to their `col_schema`
#' specifications.
#' 
construct_col_schema <- function(roadmap) {
  
  col_schema <- roadmap$schema$col_schema
  vs_names <- roadmap$visit_sequence$visit_sequence
  
  built_col_schema <- purrr::map(
    .x = vs_names,
    .f = \(x) {
      # if original variable in col_schema
      
      if (x %in% names(col_schema)) {
        # use col_schema dtype
        return(col_schema[[x]])
        
      } else if (base::endsWith(x, "_NA")) {
        
        # else, is _NA indicator and needs an artificial col_schema
        return(
          list(
            "dtype" = "fct",
            "na_prop" = 0., 
            "levels" = c("missing value", "nonmissing value")
          )
        )
        
      }
    }
  )
  names(built_col_schema) <- vs_names
  
  return(built_col_schema)
  
}