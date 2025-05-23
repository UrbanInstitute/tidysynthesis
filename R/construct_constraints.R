#'
#' Construct a list of constraints that aligns with a visit sequence.
#' 
#' This set of constraints respects the `visit_sequence` ordering and properly
#' respects default and custom settings for different modes of constraint 
#' enforcement.
#' 
#' @param roadmap A `roadmap` S3 object
#' 
#' @return A named list of lists mapping variable names to their `constraint`
#' dataframes and `max_z` values
#' 
#' @export
#' 
construct_constraints <- function(roadmap) {
  
  vs_names <- roadmap$visit_sequence$visit_sequence
  constraints <- c(roadmap$constraints$constraints_num,
                   roadmap$constraints$constraints_cat)
  max_zs <- c(roadmap$constraints$max_z_num,
              roadmap$constraints$max_z_cat)
  
  # unpack roadmap constraints while respecting _NA variables
  built_constraints <- vector(mode = "list", length = length(vs_names)) %>%
    rlang::set_names(vs_names)
  
  # for each visit_sequence variable...
  for (n in vs_names) {
    
    # add constraints_df if specified
    if (!is.null(constraints[[n]])) {
      
      built_constraints[[n]][["constraints_df"]] <- constraints[[n]]
      
    }
    
    # add max_z if specified
    if (!is.null(max_zs[[n]])) {
      
      built_constraints[[n]][["max_z"]] <- max_zs[[n]]
      
    } else {
      
      built_constraints[[n]][["max_z"]] <- 0
      
    }
    
  }
  
  return(built_constraints)
  
}