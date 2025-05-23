#' Construct a list of extractors for parsnip models
#'
#' @param roadmap A roadmap object
#' @param default_extractor An extractor from library(parsnip)
#' @param custom_extractors A formatted list of extractors
#'
#' @return A named list of extractors
#' 
#' @export
#' 
construct_extractors <- function(
    roadmap, 
    default_extractor = NULL,
    custom_extractors = NULL
) {
  
  # check function inputs ---------------------------------------------------
  
  if (!is_roadmap(roadmap)) {
    
    stop("`roadmap` must be a roadmap object")
    
  }
  
  # create a vector that we will use below
  visit_sequence <- roadmap[["visit_sequence"]][["visit_sequence"]]
  .validate_construct_inputs_optional(
    visit_sequence = visit_sequence,
    default_reg = default_extractor, 
    default_class = NULL, 
    custom_list = custom_extractors,
    type_check_func = .is_extractor,
    obj_name = "extractor(s)"
  )
  
  if (
    is.null(default_extractor) & 
    is.null(custom_extractors)
  ) {
    
    warning("No extractors specified, using default extractor.")
    
    return(
      purrr::map(purrr::set_names(visit_sequence), \(x) { NULL })
    )
    
  }  
  
  # construct extractors --------------------------------------------------------

  # create an empty list for the extractors
  extractors <- vector(mode = "list", length = length(visit_sequence))
  
  # add default extractor for all variables in the visit sequence
  for (i in seq_along(extractors)) {
    
    if (!is.null(default_extractor)) {
    
      extractors[[i]] <- default_extractor
    
    }
    
  }
  
  # add names to object
  names(extractors) <- visit_sequence
  
  # iterate through the variables and overwrite the default if an alternative
  # extractor is specified in custom_extractors
  for (var in visit_sequence) {
    
    # see if there is a custom extractor
    custom_extractor <- NULL
    for (i in seq_along(custom_extractors)) {
      
      if (var %in% custom_extractors[[i]][["vars"]]) {
        
        custom_extractor <- custom_extractors[[i]][["extractor"]]
        
      }
      
    }
    
    # if custom extractor, then replace everything with the custom extractor
    if (!is.null(custom_extractor)) {
      
      extractors[[var]] <- custom_extractor
      
    }
    
  }
  
  # overwrite extractors for outcome variables with no variation
  no_var_vars <- roadmap[["schema"]][["no_variation"]]
  
  no_var_vars <- names(no_var_vars)[unname(no_var_vars)]
  
  if (!is.null(no_var_vars)) {
    
    extractors <- purrr::modify_at(
      .x = extractors,
      .at = no_var_vars,
      .f = ~ "identity"
    )
    
  }
  
  return(extractors)
  
}
