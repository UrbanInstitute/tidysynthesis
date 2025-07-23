#' Construct a list of noise objects for synthesis
#'
#' @param roadmap A roadmap object
#' @param default_regression_noise A noise function for regression models
#' @param default_classification_noise A noise function for classification models
#' @param custom_noise A formatted list of noise functions
#'
#' @return A named list of noise
#' 
#' @export
#' 
construct_noise <- function(
    roadmap, 
    default_regression_noise = NULL,
    default_classification_noise = NULL,
    custom_noise = NULL
) {
  
  # check function inputs
  if (!is_roadmap(roadmap)) {
    
    stop("`roadmap` must be a roadmap object")
    
  }
  
  if (
    is.null(default_regression_noise) & 
    is.null(default_classification_noise) & 
    is.null(custom_noise)
  ) {
    
    warning("No noise specified, using default noise() object.")
    
  }
  
  if (is.null(default_regression_noise)) { 
  
    default_regression_noise <- noise(add_noise = FALSE, 
                                      mode = "regression")
      
  }
  
  if (is.null(default_classification_noise)) { 
    
    default_classification_noise <- noise(add_noise = FALSE, 
                                          mode = "classification")
    
  }
  
  # create vectors that we will use below
  visit_sequence <- roadmap[["visit_sequence"]][["visit_sequence"]]
  mode <- .extract_mode(roadmap)
  
  # validate inputs
  .validate_construct_inputs_optional(
    visit_sequence = visit_sequence,
    default_reg = default_regression_noise, 
    default_class = default_classification_noise, 
    custom_list = custom_noise,
    type_check_func = .is_noise,
    obj_name = "noise(s)"
  )
  
  # create list of default noise according to regression / classification
  synth_noise <- purrr::map(
    .x = mode, 
    .f = \(x) {
      if (x == "regression") { 
        return( default_regression_noise )
      } else { 
        return( default_classification_noise ) 
      }
    }
  )
  
  # add names to object
  names(synth_noise) <- visit_sequence
  
  # iterate through the variables and overwrite the default if an alternative
  # noise is specified in custom_noise
  for (var in visit_sequence) {
    
    # see if there is a custom noise
    custom_n <- NULL
    for (i in seq_along(custom_noise)) {
      
      if (var %in% custom_noise[[i]][["vars"]]) {
        
        custom_n <- custom_noise[[i]][["noise"]]
        
      }
      
    }
    
    # if custom noise, then replace everything with the custom noise
    if (!is.null(custom_n)) {
      
      synth_noise[[var]] <- custom_n
      
    }
    
  }
  
  # overwrite noise for outcome variables with no variation
  no_var_vars <- roadmap[["schema"]][["no_variation"]]
  
  no_var_vars <- names(no_var_vars)[unname(no_var_vars)]
  
  synth_noise <- purrr::modify_at(
    .x = synth_noise,
    .at = no_var_vars,
    .f = ~ "identity"
  )
  
  return(synth_noise)
  
}
