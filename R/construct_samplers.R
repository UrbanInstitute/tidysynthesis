#' Construct a list of samplers for synthesis
#'
#' @param roadmap A roadmap object
#' @param default_regression_sampler A sampler function for regression models
#' @param default_classification_sampler A sampler function for classification models
#' @param custom_samplers A formatted list of sampler functions
#'
#' @return A named list of samplers
#' 
#' @export
#' 
construct_samplers <- function(roadmap, 
                               default_regression_sampler = NULL,
                               default_classification_sampler = NULL,
                               custom_samplers = NULL) {
  
  # create vectors that we will use below
  if (!is_roadmap(roadmap)) {
    
    stop("`roadmap` must be a roadmap object")
    
  }
  
  visit_sequence <- roadmap[["visit_sequence"]][["visit_sequence"]]
  mode <- .extract_mode(roadmap)
  
  # validate inputs
  .validate_construct_inputs_required(
    visit_sequence = visit_sequence,
    mode = mode,
    default_reg = default_regression_sampler, 
    default_class = default_classification_sampler, 
    custom_list = custom_samplers,
    type_check_func = .is_sampler,
    obj_name = "sampler(s)"
  )
  
  # create list of default samplers according to regression / classification
  synth_samplers <- purrr::map(
    .x = mode, 
    .f = \(x) {
      if (x == "regression") { 
        return( default_regression_sampler )
      } else { 
        return( default_classification_sampler ) 
      }
    }
  )
  
  # add names to object
  names(synth_samplers) <- visit_sequence
  
  # iterate through the variables and overwrite the default if an alternative
  # sampler is specified in custom_samplers
  for (var in visit_sequence) {
    
    # see if there is a custom sampler
    custom_s <- NULL
    for (i in seq_along(custom_samplers)) {
      
      if (var %in% custom_samplers[[i]][["vars"]]) {
        
        custom_s <- custom_samplers[[i]][["sampler"]]
        
      }
      
    }
    
    # if custom sampler, then replace everything with the custom sampler
    if (!is.null(custom_s)) {
      
      synth_samplers[[var]] <- custom_s
      
    }
    
  }
  
  # overwrite samplers for outcome variables with no variation
  no_var_vars <- roadmap[["schema"]][["no_variation"]]
  
  no_var_vars <- names(no_var_vars)[unname(no_var_vars)]
  
  synth_samplers <- purrr::modify_at(
    .x = synth_samplers,
    .at = no_var_vars,
    .f = ~ "identity"
  )
  
  return(synth_samplers)
  
}
