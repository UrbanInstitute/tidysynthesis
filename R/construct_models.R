#' Construct a list of models for synthesis
#'
#' @param roadmap A roadmap object
#' @param default_regression_model A `parsnip` model object used for 
#' regression in numeric outcome variables
#' @param default_classification_model A `parsnip` model object used for 
#' classification in categorical outcome variables
#' @param custom_models A formatted list with `parsnip` model objects explicitly
#' paired with every variable in the `visit_sequence`
#'
#' @return A named list of models
#' 
#' @export
#' 
construct_models <- function(
    roadmap, 
    default_regression_model = NULL, 
    default_classification_model = NULL,
    custom_models = NULL
) {
  
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
    default_reg = default_regression_model, 
    default_class = default_classification_model, 
    custom_list = custom_models,
    type_check_func = .is_model,
    obj_name = "model(s)"
  )

  # construct models --------------------------------------------------------

  # create a list of default models where the default depends on if the model 
  # is a regression model or a classification model
  synth_models <- purrr::map(
    .x = mode, 
    .f = ~ if (.x == "regression") { default_regression_model } else { default_classification_model }
  )
  
  # add names to object
  names(synth_models) <- visit_sequence
  
  # iterate through the variables and overwrite the default if an alternative
  # model is specified in custom_models
  for (var in visit_sequence) {
  
    # see if there is a custom model
    custom_model <- NULL
    for (i in seq_along(custom_models)) {
      
      if (var %in% custom_models[[i]][["vars"]]) {
        
        custom_model <- custom_models[[i]][["model"]]
        
      }
      
    }
    
    # if custom model, then replace everything with the custom model
    if (!is.null(custom_model)) {
    
      synth_models[[var]] <- custom_model
    
    }
    
  }
  
  # overwrite models for outcome variables with no variation
  identity <- list(
    args = NULL,
    eng_args = NULL,
    mode = "identity",
    use_specified_mode = TRUE,
    method = NULL,
    engine = "identity",
    user_specified_engine = TRUE
  )
  
  no_var_vars <- roadmap[["schema"]][["no_variation"]]
  
  no_var_vars <- names(no_var_vars)[unname(no_var_vars)]
  
  if (!is.null(no_var_vars)) { 
    
    synth_models <- purrr::modify_at(
      .x = synth_models,
      .at = no_var_vars,
      .f = ~ identity
    )
    
  }
  
  return(synth_models)
  
}
