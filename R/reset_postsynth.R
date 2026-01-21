
#' 
#' Filter a constraint dataframe to keep only selected variables
#' 
#' @param input_df constraint data.frame (numeric or categorical)
#' @param remaining_vars a character vector of variable names
#' 
#' @return Either a new constraint data.frame or NULL
#' 
#' @noRd
#' 
.filter_constraint_var <- function(input_df, remaining_vars) {
  
  if (!is.null(input_df)) {
    
    new_constraint_df <- input_df |> 
      dplyr::filter(.data[["var"]] %in% remaining_vars)
    
    if (nrow(new_constraint_df) == 0) {
      
      return(NULL)
      
    } else {
      
      return(new_constraint_df)
      
    }
    
  } else {
    
    return(NULL)
    
  }
  
}

#' 
#' Convert a incompletely synthesized `postsynth` into a new `roadmap` using 
#' the same `roadmap` settings from the incomplete synthesis.
#' 
#' @param postsynth A `postsynth` object generated with `keep_workflows == TRUE`
#' 
#' @return A `roadmap` object.
#' 
#' @export 
#' 
postsynth_to_roadmap <- function(postsynth) {
  
  # check for correct components
  stopifnot(is_postsynth(postsynth))
  
  stopifnot(
    "postsynth must be created with `keep_workflows == TRUE`" = {
      !is.null(postsynth$roadmap)
    }
  )
  
  # check if synthesis complete or not
  if (all(postsynth$roles == "synthesized")) {
    
    warning("Synthesis already completed, returning original roadmap.") 
    
    return(postsynth$roadmap)
    
  }
  
  # new starting data is existing partially completed synthetic data
  new_start_data <- postsynth$synthetic_data
  remaining_vars <- names(postsynth$roles[postsynth$roles == "unsynthesized"])
  
  # update schema to reflect new synthesis variables
  new_schema <- postsynth$roadmap$schema
  new_schema$synth_vars <- remaining_vars
  
  # update visit_sequence to retain original relative ordering
  new_vs <- visit_sequence(schema = new_schema)
  new_vs$visit_sequence <- remaining_vars # already in visit_sequence order
  new_vs$visit_method <- utils::tail(
    postsynth$roadmap$visit_sequence$visit_method,
    length(remaining_vars)
  )
  
  # filter constraints to remaining synthesized variables
  old_constraint_inputs <- postsynth$roadmap$constraints$inputs
  
  new_constraints_df_num <- .filter_constraint_var(
    input_df = old_constraint_inputs$input_constraints_df_num,
    remaining_vars = remaining_vars
  )
  
  new_constraints_df_cat <- .filter_constraint_var(
    input_df = old_constraint_inputs$input_constraints_df_cat,
    remaining_vars = remaining_vars
  )
  
  new_constraints <- constraints(
    schema = new_schema,
    constraints_df_num = new_constraints_df_num,
    constraints_df_cat = new_constraints_df_cat,
    max_z_num = old_constraint_inputs$input_max_z_num,
    max_z_cat = old_constraint_inputs$input_max_z_cat
  )
  
  # combine into new roadmap
  return(
    roadmap(
      conf_data = postsynth$roadmap$conf_data,
      start_data = new_start_data,
      schema = new_schema,
      visit_sequence = new_vs,
      constraints = new_constraints,
      replicates = postsynth$roadmap$replicates
    )
  )
  
}


#' 
#' Remove a custom variable name from a nested element list
#' 
#' @param custom_elms a nested list of arguments passed to `synth_spec` through
#' the `custom_*` arguments (ex: `custom_steps`)
#' @param varname character variable name to remove
#' 
#' @return Either a new custom element list or NULL
#' 
#' @noRd
#' 
.remove_custom_var <- function(custom_elms, 
                               varname) {
  
  if (is.null(custom_elms)) {
    
    return(NULL)
    
  }
  
  new_custom_elms <- list() 
  
  for (elm in custom_elms) {
    
    if (varname %in% elm[["vars"]]) {
      
      if (length(elm[["vars"]]) > 1) {
        
        new_elm <- elm
        new_elm[["vars"]] <- elm[["vars"]][elm[["vars"]] != varname]
        
        new_custom_elms <- append(new_custom_elms, list(new_elm))
        
      }
      
    } else {
      
      new_custom_elms <- append(new_custom_elms, list(elm))
      
    }
    
  }
  
  if (rlang::is_empty(new_custom_elms)) {
    
    return(NULL)
    
  } else {
    
    return(new_custom_elms)
    
  }
  
  
}

#' 
#' Remove multiple variable names from a nested element list
#' 
#' @param custom_elms a nested list of arguments passed to `synth_spec` through
#' the `custom_*` arguments (ex: `custom_steps`)
#' @param varname character vector of variable names to remove
#' 
#' @return Either a new custom element list or NULL
#' 
#' @noRd
#' 
.remove_custom_vars <- function(custom_elms, varnames) {
  
  # sequentially remove variable names
  result <- custom_elms 
  for (varname in varnames) {
    result <- .remove_custom_var(result, varname)
  }
  
  return(result)
  
}

#' 
#' Convert a incompletely synthesized `postsynth` into a new `synth_spec` using 
#' the same `synth_spec` settings from the incomplete synthesis.
#' 
#' @param postsynth A `postsynth` object generated with `keep_workflows == TRUE`
#' 
#' @return A `synth_spec` object.
#' 
#' @export 
#' 
postsynth_to_synth_spec <- function(postsynth) {
  
  stopifnot(is_postsynth(postsynth))
  
  stopifnot(
    "postsynth must be created with `keep_workflows == TRUE`" = {
      !is.null(postsynth$synth_spec)
    }
  )
  
  ss <- postsynth$synth_spec
  
  if (all(postsynth$roles == "synthesized")) {
    
    warning("Synthesis already completed, returning original synth_spec.") 
    
    return(ss)
    
  }
  
  remove_vars <- names(postsynth$roles[postsynth$roles == "synthesized"])
  
  new_synth_spec <- synth_spec(
    default_regression_model = ss$default_regression_model,
    default_classification_model = ss$default_classification_model,
    custom_models = .remove_custom_vars(
      custom_elms = ss$custom_models,
      varnames = remove_vars
    ),
    default_regression_steps = ss$default_regression_steps,
    default_classification_steps = ss$default_classification_steps,
    custom_steps = .remove_custom_vars(
      custom_elms = ss$custom_steps,
      varnames = remove_vars
    ),
    default_regression_sampler = ss$default_regression_sampler,
    default_classification_sampler = ss$default_classification_sampler,
    custom_samplers = .remove_custom_vars(
      custom_elms = ss$custom_samplers,
      varnames = remove_vars
    ),
    default_regression_noise = ss$default_regression_noise,
    default_classification_noise = ss$default_classification_noise,
    custom_noise = .remove_custom_vars(
      custom_elms = ss$custom_noise,
      varnames = remove_vars
    ),
    default_regression_tuner = ss$default_regression_tuner,
    default_classification_tuner = ss$default_classification_tuner,
    custom_tuners = .remove_custom_vars(
      custom_elms = ss$custom_tuners,
      varnames = remove_vars
    ),
    default_extractor = ss$default_extractor,
    custom_extractors = .remove_custom_vars(
      custom_elms = ss$custom_extractors,
      varnames = remove_vars
    ),
    invert_transformations = ss$invert_transformations,
    enforce_na = ss$enforce_na
  )
  
  return(new_synth_spec)
  
}

