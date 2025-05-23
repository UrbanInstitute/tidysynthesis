#' 
#' Helper for functional equivalent of `identical()` for functions
#' @param x function
#' @param y function
#' @return Logical
#' 
.identical_funcs <- function(x, y) { 
  
  stopifnot(is.function(x) & is.function(y))
  return(identical(all.equal(x, y), TRUE))
  
} 

#'
#' Inspections for `synth_spec` components
#' 
#' @param z Object
#' @return Boolean if matches class type
#' @name synth_spec_is_component
#'
NULL
#> NULL

#'
#' @rdname synth_spec_is_component
#' 
.is_model <- function(z) { "model_spec" %in% class(z) }

# todo: better inspection
#'
#' @rdname synth_spec_is_component
#' 
.is_recipe <- function(z) { is.function(z) } 

# todo: better inspection
#'
#' @rdname synth_spec_is_component
#' 
.is_steps <- function(z) { is.function(z) } 

# todo: replace with new sampler object
#'
#' @rdname synth_spec_is_component
#' 
.is_sampler <- function(z) { is.function(z) } 

# todo: better inspection
#'
#' @rdname synth_spec_is_component
#' 
.is_tuner <- function(z) { "list" %in% class(z) | all(is.na(z)) }

# todo: replace with new noise object
#'
#' @rdname synth_spec_is_component
#' 
.is_noise <- function(z) { is_noise(z) } 

# todo: better inspection
#'
#' @rdname synth_spec_is_component
#' 
.is_extractor <- function(z) { is.function(z) }

#' constant mapping between component names and inspections
.name_to_inspect <- list(
  "model" = .is_model,
  "recipe" = .is_recipe,
  "steps" = .is_steps,
  "sampler" = .is_sampler,
  "noise" = .is_noise,
  "tuner" = .is_tuner,
  "extractor" = .is_extractor
)

#' 
#' Validate custom component specification in `synth_spec`
#' 
#' Raises error if custom component improperly specified for `custom_*` argument.
#' 
#' @param custom_components A named list of lists
#' @param component_name String, one of the names in `names(.name_to_inspect)`
#' 
.validate_custom_components <- function(custom_components, component_name) {
  
  # type checking for list structure
  list_checks <- purrr::map_lgl(
    .x = custom_components, 
    .f = \(x) { "list" %in% class(x) }
  )
  
  if (!all(list_checks)) {
    
    stop("Some custom ", component_name, " elements are not lists.")
    
  }
  
  list_name_checks <- purrr::map_lgl(
    .x = custom_components, 
    .f = \(x) { setequal(names(x), c("vars", component_name)) } 
  )
  
  if (!all(list_name_checks)) {
    
    stop("Some custom ", component_name, " elements are missing the two required 
         sublist names, 'vars' and '", component_name, "'")
    
  }
  
  # type checking for individual elements
  list_type_checks <- purrr::map_lgl(
    .x = custom_components, 
    .f = \(x) { .name_to_inspect[[component_name]](x[[component_name]]) } 
  )
  
  if (!all(list_type_checks)) {
    
    stop("Some custom ", component_name, " elements have incorrect type.")
    
  }
  
  list_type_checks_varname <- purrr::map_lgl(
    .x = custom_components, 
    .f = \(x) { is.character(x[["vars"]]) } 
  )

  if (!all(list_type_checks_varname)) {
    
    stop("Some custom ", component_name, " variable names are not strings.")
    
  }
  
}

#' 
#' Map model object to default sampler function
#' 
#' @param model A `parsnip::model_spec` object
#' 
#' @return A sampler function
#'
#' 
.map_model_to_default_sampler <- function(model) {
  
  stopifnot(.is_model(model))
  engine <- model[["engine"]]
  
  valid_engines = c("rpart", "ranger", "lm", "glm") 
  
  if (!(engine %in% valid_engines)) {
    
    stop("Unrecognized engine: ", engine, ". Please either supply
         a specific sampler or use a recognized engine:",
         paste0(valid_engines, collapse=", "))
    
  }
  
  # have to do this manually because dplyr::case_when does not allow
  # recycling size 1 functions 
  if (engine == "rpart") {
    
    return(sample_rpart)
    
  } else if (engine == "ranger") {
    
    return(sample_ranger) 
    
  } else if (engine == "lm") {
    
    return(sample_lm)
    
  } else if (engine == "glm") {
    
    return(sample_glm)
    
  }
    
}


#' 
#' Update custom_components 
#' 
#' @param synth_spec A `synth_spec` object
#' @param component_name String, one of the names in `names(.name_to_inspect)`
#' @param custom_name String, name for `custom_<>` components
#' @param ... Optional named lists with two elements, `vars` and `<component_name>`
#' mapping variable names to their corresponding `synth_spec` objects 
#' 
.update_custom_components <- function(
    synth_spec, 
    component_name, 
    custom_name,
    ...) {
  
  stopifnot(
    "`synth_spec` must be a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  # construct and validate arguments
  custom_comps <- list(...)
  
  .validate_custom_components(
    custom_components = custom_comps, 
    component_name = component_name
  )
  
  # extract list of all custom variables
  custom_vars <- purrr::imap(
    .x = synth_spec[[custom_name]],
    .f = \(x, idx) { 
      stats::setNames(rep(list(idx), length(x[["vars"]])), x[["vars"]])
    }
  ) %>% unlist()
  
  # for each custom entry
  for (i in seq_along(custom_comps)) {
    
    candidate_vars <- custom_comps[[i]][["vars"]]
    
    # append new entry for candidate_vars
    to_append <- list("vars" = candidate_vars) 
    to_append[[component_name]] <- custom_comps[[i]][[component_name]]
    
    synth_spec[[custom_name]] <- base::append(
      synth_spec[[custom_name]], 
      list(to_append)
    )
    
    # delete existing entries for repeated variables
    repeat_vars <- intersect(candidate_vars, names(custom_vars))
    
    for (repeat_var in repeat_vars) {
      
      repeat_ix <- custom_vars[[repeat_var]]
      synth_spec[[custom_name]][[repeat_ix]][["vars"]] <- setdiff(
        synth_spec[[custom_name]][[repeat_ix]][["vars"]], repeat_var
      )
      
    }
    
  }
  
  return(synth_spec)
  
}

#'
#'  Convert list of steps to a `recipe::recipe`
#'
