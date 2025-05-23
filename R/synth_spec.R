#' 
#' Create a `synth_spec` object
#' 
#' The `synth_spec` object holds specifications for modeling and sampling components
#' for sequential synthetic data generation. Each component has an associated
#' `construct_*` function called when creating a `presynth` object. 
#' 
#' @param default_regression_model A `model_spec` object from `library(parsnip)`
#' for use in regression models.
#' @param default_classification_model A `model_spec` object from `library(parsnip)`
#' for use in classification models.
#' @param custom_models A list of named lists each with two elements: 
#' `vars` for variable names, and `model` for their associated model.
#' from `library(parsnip)`. 
#' @param default_regression_steps A list of `recipe::step_` function(s) 
#' from `library(recipes)` for use in regression models.
#' @param default_classification_steps A list of `recipe::step_` function(s) 
#' from `library(recipes)` for use in classification models.
#' @param custom_steps A list of named lists each with two elements: 
#' `vars` for variable names, and `steps` for their associated recipe.
#' @param default_regression_sampler A sampling function for drawing
#' new values from regression models.
#' @param default_classification_sampler A sampling function for drawing
#' new values from classification models.
#' @param custom_samplers A list of named lists each with two elements: 
#' `vars` for variable names, and `sampler` for their associated sampler
#' @param default_regression_noise A noise function for adding noise to numeric 
#' values.
#' @param default_classification_noise A noise function for adding noise to 
#' classification values.
#' @param custom_noise A list of named lists each with two elements:
#' `vars` for variable names, and `noise` for their associated noise
#' @param default_regression_tuner A `tuner` from `library(tune)` 
#' for use in regression models.
#' @param default_classification_tuner A `tuner` from `library(tune)` 
#' for use in classification models.
#' @param custom_tuners A list of named lists each with two elements: 
#' `vars` for variable names, and `tuner` for their associated tuner
#' @param default_extractor An optional method for extracting workflows or 
#' extracts from workflows.
#' @param custom_extractors A list of named lists each with two elements: 
#' `vars` for variable names, and `extractor` for their associated extractor
#' @param invert_transformations A Boolean for if outcome variable 
#' transformations applied through recipes should be inverted during synthesis.
#' recipes need ids that begin with "outcome".
#' @param enforce_na A Boolean for if NA values should be added into the 
#' synthetic data with `enforce_na()` during synthesis. An alternative approach 
#' is to add the NA values after synthesis
#' 
#' @examples
#' rpart_mod <- parsnip::decision_tree() %>%
#'   parsnip::set_engine(engine = "rpart") %>%
#'   parsnip::set_mode(mode = "regression")
#' 
#' lm_mod <- parsnip::linear_reg() %>% 
#'   parsnip::set_engine("lm") %>%
#'   parsnip::set_mode(mode = "regression")
#' 
#' step1 <- function(x) {
#'  x %>%
#'    recipes::step_center(recipes::all_predictors(), id = "center")
#' }
#'
#' step2 <- function(x) {
#'   x %>%
#'     recipes::step_scale(recipes::all_predictors(), id = "scale")
#' }
#' 
#' step3 <- function(x) { x %>% step1 %>% step2 }
#' 
#' 
#' synth_spec(
#'  default_regression_model = rpart_mod,
#'  custom_models = list(
#'    list("vars" = c("var1", "var2"), 
#'         "model" = lm_mod)
#'  ),
#'  default_regression_steps = step1,
#'  custom_steps = list(
#'    list("vars" = c("var2", "var3"),
#'         "steps" = step2),
#'    list("vars" = c("var4"), 
#'         "steps" = step3)
#'  ),
#'  default_regression_sampler = sample_rpart,
#'  custom_samplers = list(
#'    list("vars" = c("var1", "var2"), 
#'         "sampler" = sample_lm)
#'  )
#' )
#' 
#' @export
#' 
synth_spec <- function(
    default_regression_model = NULL,
    default_classification_model = NULL,
    custom_models = NULL,
    default_regression_steps = NULL,
    default_classification_steps = NULL,
    custom_steps = NULL,
    default_regression_sampler = NULL,
    default_classification_sampler = NULL,
    custom_samplers = NULL,
    default_regression_noise = NULL,
    default_classification_noise = NULL,
    custom_noise = NULL,    
    default_regression_tuner = NULL,
    default_classification_tuner = NULL,
    custom_tuners = NULL,
    default_extractor = NULL,
    custom_extractors = NULL,
    invert_transformations = TRUE,
    enforce_na = TRUE) {
  
  # model type checking 
  stopifnot(
    "`default_regression_model` must be a parsnip model_spec, 
    if specified" = {
      is.null(default_regression_model) | .is_model(default_regression_model)
    }
  )
  
  stopifnot(
    "`default_classification_model` must be a parsnip model_spec, 
    if specified" = {
      is.null(default_classification_model) | 
        .is_model(default_classification_model)
    }
  )
  
  if (!is.null(custom_models)) {
    
    .validate_custom_components(custom_models, "model")
    
  }
  
  # recipe type checking 
  stopifnot(
    "`default_regression_steps` must be a function, if specified" = {
      is.null(default_regression_steps) | .is_steps(default_regression_steps)
    }
  )
  
  stopifnot(
    "`default_classification_steps` must be a function, if specified" = {
      is.null(default_classification_steps) | 
        .is_steps(default_classification_steps)
    }
  )
  
  if (!is.null(custom_steps)) {
    
    .validate_custom_components(custom_steps, "steps")
    
  }
  
  # sampler type checking 
  stopifnot(
    "`default_regression_sampler` must be a function, if specified" = {
      is.null(default_regression_sampler) | 
        .is_sampler(default_regression_sampler)
    }
  )
  
  
  stopifnot(
    "`default_classification_sampler` must be a function, if specified" = {
      is.null(default_classification_sampler) | 
        .is_sampler(default_classification_sampler)
    }
  )
  
  if (!is.null(custom_samplers)) {
    
    .validate_custom_components(custom_samplers, "sampler")
    
  }

  # noise type checking 
  stopifnot(
    "`default_regression_noise` must be a noise object, if specified" = {
      is.null(default_regression_noise) | .is_noise(default_regression_noise)
    }
  )
  
  stopifnot(
    "`default_classification_noise` must be a noise object, if specified" = {
      is.null(default_classification_noise) | 
        .is_noise(default_classification_noise)
    }
  )
  
  if (!is.null(custom_noise)) {
    
    .validate_custom_components(custom_noise, "noise")
    
  }
  
  # tuner type checking  
  stopifnot(
    "`default_regression_tuner` must be a list, if specified" = {
      is.null(default_regression_tuner) | .is_tuner(default_regression_tuner)
    }
  )
  
  stopifnot(
    "`default_classification_tuner` must be a list, if specified" = {
      is.null(default_classification_tuner) | 
        .is_tuner(default_classification_tuner)
    }
  )
  
  if (!is.null(custom_tuners)) {
    
    .validate_custom_components(custom_tuners, "tuner")
    
  }
  
  # extractor type checking
  stopifnot(
    "`default extractor` must be a function, if specified" = {
      is.null(default_extractor) | .is_extractor(default_extractor)
    }
  )
  
  if (!is.null(custom_extractors)) {
    
    .validate_custom_components(custom_extractors, "extractor")
    
  }
  
  stopifnot(
    "`invert_transformations` must be logical" = {
      is.logical(invert_transformations) 
    }
  )
  stopifnot("`enforce_na` must be logical" = { is.logical(enforce_na) } )
  
  synth_spec <- list(
    default_regression_model = default_regression_model,
    default_classification_model = default_classification_model,
    custom_models = custom_models,
    default_regression_steps = default_regression_steps,
    default_classification_steps = default_classification_steps,
    custom_steps = custom_steps,
    default_regression_sampler = default_regression_sampler,
    default_classification_sampler = default_classification_sampler,
    custom_samplers = custom_samplers,
    default_regression_noise = default_regression_noise,
    default_classification_noise = default_classification_noise,
    custom_noise = custom_noise,
    default_regression_tuner = default_regression_tuner,
    default_classification_tuner = default_classification_tuner,
    custom_tuners = custom_tuners,
    default_extractor = default_extractor,
    custom_extractors = custom_extractors,
    invert_transformations = invert_transformations,
    enforce_na = enforce_na
  )
  
  synth_spec <- structure(synth_spec, class = "synth_spec")
  
  return(synth_spec)
  
}

#' 
#' Check if object is `synth_spec`
#' 
#' @param z Object
#' @return Logical 
#'  
#' @export 
#' 
is_synth_spec <- function(z) {
  
  return("synth_spec" %in% class(z))
  
}

#' @export 
print.synth_spec <- function(x, ...) {
  
  cat("Synthesis specification with user-specified components: \n")
  
  components <- c(
    "default_regression_model",
    "default_classification_model",
    "custom_models",
    "default_regression_steps",
    "default_classification_steps",
    "custom_steps",
    "default_regression_sampler",
    "default_classification_sampler",
    "custom_samplers",
    "default_regression_noise",
    "default_classification_noise",
    "custom_noise",
    "default_regression_tuner",
    "default_classification_tuner",
    "custom_tuners",
    "default_extractor",
    "custom_extractors"
  )
  
  for (component in components) {
    
    if (!is.null(x[[component]])) {
      
      cat(paste0("* ", component, "\n"))
      
    }
    
  }
  
  invisible(x)
  
}

#' Tidy API calls ----------------------------------------------------------

#' 
#' Update non-custom `synth_spec` arguments
#' 
#' @param synth_spec A `synth_spec` object
#' @param ... Optional named keywords in `synth_spec`, with the exception of
#'any `custom_*`arguments
#' 
#' @return A `synth_spec`
#' 
#' @export 
#' 
update_synth_spec <- function(synth_spec, ...) {
  
  # check input names
  kwargs <- list(...)
  valid_args <- c(
    "default_regression_model",
    "default_classification_model",
    "default_regression_steps",
    "default_classification_steps",
    "default_regression_sampler",
    "default_classification_sampler",
    "default_regression_noise",
    "default_classification_noise",
    "default_regression_tuner",
    "default_classification_tuner",
    "default_extractor",
    "invert_transformations",
    "enforce_na"
  )
  
  mismatches <- setdiff(names(kwargs), valid_args)
  
  if (length(mismatches) >= 1) {
    
    # if synth_spec key corresponds to custom API function, warning
    if (any(stringr::str_starts(mismatches, "custom_"))) {
      
      warning("Please use the *_custom_* API calls to update custom
              synth_spec components")
      
    }
    
    # raise error regardless of warninging
    stop("Unexpected argument(s) to update_synth_spec(): ", 
         paste0(mismatches, collapse = ", "))
    
  } 
  
  for (n in names(kwargs)) {
    
    # type checking 
    if (n %in% c("default_regression_model", 
                 "default_classification_model")) {
      
      stopifnot(
        "Default model parameter must be a parsnip model_spec" = {
          .is_model(kwargs[[n]])
        }
      )
      
    } else if (n %in% c("default_regression_steps", 
                        "default_classification_steps")) {
      
      stopifnot(
        "Default steps parameter must be a function" = {
          .is_recipe(kwargs[[n]])
        }
      )
      
    } else if (n %in% c("default_regression_sampler", 
                        "default_classification_sampler")) {
      
      stopifnot(
        "Default sampler parameter must be a function" = {
          .is_sampler(kwargs[[n]])
        }
      )
      
    } else if (n %in% c("default_regression_noise", 
                        "default_classification_noise")) {
      
      stopifnot(
        "Default noise parameter must be a noise object" = {
          .is_noise(kwargs[[n]])
        }
      )
      
    } else if (n %in% c("default_regression_tuner", 
                        "default_classification_tuner")) {
      
      stopifnot(
        "Default tuner parameter must be a list" = {
          .is_tuner(kwargs[[n]])
        }
      )
      
    } else if (n %in% c("default_extractor")) {
      
      stopifnot(
        "Default extractor parameter must be a function" = {
          .is_extractor(kwargs[[n]])
        }
      )
      
    } else if (n %in% c("invert_transformations", "enforce_na")) {
      
      stopifnot(
        "`invert_transformations` and `enforce_na` must be logical" = {
          is.logical(kwargs[[n]])
        } 
      )
      
    }
    
    # update synth_spec and return
    synth_spec[[n]] <- kwargs[[n]]
    
  }
  
  return(synth_spec)
  
}


#' 
#' Add, update, or remove custom models from a `synth_spec` object
#'
#' @param synth_spec A `synth_spec` object
#' @param ... Optional named lists with two elements, `vars` and `model`,
#' mapping variable names to `model_spec` objects from `library(parsnip)`. 
#' 
#' @return A new `synth_spec` object.
#'
#' @name synth_spec_model_api
#'
NULL
#> NULL

#'
#' @rdname synth_spec_model_api
#' @export 
#' 
add_custom_models <- function(synth_spec, ...) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  # construct and validate arguments
  custom_models <- list(...)
  .validate_custom_components(
    custom_components = custom_models, 
    component_name = "model"
  )
  
  # update custom_models
  synth_spec[["custom_models"]] <- custom_models
  
  return(synth_spec)
  
}

#'
#' @rdname synth_spec_model_api
#' @export 
#' 
update_custom_models <- function(synth_spec, ...) {
  
  return(
    .update_custom_components(
      synth_spec = synth_spec,
      component_name = "model",
      custom_name = "custom_models",
      ...
    )
  )
  
}

#'
#' @rdname synth_spec_model_api
#' @export 
#' 
remove_custom_models <- function(synth_spec) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  if (is.null(synth_spec[["default_regression_model"]]) &
      is.null(synth_spec[["default_classification_model"]])) {
    
    message("No default model(s) specified; using default CART settings.")
    
  }
  
  synth_spec[["custom_models"]] <- NULL
  
  return(synth_spec)
  
}

#' 
#' Add, update, or remove recipe recipes from a `synth_spec` object
#'
#' @param synth_spec A `synth_spec` object
#' @param ... Optional named arguments mapping variables to lists of 
#' `recipe::recipe_` function(s) from `library(recipes)`.
#' 
#' @return A new `synth_spec` object.
#'
#' @name synth_spec_recipes_api
#'
NULL
#> NULL

#'
#' @rdname synth_spec_recipes_api
#' @export 
#' 
add_custom_steps <- function(synth_spec, ...) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  # construct and validate arguments
  custom_steps <- list(...)
  .validate_custom_components(
    custom_components = custom_steps, 
    component_name = "steps"
  )
  
  # extract list of all custom variables
  synth_spec[["custom_steps"]] <- custom_steps
  
  return(synth_spec)
  
}

#'
#' @rdname synth_spec_recipes_api
#' @export 
#' 
update_custom_steps <- function(synth_spec, ...) {
  
  return(
    .update_custom_components(
      synth_spec = synth_spec,
      component_name = "steps",
      custom_name = "custom_steps",
      ...
    )
  )
  
}

#'
#' @rdname synth_spec_recipes_api
#' @export 
#' 
remove_custom_steps <- function(synth_spec) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  synth_spec[["custom_steps"]] <- NULL
  
  return(synth_spec)
  
}

#' 
#' Add, update, or remove samplers from a `synth_spec` object
#'
#' @param synth_spec A `synth_spec` object
#' @param ... Optional named lists with two elements, `vars` and `sampler`,
#' mapping variable names to samplers.
#'
#' @return A new `synth_spec` object.
#'
#' @name synth_spec_sampler_api
#'
NULL
#> NULL

#'
#' @rdname synth_spec_sampler_api
#' @export 
#' 
add_custom_samplers <- function(synth_spec, ...) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  custom_samplers <- list(...)
  .validate_custom_components(
    custom_components = custom_samplers, 
    component_name = "sampler"
  )
  
  synth_spec[["custom_samplers"]] <- custom_samplers
  
  return(synth_spec)
  
}

#'
#' @rdname synth_spec_sampler_api
#' @export 
#'  
update_custom_samplers <- function(synth_spec, ...) {
  
  return(
    .update_custom_components(
      synth_spec = synth_spec,
      component_name = "sampler",
      custom_name = "custom_samplers",
      ...
    )
  )
  
}

#'
#' @rdname synth_spec_sampler_api
#' @export 
#' 
remove_custom_samplers <- function(synth_spec) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  synth_spec[["custom_samplers"]] <- NULL
  
  return(synth_spec)
  
}


#' 
#' Add, update, or remove noise from a `synth_spec` object
#'
#' @param synth_spec A `synth_spec` object
#' @param ... Optional named lists with two elements, `vars` and `noise`,
#' mapping variable names to samplers.
#'
#' @return A new `synth_spec` object.
#'
#' @name synth_spec_noise_api
#'
NULL
#> NULL

#'
#' @rdname synth_spec_sampler_api
#' @export 
#' 
add_custom_noise <- function(synth_spec, ...) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  custom_noise <- list(...)
  .validate_custom_components(
    custom_components = custom_noise, 
    component_name = "noise"
  )
  
  synth_spec[["custom_noise"]] <- custom_noise
  
  return(synth_spec)
  
}

#'
#' @rdname synth_spec_sampler_api
#' @export 
#'  
update_custom_noise <- function(synth_spec, ...) {
  
  return(
    .update_custom_components(
      synth_spec = synth_spec,
      component_name = "noise",
      custom_name = "custom_noise",
      ...
    )
  )
  
}

#'
#' @rdname synth_spec_sampler_api
#' @export 
#' 
remove_custom_noise <- function(synth_spec) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  synth_spec[["custom_noise"]] <- NULL
  
  return(synth_spec)
  
}


#' 
#' Add, update, or remove tuners from a `synth_spec` object
#'
#' @param synth_spec A `synth_spec` object
#' @param ... Optional named lists with two elements, `vars` and `tuner`,
#' mapping variable names to tuners.
#'
#' @return A new `synth_spec` object.
#'
#' @name synth_spec_tuner_api
#'
NULL
#> NULL

#'
#' @rdname synth_spec_tuner_api
#' @export 
#' 
add_custom_tuners <- function(synth_spec, ...) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  custom_tuners <- list(...)
  .validate_custom_components(
    custom_components = custom_tuners, 
    component_name = "tuner"
  )
  
  synth_spec[["custom_tuners"]] <- custom_tuners
  
  return(synth_spec)
  
}

#'
#' @rdname synth_spec_tuner_api
#' @export 
#'  
update_custom_tuners <- function(synth_spec, ...) {
  
  return(
    .update_custom_components(
      synth_spec = synth_spec,
      component_name = "tuner",
      custom_name = "custom_tuners",
      ...
    )
  )
  
}

#'
#' @rdname synth_spec_tuner_api
#' @export 
#' 
remove_custom_tuners <- function(synth_spec) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  synth_spec[["custom_tuners"]] <- NULL
  
  return(synth_spec)
  
}



#' 
#' Add, update, or remove extractors from a `synth_spec` object
#'
#' @param synth_spec A `synth_spec` object
#' @param ... Optional named lists with two elements, `vars` and `extractor`,
#' mapping variable names to extractors.
#'
#' @return A new `synth_spec` object.
#'
#' @name synth_spec_extractor_api
#'
NULL
#> NULL

#'
#' @rdname synth_spec_extractor_api
#' @export 
#' 
add_custom_extractors <- function(synth_spec, ...) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  custom_extractors <- list(...)
  .validate_custom_components(
    custom_components = custom_extractors, 
    component_name = "extractor"
  )
  
  synth_spec[["custom_extractors"]] <- custom_extractors
  
  return(synth_spec)
  
}

#'
#' @rdname synth_spec_extractor_api
#' @export 
#'  
update_custom_extractors <- function(synth_spec, ...) {
  
  return(
    .update_custom_components(
      synth_spec = synth_spec,
      component_name = "extractor",
      custom_name = "custom_extractors",
      ...
    )
  )
  
}

#'
#' @rdname synth_spec_extractor_api
#' @export 
#' 
remove_custom_extractors <- function(synth_spec) {
  
  stopifnot(
    "`synth_spec` is not a synth_spec object" = { is_synth_spec(synth_spec) } 
  )
  
  synth_spec[["custom_extractors"]] <- NULL
  
  return(synth_spec)
  
}

