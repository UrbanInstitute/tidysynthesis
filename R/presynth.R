
#'
#' Construct tidysynthesis workflows from a `roadmap` and `synth_spec`
#' 
#' @param roadmap A `roadmap` object
#' @param synth_spec A `synth_spec` object
#' 
#' @return A list of built models, recipes, samplers, etc.
#' 
#' @export 
#'
.construct_workflows <- function(roadmap, synth_spec) {
  
  # construct presynth components
  built_models <- construct_models(
    roadmap = roadmap, 
    default_regression_model = synth_spec[["default_regression_model"]],
    default_classification_model = synth_spec[["default_classification_model"]],
    custom_models = synth_spec[["custom_models"]]
  )
  
  built_recipes <- construct_recipes(
    roadmap = roadmap, 
    default_regression_steps = synth_spec[["default_regression_steps"]],
    default_classification_steps = synth_spec[["default_classification_steps"]],
    custom_steps = synth_spec[["custom_steps"]]
  )
  
  built_samplers <- construct_samplers(
    roadmap = roadmap, 
    default_regression_sampler = synth_spec[["default_regression_sampler"]],
    default_classification_sampler = synth_spec[["default_classification_sampler"]],
    custom_samplers = synth_spec[["custom_samplers"]]
  )

  built_noises <- construct_noise(
    roadmap = roadmap,
    default_regression_noise = synth_spec[["default_regression_noise"]], 
    default_classification_noise = synth_spec[["default_classification_noise"]],
    custom_noise = synth_spec[["custom_noise"]]
  )
  
  built_tuners <- construct_tuners(   
    roadmap = roadmap, 
    default_regression_tuner = synth_spec[["default_regression_tuner"]],
    default_classification_tuner = synth_spec[["default_classification_tuner"]],
    custom_tuners = synth_spec[["custom_tuners"]]
  )
  
  built_extractors <- construct_extractors(   
    roadmap = roadmap, 
    default_extractor = synth_spec[["default_extractor"]],
    custom_extractors = synth_spec[["custom_extractors"]]
  )
  
  built_constraints <- construct_constraints(roadmap = roadmap)
  
  built_col_schema <- construct_col_schema(roadmap = roadmap)
  
  # create results
  results <- list(
    built_models = built_models,
    built_recipes = built_recipes,
    built_samplers = built_samplers,
    built_noises = built_noises,
    built_tuners = built_tuners,
    built_extractors = built_extractors,
    built_constraints = built_constraints,
    built_col_schema = built_col_schema
  )
  
  return(results)
  
}



#' Create a presynth object
#'
#' @param roadmap A `roadmap` object from `roadmap()`.
#' @param synth_spec A `synth_spec` object from `synth_spec()`.
#' 
#' @return A `presynth` object.
#' 
#' @export
presynth <- function(roadmap,
                     synth_spec) {
  
  # create new presynth
  presynth <- new_presynth(roadmap = roadmap,
                           synth_spec = synth_spec)
  
  return(presynth)    
  
}

#'
#' `presynth` constructor
#' 
#' @param roadmap A `roadmap` object
#' @param synth_spec A `synth_spec` object
#' 
new_presynth <- function(roadmap,
                         synth_spec) {
  
  # test inputs
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`synth_spec` must be a synth_spec object" = { is_synth_spec(synth_spec) }
  )
  
  # first, enforce schema if needed
  if (roadmap[["schema"]][["enforce"]]) {
    
    roadmap <- enforce_schema(roadmap)
    
  }
  
  # create workflows 
  workflows <- .construct_workflows(roadmap, synth_spec)
  
  # create presynth
  presynth <- list(
    roadmap = roadmap,
    synth_spec = synth_spec,
    workflows = workflows
  )
  presynth <- structure(presynth, class = "presynth")
  
  # validate and return
  .validate_presynth(presynth)
  return(presynth)
  
}

#' 
#' Check if object is `presynth`
#' 
#' @param x Object
#' @return Logical 
#'  
#' @export 
#' 
is_presynth <- function(x) {
  inherits(x, "presynth")
}



#' 
#' `presynth` validator
#' 
#' Ensures compability between `synth_spec` and `roadmap` objects, raising 
#' an error if not.
#' 
#' @param presynth A `presynth` object
#' 
#' @return NULL
#' 
.validate_presynth <- function(presynth) {
  
  # validate underlying roadmap
  validate_roadmap(presynth$roadmap)
  
  # test component sizes
  expected_length <- length(
    presynth[["roadmap"]][["visit_sequence"]][["visit_sequence"]]
  )
  
  # note: R docs recommend against dynamically creating expression lists and 
  # passing them into `stopifnot()`, so there's known repetition here.
  stopifnot(
    "`built_models` has incorrect length" = {
      length(presynth[["workflows"]][["built_models"]]) == expected_length
    },
    "`built_recipes` has incorrect length" = {
      length(presynth[["workflows"]][["built_recipes"]]) == expected_length
    },
    "`built_samplers` has incorrect length" = {
      length(presynth[["workflows"]][["built_samplers"]]) == expected_length
    },
    "`built_noises` has incorrect length" = {
      length(presynth[["workflows"]][["built_noises"]]) == expected_length
    },
    "`built_tuners` has incorrect length" = {
      length(presynth[["workflows"]][["built_tuners"]]) == expected_length
    },
    "`built_extractors` has incorrect length" = {
      length(presynth[["workflows"]][["built_extractors"]]) == expected_length
    },
    "`built_constraints` has incorrect length" = {
      length(presynth[["workflows"]][["built_constraints"]]) == expected_length
    },
    "`built_col_schema` has incorrect length" = {
      length(presynth[["workflows"]][["built_col_schema"]]) == expected_length
    }
  )
  
  # test component names
  expected_names <- names(
    presynth[["roadmap"]][["visit_sequence"]][["visit_sequence"]]
  )
  
  stopifnot(
    "`built_models` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_models"]]) == expected_names)
    },
    "`built_recipes` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_recipes"]]) == expected_names)
    },
    "`built_samplers` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_samplers"]]) == expected_names)
    },
    "`built_noises` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_noises"]]) == expected_names)
    },
    "`built_tuners` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_tuners"]]) == expected_names)
    },
    "`built_extractors` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_extractors"]]) == expected_names)
    },
    "`built_constraints` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_constraints"]]) == expected_names)
    },
    "`built_col_schema` has incorrect names" = {
      all(names(presynth[["workflows"]][["built_col_schema"]]) == expected_names)
    }
  )
  
  # test that the models are at the right positions for variables with and without variation
  var_vars <- (presynth[["roadmap"]][["schema"]][["no_variation"]] == FALSE)
  var_names <-  presynth[["roadmap"]][["visit_sequence"]][["visit_sequence"]]
  
  var_mismatches <- purrr::map_lgl(
    .x = var_names,
    .f = \(x) {
      
      class(presynth[["roadmap"]][["conf_data"]][[x]]) != dplyr::case_when(
        presynth[["workflows"]][["built_models"]][[x]]$mode == "classification" ~ "factor", 
        presynth[["workflows"]][["built_models"]][[x]]$mode == "regression" ~ "numeric", 
        TRUE ~ class(presynth[["roadmap"]][["conf_data"]][[x]]))
      
    } 
  ) 
  
  if(sum(var_mismatches) > 0){
    
    stop(
      "Variable types in visit_sequence do not match model types in synth_algorithms\n",
      "  Problem variable(s): ", paste0(var_names[var_mismatches], collapse = ", ")
    )
    
  }
  
}

# print method for presynth objects
# 
#' @export
print.presynth <- function(x, ...) {
  
  print("Presynth \n")
  
  print(x$roadmap)
  print(x$synth_spec)
  
  invisible(x)
  
}

#'
#' Update `presynth` object
#' 
#' @param presynth A `presynth` object
#' @param roadmap An optional `roadmap` object
#' @param synth_spec An optional `synth_spec` object
#' 
#' @return A `presynth` object.
#' 
#' @export
#' 
update_presynth <- function(presynth, 
                            roadmap = NULL, 
                            synth_spec = NULL) {
  
  # type checking
  stopifnot(
    "`roadmap` must be a roadmap object, if specified" = {
      is.null(roadmap) | is_roadmap(roadmap)
    },
    "`synth_spec` must be a synth_spec object, if specified" = {
      is.null(synth_spec) | is_synth_spec(synth_spec)
    }
  )
  
  # check at least one non-NULL argument provided
  stopifnot(
    "Only one of either `roadmap` or `synth_spec` must be specified" = {
      !(is.null(roadmap) & is.null(synth_spec))
    }
  )
  
  # update objects and rebuild workflows
  if (!is.null(roadmap)) {
    
    presynth[["roadmap"]] <- roadmap
    
  }
  
  if (!is.null(synth_spec)) {
    
    presynth[["synth_spec"]] <- synth_spec
    
  }
  
  # next, enforce schema if needed
  if (presynth[["roadmap"]][["schema"]][["enforce"]]) {
    
    presynth[["roadmap"]] <- enforce_schema(presynth[["roadmap"]])
    
  }
  
  # next, reconstruct workflows
  presynth[["workflows"]] <- .construct_workflows(
    roadmap = presynth[["roadmap"]], 
    synth_spec = presynth[["synth_spec"]]
  )
  .validate_presynth(presynth)
  
  return(presynth)
  
}
