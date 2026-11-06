#' Pull the outcome variable name from a recipe
#'
#' @param recipe A recipe from `library(recipes)`.
#'
#' @return A string
#' @noRd
#'
.outcome_var <- function(recipe) {
  
  outcome_var <- recipe[["var_info"]][["variable"]][[
    which(recipe$var_info$role == "outcome")
  ]]
  
  return(outcome_var)
  
}

#' Subset confidential data to observations usable for modeling
#' 
#' Numeric outcome variables with missing values are modeled only on the 
#' observations with non-missing outcomes.
#'
#' @param conf_data A data frame of confidential data
#' @param col_schema A list of schematic information about the variable.
#' @param outcome_var A string with the outcome variable name.
#'
#' @return A data frame
#' @noRd
#'
.conf_model_data <- function(conf_data, col_schema, outcome_var) {
  
  if (col_schema[["na_prop"]] > 0 & col_schema[["dtype"]] == "dbl") {
    
    conf_model_data <- conf_data[!is.na(conf_data[[outcome_var]]), ]
    
  } else {
    
    conf_model_data <- conf_data
    
  }
  
  return(conf_model_data)
  
}

#' Fit the model for the jth variable
#'
#' @param conf_data A data frame for estimating the predictive model
#' @param col_schema A list of schematic information about each variable.
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param recipe A recipe from `library(recipes)`.
#' @param tuner A `tuner` object for hyperparameter tuning. Hyperparameter 
#' tuning is skipped if `tuner = NULL`.
#' @param extractor A method for extracting workflows or extracts from 
#' workflows. `NULL` will extract no information. `"workflow"` will extract the
#' entire workflow. `extract_*()` will use extract functions from 
#' `library(workflows)` to extract information.
#' @param p A function for tracking progress created with progressr
#'
#' @return A list with the fitted `workflow` (`"identity"` for identity 
#' models), the extraction, and the time spent fitting
#' @noRd
#' 
.fit_j <- function(conf_data, 
                   col_schema,
                   model,
                   recipe,
                   tuner,
                   extractor,
                   p = function() { NULL }) {
  
  p()
  
  # start jth variable fitting time
  fit_start_time <- Sys.time()
  
  # pull outcome var name
  outcome_var <- .outcome_var(recipe)
  
  # identity models have nothing to fit
  if (model[["mode"]] == "identity") {
    
    jth_fit_time <- as.numeric(Sys.time() - fit_start_time, units = "secs")
    
    return(
      list(
        fitted = "identity",
        extraction = "identity",
        jth_fit_time = jth_fit_time
      )
    )
    
  }
  
  # subset the data to observations with non-missing values for numeric outcome
  # variables
  conf_model_data <- .conf_model_data(
    conf_data = conf_data,
    col_schema = col_schema,
    outcome_var = outcome_var
  )
  
  # create a workflow with the model and recipe
  model_wf <- workflows::workflow() |>
    workflows::add_model(spec = model) |>
    workflows::add_recipe(recipe = recipe)
  
  if (is.null(tuner)) {
    
    # estimate the specified model using the confidential data
    estimated_model <- model_wf |>
      parsnip::fit(data = conf_model_data)
    
  } else {
    
    # create tuning grid
    folds <- rsample::vfold_cv(
      data = conf_model_data,
      v = tuner[["v"]]
    )
    
    # tune
    tuning_results <- tune::tune_grid(
      object = model_wf,
      resamples = folds,
      grid = tuner[["grid"]],
      metrics = tuner[["metrics"]]
    )
    
    # select best/finalize workflow
    # add the tuned hyperparameters to the workflow
    tuned_wf <- 
      model_wf |> 
      tune::finalize_workflow(tune::select_best(x = tuning_results))
    
    # fit the model with the best hyperparameters on all of the training data
    estimated_model <- tuned_wf |>
      parsnip::fit(data = conf_model_data) 
    
  }
  
  if (is.null(extractor)) {
  
    extraction <- NA
    
  }  else if (.is_extractor(extractor)) {
    
    # save fitted model (and hyperparameters)
    extraction <- extractor(estimated_model)
    
  } else if (extractor == "workflow") {
    
    extraction <- estimated_model
    
  }
  
  jth_fit_time <- as.numeric(Sys.time() - fit_start_time, units = "secs")
  
  return(
    list(
      fitted = estimated_model,
      extraction = extraction,
      jth_fit_time = jth_fit_time
    )
  )
  
}

#' Sample the jth variable from a fitted model
#'
#' @param fitted A fitted `workflow`, or `"identity"` for identity models, as
#' returned by `.fit_j()`.
#' @param conf_data A data frame of confidential data
#' @param synth_data A data frame of non-sensitive predictors or synthesized 
#' predictors
#' @param col_schema A list of schematic information about each variable.
#' @param recipe A recipe from `library(recipes)`.
#' @param sampler A method used to predict values from estimated models.
#' @param noise A `noise` object
#' @param constraints A list of `constraints_df` and `max_z` values
#' @param invert_transformations A Boolean for if outcome variable 
#' transformations should be inverted during synthesis
#'
#' @return A list with a data frame of predictions, the ldiversity, and the 
#' time spent sampling
#' @noRd
#' 
#' @importFrom rlang :=
#' 
.sample_j <- function(fitted,
                      conf_data,
                      synth_data,
                      col_schema,
                      recipe,
                      sampler,
                      noise,
                      constraints,
                      invert_transformations) {
  
  # start jth variable sampling time
  sample_start_time <- Sys.time()
  
  # pull outcome var name
  outcome_var <- .outcome_var(recipe)
  
  # apply identity method
  if (identical(fitted, "identity")) {
    
    predictions <- tibble::tibble(
      .pred = rep(unique(dplyr::pull(conf_data, outcome_var)), nrow(synth_data))
    ) |>
      dplyr::rename(!!outcome_var := ".pred")
    
    jth_sample_time <- as.numeric(Sys.time() - sample_start_time, units = "secs")
    
    return(
      list(
        predictions = predictions,
        ldiversity = 1,
        jth_sample_time = jth_sample_time
      )
    )
    
  }
  
  # subset the data to observations with non-missing values for numeric outcome
  # variables
  conf_model_data <- .conf_model_data(
    conf_data = conf_data,
    col_schema = col_schema,
    outcome_var = outcome_var
  )
  
  # assign constraints
  condition_defs <- NULL
  factor_levels <- NULL
  
  # Check to see if we have no constraints 
  if (!is.null(constraints$constraints_df)) {
    
    # if new variable is numeric...
    if (col_schema[["dtype"]] == "dbl") {
      
      # assign numeric constraints
      new_data <- assign_constraints_num(
        synth_data = synth_data, 
        constraints = constraints$constraints_df
      )
      
    } else {
      
      # else assign categorical constraints
      cat_constraints <- assign_constraints_cat(
        synth_data = synth_data, 
        constraints = constraints$constraints_df
      )
      
      new_data <- cat_constraints$synth_data
      condition_defs <- cat_constraints$condition_defs
      factor_levels <- col_schema[["levels"]]
      
    }
    
  } else {
      
    new_data <- synth_data
      
  }
  
  # set empty vector for predictions
  if (col_schema$dtype == "fct") {
    
    ysyn <- vector(mode = "character", length = nrow(synth_data))
    ysyn <- factor(ysyn, levels = levels(dplyr::pull(conf_data, outcome_var)))
    
  } else {
  
    ysyn <- vector(mode = "numeric", length = nrow(synth_data))
  
  }
    
  # add index to synth_data
  new_data$index <- seq.int(nrow(synth_data))

  predictions <- generate_predictions(
    model = fitted,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    sampler = sampler,
    noise = noise, 
    constraints_df = constraints$constraints_df,
    condition_defs = condition_defs,
    factor_levels = factor_levels,
    max_z = constraints$max_z,
    ysyn = ysyn,
    invert_transformations = invert_transformations,
    calc_ldiversity = TRUE,
    store_ldiversity = NULL
  )
  
  ldiversity <- predictions$ldiversity
  predictions <- predictions$ysyn
  
  # rename the vector of predictions
  predictions <- tibble::tibble(.pred = predictions) |>
    dplyr::rename(!!outcome_var := ".pred")
  
  # calculate jth variable sampling time
  jth_sample_time <- as.numeric(Sys.time() - sample_start_time, units = "secs")
  
  return(
    list(
      predictions = predictions,
      ldiversity = ldiversity,
      jth_sample_time = jth_sample_time
    )
  )
  
}

#' synthesize the jth variable
#' 
#' Fits the model for the jth variable with `.fit_j()` and immediately samples
#' from it with `.sample_j()`.
#'
#' @param conf_data A data frame for estimating the predictive model
#' @param synth_data A data frame of non-sensitive predictors or synthesized 
#' predictors
#' @param col_schema A list of schematic information about each variable.
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param recipe A recipe from `library(recipes)`.
#' @param sampler A method used to predict values from estimated models.
#' @param noise A `noise` object
#' @param tuner A `tuner` object for hyperparameter tuning. Hyperparameter 
#' tuning is skipped if `tuner = NULL`.
#' @param extractor A method for extracting workflows or extracts from 
#' workflows. `NULL` will extract no information. `"workflow"` will extract the
#' entire workflow. `extract_*()` will use extract functions from 
#' `library(workflows)` to extract information.
#' @param constraints A list of `constraints_df` and `max_z` values
#' @param invert_transformations A Boolean for if outcome variable 
#' transformations should be inverted during synthesis
#' @param p A function for tracking progress created with progressr
#'
#' @return A list with an extraction, a data frame of predictions, the 
#' synthesis time, and the ldiversity
#' @noRd
#' 
synthesize_j <- function(conf_data, 
                         synth_data,
                         col_schema,
                         model,
                         recipe,
                         sampler,
                         noise,
                         tuner,
                         extractor,
                         constraints,
                         invert_transformations,
                         p = function() { NULL } ) {
  
  fit <- .fit_j(
    conf_data = conf_data,
    col_schema = col_schema,
    model = model,
    recipe = recipe,
    tuner = tuner,
    extractor = extractor,
    p = p
  )
  
  sample <- .sample_j(
    fitted = fit[["fitted"]],
    conf_data = conf_data,
    synth_data = synth_data,
    col_schema = col_schema,
    recipe = recipe,
    sampler = sampler,
    noise = noise,
    constraints = constraints,
    invert_transformations = invert_transformations
  )
  
  # construct return object
  synth_object <- list(
    extraction = fit[["extraction"]],
    predictions = sample[["predictions"]],
    jth_synthesis_time = fit[["jth_fit_time"]] + sample[["jth_sample_time"]],
    ldiversity = sample[["ldiversity"]]
  )
  
  return(synth_object)
  
}
