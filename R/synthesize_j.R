#' synthesize the jth variable
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
#' @return A list with an estimated model and a data frame of predictions
#' @noRd
#' 
#' @importFrom rlang :=
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
  
  p()
  
  # start jth variable synthesis time
  synth_j_start_time <- Sys.time()
  
  # pull outcome var name
  outcome_var <- recipe[["var_info"]][["variable"]][[
    which(recipe$var_info$role == "outcome")
  ]]
  
  # apply identity method
  if (model[["mode"]] == "identity") {
    
    # set the ldiversity
    ldiversity <- 1

    # # copy the original value
    # 
    
    predictions <- tibble::tibble(
      .pred = rep(unique(dplyr::pull(conf_data, outcome_var)), nrow(synth_data))
    ) |>
      dplyr::rename(!!outcome_var := ".pred")
    
    # calculate the synthesis time for the jth variable
    synth_j_end_time <- Sys.time()
    jth_synthesis_time <- as.numeric(
      synth_j_end_time - synth_j_start_time, units = "secs"
    )
    
    # add NULL model object
    extraction <- "identity"
    
    return(
      list(
        predictions = predictions,
        jth_synthesis_time = jth_synthesis_time,
        extraction = extraction,
        ldiversity = ldiversity
      )
    )
    
  }

  # subset the data to observations with non-missing values for numeric outcome
  # variables
  if (col_schema[["na_prop"]] > 0 & col_schema[["dtype"]] == "dbl") {
    
    conf_model_data <- conf_data[!is.na(conf_data[[outcome_var]]), ]
      
  } else {
    
    conf_model_data <- conf_data
    
  }
  
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
    model = estimated_model,
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
  
  # calculate jth variable synthesis time
  synth_j_end_time <- Sys.time()
  jth_synthesis_time <- as.numeric(
    synth_j_end_time - synth_j_start_time, units = "secs"
  )
  
  # construct return object
  synth_object <- list(extraction = extraction,
                       predictions = predictions,
                       jth_synthesis_time = jth_synthesis_time,
                       ldiversity = ldiversity)
  
  return(synth_object)
  
}
