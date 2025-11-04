#' wrapper function for generating predictions
#' 
#' This function heavily relies on recursion.
#'
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param sampler A method used to predict values from estimated models.
#' @param noise A `noise` S3 object
#' @param constraints_df A data frame of constraints
#' @param condition_defs A data.frame of condition definitions
#' @param factor_levels A vector of allowable factor levels.
#' @param max_z An integer for the number of z-bounding iterations before hard 
#' bounding. A 0 will simply hard bound without iterating predictions
#' @param ysyn A vector of predicted values
#' @param invert_transformations A Boolean for if outcome variable 
#' transformations should be inverted during synthesis
#' @param calc_ldiversity A Boolean for if ldiversity should be calculated
#' @param store_ldiversity A vector for storing ldiversity
#'
#' @return A list with synthetic values and calulcated ldiveristy
#'
generate_predictions <- function(model,
                                 new_data,
                                 conf_model_data,
                                 outcome_var,
                                 col_schema,
                                 sampler,
                                 noise,
                                 constraints_df,
                                 condition_defs,
                                 factor_levels,
                                 max_z,
                                 ysyn,
                                 invert_transformations,
                                 calc_ldiversity,
                                 store_ldiversity = NULL) {
  
  # generate predictions
  if (is.null(sampler)) {
    
    stop("Missing sampler object in generate_predictions().")
    
  } 

  pred <- sampler(
    model = model, 
    new_data = new_data, 
    conf_data = conf_model_data
  )
  

  
  # pull l-diversity
  if (calc_ldiversity) {
    
    ldiversity <- if (is.list(pred)) { pred$ldiversity } else { NULL }
    
  } else if (!is.null(store_ldiversity)) {
    
    # keep ldiversity when using recursion for repeated sampling to 
    # resolve constraints
    ldiversity <- store_ldiversity
      
  }
  
  if (is.list(pred)) {
    
    pred <- pred$y_hat
    
  }
  
  
  # invert outcome variable transformations
  if (invert_transformations) {
    
    pred <- garnish(
      predictions = tibble::tibble(.pred = pred), 
      object = model
    ) |>
      dplyr::pull()
    
  }
  
  # add noise
  if (noise$add_noise) {
    
    pred <- exec_noise_func(
      model = model,
      new_data = new_data,
      conf_model_data = conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      noise = noise
    )
    
  }
  
  # test constraints
  if (is.null(constraints_df)) {
    
    # create dummy index
    fail_constraint <- FALSE
    pass_index <- data.frame(index = new_data$index, pred = pred)
    
  } else {
    
    if (col_schema[["dtype"]] == "dbl") {
      
      # find failures & index
      pass_index <- dplyr::bind_cols(new_data, pred = pred)
      
      pass_index <- pass_index |>
        dplyr::mutate(
          fail_constraint = (
            (.data$pred <= .data$.assigned_min) | 
              (.data$pred >= .data$.assigned_max)
          )
        )
      
      fail_constraint <- any(pass_index$fail_constraint)
      
    } else if (col_schema[["dtype"]] == "fct") {
      
      # find failures & index
      pass_index <- dplyr::bind_cols(new_data, pred = pred) |>
        dplyr::mutate(fail_constraint = FALSE)
      
      pass_index <- pass_index |>
        # for each .condition_id, i.e. sets of conditions where the 
        # categorical constraints apply...
        dplyr::group_by(dplyr::across(dplyr::all_of(c(".condition_id")))) |>
        # modify each group where .x = subset of rows in group and .y is 
        # the .condition_id
        dplyr::group_modify(
          .f = \(.x, .y) { 
            cid <- as.integer(.y)
            # extract the mode (whether to include or exclude values)
            mode <- as.character(condition_defs[cid, ".condition_mode"])
            # extract the values to be included or excluded
            vals <- unlist(as.list(condition_defs[cid, ".condition_vals"]))
            # check for inclusion or exclusion depending on mode
            result <- .x[["pred"]] %in% vals
            if (mode == "inclusion") {
              .x[["fail_constraint"]] <- !result
            } else if (mode == "exclusion") {
              .x[["fail_constraint"]] <- result
            }
            return(.x)
          } 
        ) |>
        dplyr::ungroup()
      
      fail_constraint <- any(pass_index$fail_constraint)
      
    }
    
  }
  
  # generate ysyn
  if (max_z == 0 && fail_constraint) {
    
      # hard bounding for numeric variables
      if (col_schema[["dtype"]] == "dbl") {
        
        # when z = 0, hard bound
        pass_index <- pass_index |>
          dplyr::mutate(pred = pmax(.data$.assigned_min, .data$pred)) |>
          dplyr::mutate(pred = pmin(.data$.assigned_max, .data$pred)) |>
          dplyr::select("index", "pred")
        
        # and set predictions
        ysyn[pass_index$index] <- pass_index$pred
        
        return(list(ysyn = ysyn, ldiversity = ldiversity))
      
      # uniform resampling for categorical variables
      } else {
      
        # when z = 0, sample from allowed conditions
        pass_index <- pass_index |>
          dplyr::group_by(dplyr::across(dplyr::all_of(c(".condition_id")))) |>
          dplyr::group_modify(
            .f = \(.x, .y) { 
              cid <- as.integer(.y)
              mode <- as.character(condition_defs[cid, ".condition_mode"])
              vals <- unlist(as.list(condition_defs[cid, ".condition_vals"]))
              
              if (mode == "inclusion") {
                .x[["pred"]] <- dplyr::if_else(
                  !.x[["fail_constraint"]], 
                  .x[["pred"]], 
                  sample(vals, size = nrow(.x), replace = TRUE)
                )
              } else {
                .x[["pred"]] <- dplyr::if_else(
                  !.x[["fail_constraint"]], 
                  .x[["pred"]], 
                  sample(setdiff(factor_levels, vals), 
                         size = nrow(.x), 
                         replace = TRUE)
                )
              }
              
              return(.x)
            } 
          ) |>
          dplyr::ungroup() |>
          dplyr::select("index", "pred")
        
        # and set predictions
        ysyn[pass_index$index] <- pass_index$pred
        
        return(list(ysyn = ysyn, ldiversity = ldiversity))
        
      }
    
  } else if (max_z > 0 && fail_constraint) {
    
    # when z > 0, set predictions for non-failures
    pass_index <- dplyr::filter(pass_index, !.data$fail_constraint)
    
    if (nrow(pass_index) > 0) {
      
      ysyn[pass_index$index] <- pass_index$pred
      
    }
    
    # reset args
    new_data <- dplyr::filter(new_data, !(.data$index %in% pass_index$index))
    max_z <- max_z - 1
    
    # and recursively recall
    generate_predictions(
      model = model,
      new_data = new_data,
      conf_model_data = conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      sampler = sampler,
      noise = noise,
      constraints_df = constraints_df,
      condition_defs = condition_defs,
      factor_levels = factor_levels,
      max_z = max_z,
      ysyn = ysyn,
      invert_transformations = invert_transformations,
      calc_ldiversity = FALSE,
      store_ldiversity = ldiversity
    )
    
  } else {
    
    ysyn[pass_index$index] <- pass_index$pred
    
    return(list(ysyn = ysyn, ldiversity = ldiversity))
    
  }
}