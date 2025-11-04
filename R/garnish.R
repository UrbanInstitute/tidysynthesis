#' Invert outcome variable transformations after prediction
#'
#' @param object A fit model object
#' @param predictions A data frame of predictions with a variable .pred
#'
#' @return A vector of transformed predictions
#' 
garnish <- function(object, predictions) {
  
  # subset to the steps that affect the outcome variable
  steps <- object[["pre"]][["mold"]][["blueprint"]][["recipe"]][["steps"]]
  
  outcome_steps_index <- purrr::map_lgl(.x = steps, .f = ~grepl(pattern = "^outcome", x = .x$id))
  
  outcome_steps <- steps[outcome_steps_index]
  
  # loop over the steps and invert the transformations
  n_steps <- length(outcome_steps)
  
  transformed_predictions <- predictions
  
  for (i in seq_len(n_steps)) {

    transformed_predictions <- invert(object = outcome_steps[[i]], 
                                      predictions = transformed_predictions)
    
  }
  
  return(transformed_predictions)
  
}


