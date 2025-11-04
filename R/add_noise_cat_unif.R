#' Inject noise into a categorical random variable by mixing a sample of uniform
#' records into the predictions.
#'
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param pred A vector of values predicted by the model
#' @param unif_prop A proportion of records to resample with uniform noise
#' @param resample_props An optional named vector of probabilities for resampling,
#' defaults to uniform over all levels supplied in `col_schema`.
#' @param observed_levels An optional Boolean to only resample from observed levels
#' in the confidential data. 
#' 
#' @return A numeric vector with noise added to each prediction
#' 
#' @examples
#' 
#' conf_model_data <- mtcars|>
#'   dplyr::mutate(gear = factor(.data[["gear"]]))
#' 
#' col_schema <- list(
#'   "dtype" = "fct",
#'   "levels" = c("3", "4", "5"),
#'   "na_prop" = 0
#' )
#' 
#' add_noise_cat_unif(
#'   model = conf_model_data,
#'   new_data = NULL,
#'   conf_model_data = NULL,
#'   outcome_var = "gear",
#'   col_schema = col_schema,
#'   pred = factor(c(rep("3", 10), rep("4", 10), rep("5", 10))),
#'   unif_prop = 0.5
#' )
#' 
#' @export 
add_noise_cat_unif <- function(
    model,
    new_data,
    conf_model_data,
    outcome_var,
    col_schema,
    pred,
    unif_prop,
    resample_props = NULL,
    observed_levels = FALSE) {
  
  stopifnot(is.numeric(unif_prop))
  stopifnot(unif_prop >= 0 & unif_prop <= 1)
  
  # construct levels to resample, starting with col_schema
  resample_levels <- col_schema$levels
  
  # if only using observed levels...
  if (observed_levels) {
    
    # subset to levels observed in conf_model_data
    resample_levels_new <- intersect(
      resample_levels, 
      dplyr::pull(conf_model_data, outcome_var)
    )
    
    # raise a warning if levels dropped
    if (!identical(resample_levels_new, resample_levels)) {
      
      dropped_levels <- setdiff(
        resample_levels,
        dplyr::pull(conf_model_data, outcome_var)
      )
      
      warning("Level(s) dropped from resampling: ",
              paste0(dropped_levels, collapse = ", "))
      
    }
    
    resample_levels <- resample_levels_new
    
  }
  
  # if using custom resampling proportions
  if (!is.null(resample_props)) {
    
    # ensure all names are valid levels
    stopifnot(names(resample_props) %in% resample_levels)
    
  } else {
    
    # else, add uniform resampling probabilities for all levels
    resample_props <- rep(1, length(resample_levels))
    
  }
  
  n <- length(pred)
  
  # sample indicators whether to use original or resampled value 
  resample_ind <- (stats::runif(n) <= unif_prop)
  
  # create a vector of resampled values 
  resample_vals = sample(resample_levels, 
                         size = n, 
                         replace = TRUE,
                         prob = resample_props)
  
  # combine using the mixture
  results <- dplyr::if_else(resample_ind, resample_vals, pred)
  
  # readd original factor levels and return
  return(
    factor(results, levels = col_schema$levels)
  )
  
}
