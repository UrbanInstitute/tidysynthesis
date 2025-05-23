#' 
#' Add Laplace noise with mean 0 to predicted values with constant variance
#'
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param pred A vector of values predicted by the model
#' @param variance Sampling variance for additive noise
#' @param epsilon Alternative privacy loss budget prescribed by the Laplace
#' mechanism under epsilon differential privacy.
#' @param sensitivity Alternative sample sensitivity prescribed by the Laplace
#' mechanism under epsilon differential privacy.
#'
#' @return A numeric vector with noise added to each prediction
#' 
#' @export
#' 
add_noise_laplace <- function(
    model,
    new_data,
    conf_model_data,
    outcome_var,
    col_schema,
    pred,
    variance = NULL,
    epsilon = NULL,
    sensitivity = NULL) {
  
    # if variance directly specified...
    if (!is.null(variance)) {
      
      if (!is.null(epsilon) | !is.null(sensitivity)) {
        
        stop("Cannot use non-null variance with non-null epsilon or sensitivity.")
        
      }
      
      stopifnot(is.numeric(variance))
      stopifnot(variance > 0)
      
      b <- sqrt(variance / 2)
    
    # else if using epsilon-DP Laplace mechanism
    } else {
      
      if (is.null(epsilon) | is.null(sensitivity)) {
        
        stop("Must specify either `variance` or both `epsilon` and `sensitivity`.")
        
      }
      
      stopifnot(is.numeric(sensitivity))
      stopifnot(sensitivity > 0)
      stopifnot(is.numeric(epsilon))
      stopifnot(epsilon > 0)
      
      b <- sensitivity / epsilon
      
    }
  
  result <- pred + ExtDist::rLaplace(n = length(pred), 
                                     mu = 0, 
                                     b = b)
  return(result)
  
}