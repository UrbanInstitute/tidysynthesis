#' 
#' Add normal noise with mean 0 to predicted values with constant variance
#'
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param pred A vector of values predicted by the model
#' @param variance Sampling variance for additive noise
#' @param rho Alternative privacy loss budget prescribed by the Gaussian
#' mechanism under rho-zero-concentrated differential privacy.
#' @param sensitivity Alternative sample sensitivity prescribed by the Gaussian
#' mechanism under rho-zero-concentrated differential privacy.
#'
#' @return A numeric vector with noise added to each prediction
#' 
#' @examples
#' 
#' add_noise_gaussian(
#'   model = NULL,
#'   new_data = NULL,
#'   conf_model_data = NULL,
#'   outcome_var = NULL,
#'   col_schema = NULL,
#'   pred = 1:100,
#'   variance = 3
#' )
#' 
#' @export
add_noise_gaussian <- function(
    model,
    new_data,
    conf_model_data,
    outcome_var,
    col_schema,
    pred,
    variance = NULL,
    rho = NULL,
    sensitivity = NULL) {
  
    # if variance directly specified...
    if (!is.null(variance)) {
      
      if (!is.null(rho) | !is.null(sensitivity)) {
        
        stop("Cannot use non-null variance with non-null rho or sensitivity.")
        
      }
      
      stopifnot(is.numeric(variance))
      stopifnot(variance > 0)
      
      sampling_var <- variance
    
    # else if using rho-zCDP Gaussian mechanism
    } else {
      
      if (is.null(rho) | is.null(sensitivity)) {
        
        stop("Must specify either `variance` or both `rho` and `sensitivity`.")
        
      }
      
      stopifnot(is.numeric(sensitivity))
      stopifnot(sensitivity > 0)
      stopifnot(is.numeric(rho))
      stopifnot(rho > 0)
      
      sampling_var <- sensitivity^2 / (2 * rho)
      
    }
  
  result <- pred + stats::rnorm(n = length(pred), 
                                mean = 0, 
                                sd = sqrt(sampling_var))
  return(result)
  
}