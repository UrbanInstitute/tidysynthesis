#' 
#' Add discrete normal noise with mean 0 to predicted values with constant variance
#'
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param pred A vector of values predicted by the model
#' @param variance float, sampling variance for additive noise
#' @param rho float, alternative privacy loss budget prescribed by the Gaussian
#' mechanism under rho-zero-concentrated differential privacy.
#' @param sensitivity float, alternative sample sensitivity prescribed by the Gaussian
#' mechanism under rho-zero-concentrated differential privacy.
#' @param increment Numeric indicating space between discrete noise samples, 
#' defaults to 1. Note that this does not impact the noise sampling variance, as 
#' the increment rescales noise distributions specified by sampling variance.
#'
#' @return A numeric vector with noise added to each prediction
#' 
#' @examples
#' 
#' add_noise_disc_gaussian(
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
add_noise_disc_gaussian <- function(
    model,
    new_data,
    conf_model_data,
    outcome_var,
    col_schema,
    pred,
    variance = NULL,
    rho = NULL,
    sensitivity = NULL,
    increment = 1) {
  
  stopifnot("add_noise_disc_gaussian increment must be greater than 0." = { 
    increment > 0 
  })
  
  # if variance directly specified...
  if (!is.null(variance)) {
    
    if (!is.null(rho) | !is.null(sensitivity)) {
      
      stop("If using variance, rho and sensitivity cannot be specified.")
      
    }
    
    stopifnot(is.numeric(variance))
    stopifnot(variance > 0)
    
    sampling_var <- variance / (increment**2)
    
    # else if using rho-zCDP Gaussian mechanism
  } else {
    
    if (is.null(rho) | is.null(sensitivity)) {
      
      stop("Must specify either `variance` or both `rho` and `sensitivity`.")
      
    }
    
    stopifnot(is.numeric(sensitivity))
    stopifnot(sensitivity > 0)
    stopifnot(is.numeric(rho))
    stopifnot(rho > 0)
    
    # see: https://arxiv.org/abs/1605.02065, proposition 1.6
    sampling_var <- sensitivity^2 / (2 * rho)
    
  }
  
  result <- pred + increment * dapper::rdnorm(n = length(pred),
                                              mu = 0,
                                              sigma = sqrt(sampling_var))

  return(result)
  
}