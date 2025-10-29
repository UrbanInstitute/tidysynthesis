#' 
#' Add discrete Laplace noise with mean 0 to predicted values with constant variance
#'
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param pred A vector of values predicted by the model
#' @param variance float, sampling variance for additive noise
#' @param epsilon float, alternative privacy loss budget prescribed by the Laplace
#' mechanism under epsilon differential privacy.
#' @param sensitivity float, alternative sample sensitivity prescribed by the Laplace
#' mechanism under epsilon differential privacy.
#' @param increment Numeric indicating space between discrete noise samples, 
#' defaults to 1. Note that this does not impact the noise sampling variance, as 
#' the increment rescales noise distributions specified by sampling variance.
#'
#' @return A numeric vector with noise added to each prediction
#' 
#' @export
#' 
add_noise_disc_laplace <- function(
    model,
    new_data,
    conf_model_data,
    outcome_var,
    col_schema,
    pred,
    variance = NULL,
    epsilon = NULL,
    sensitivity = NULL,
    increment = 1) {
  
  stopifnot("add_noise_disc_laplace increment must be greater than 0." = { 
    increment > 0 
  })
  
  # if variance directly specified...
  if (!is.null(variance)) {
    
    if (!is.null(epsilon) | !is.null(sensitivity)) {
      
      stop("If using variance, epsilon and sensitivity cannot be specified.")
      
    }
    
    stopifnot(is.numeric(variance))
    stopifnot(variance > 0)
    
    # for different increments, rescale to normalize by the increment
    scale1_var <- variance / (increment**2)
    
    # see: https://dl.acm.org/doi/abs/10.1145/1536414.1536464
    # variance formula inverts the expression from the "Variance" derivation 
    # here: https://randorithms.com/2020/10/09/geometric-mechanism.html
    
    # inverse of discrete laplace variance as a function of scale param...
    scale_param <- log(1 + (1. + sqrt(2 * scale1_var + 1)) / scale1_var)
    
    # else if using epsilon-DP Laplace mechanism
  } else {
    
    if (is.null(epsilon) | is.null(sensitivity)) {
      
      stop("Must specify either `variance` or both `epsilon` and `sensitivity`.")
      
    }
    
    stopifnot(is.numeric(sensitivity))
    stopifnot(sensitivity > 0)
    stopifnot(is.numeric(epsilon))
    stopifnot(epsilon > 0)
    
    scale_param <- epsilon / sensitivity 
    
  }
  
  # compute scale=1 noise 
  p <- 1 - exp(-scale_param)
  d1 <- stats::rgeom(n = length(pred), p = p)
  d2 <- stats::rgeom(n = length(pred), p = p)
  scale1_noise <- d1 - d2
  
  # add noise to final result and return
  noisy_result <- pred + increment * scale1_noise
  return(noisy_result)
  
}