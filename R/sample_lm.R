#' Sample the conditional distribution created by a linear model
#'
#' @param model A "model_fit" object created by parsnip::linear_reg()
#' @param new_data A data frame with predictors
#' @param conf_data A data frame with original confidential predictors
#'
#' @return A numeric vector of predictions
#' 
#' @export
sample_lm <- function(model, new_data, conf_data) {
  
  if (model$fit$fit$spec$mode == "classification") {
    
    stop("sample_lm only works with regression models")
    
  }
  
  pred_i <- stats::predict(model, new_data = new_data)
  
  y_hat <- stats::rnorm(n = nrow(new_data), mean = dplyr::pull(pred_i), sd = stats::sigma(model$fit$fit$fit))
  
  return(y_hat)
                 
}
