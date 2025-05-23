#' An S3 method for inverting a step
#'
#' @rdname invert
#' @aliases invert invert.recipe
#' @author Aaron R. Williams
#' @concept postprocessing
#' 
#' @param object A recipe after fitting a model
#' @param predictions A data frame with .pred
#' @param ... Other arguments 
#' 
#' @export
invert <- function(object, predictions, ...)
  UseMethod("invert")

#' Invert a Box-Cox transformation
#'
#' @param object A recipe after fitting a model
#' @param predictions A data frame with .pred
#' @param ... Other arguments 
#'
#' @export
invert.step_BoxCox <- function(object, predictions, ...) {
  
  lambda <- object$lambdas
  eps <- 0.001
  
  if (length(lambda) > 1) stop("Too many lambdas for invert.step_BoxCox()")
  
  if (length(lambda) > 0) {
    
    if (is.na(lambda))
      predictions[, ".pred"] <- predictions[, ".pred"]
    else if (abs(lambda) < eps)
      predictions[, ".pred"] <- exp(predictions[, ".pred"])
    else
      predictions[, ".pred"] <- (lambda * predictions[, ".pred"] + 1) ^ (1 / lambda)
    
  }
  
  return(predictions)
  
}

#' Invert a log transformation
#'
#' @param object A recipe after fitting a model
#' @param predictions A data frame with .pred
#' @param ... Other arguments 
#'
#' @export
invert.step_log <- function(object, predictions, ...) {

  predictions[, ".pred"] <- exp(predictions[, ".pred"])

  return(predictions)

}

#' Invert a Yeo-Johnson transformation
#'
#' @param object A recipe after fitting a model
#' @param predictions A data frame with .pred
#' @param ... Other arguments
#'
#' @return A tibble with Yeo_johnson transformation inverted for .pred 
#' 
#' @export
invert.step_YeoJohnson <- function(object, predictions, ...) {

  # get lambdas
  lambda <- object$lambdas
  
  if (length(lambda) > 1) stop("Too many lambdas for invert.step_YeoJohnson()")
  
  if (length(lambda) > 0) {
    
    if (is.na(lambda)) {
      
      predictions[, ".pred"] <- predictions[, ".pred"]
      
    } else {
      
      predictions[, ".pred"] <- yeo_johnson(y = predictions[, ".pred", drop = TRUE], 
                                            lambda = lambda,
                                            inverse = TRUE)
      
    }
    
  }
  
  return(predictions)
  
}

