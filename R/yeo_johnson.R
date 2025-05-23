#' Ported from library(VGAM)
#'
#' @param x A vector
#' @param length.arg A numeric
#' @param integer.valued A logical
#' @param positive A logical
#' 
is.Numeric <- function(x, length.arg = Inf,
                       integer.valued = FALSE, positive = FALSE) {
  
  if (all(is.numeric(x)) && all(is.finite(x)) &&
      (if (is.finite(length.arg))
        length(x) == length.arg else TRUE) &&
      (if (integer.valued) all(x == round(x)) else TRUE) &&
      (if (positive) all(x>0) else TRUE)) TRUE else FALSE

}
  
#' Perform a Yeo-Johnson transformation. Ported from library(VGAM)
#'
#' @param y Numeric, a vector or matrix.
#' @param lambda Numeric. It is recycled to the same length as y if necessary.
#' @param derivative Non-negative integer. The default is the ordinary function evaluation, otherwise the derivative with respect to lambda.
#' @param epsilon Numeric and positive value. The tolerance given to values of lambda when comparing it to 0 or 2.
#' @param inverse Logical. Return the inverse transformation?
#'
#' @return A transformed numeric vector
#'
#' @export
#'
yeo_johnson <- function(y, lambda, derivative = 0, epsilon = sqrt(.Machine$double.eps), 
                        inverse = FALSE) {
  if (!is.Numeric(derivative, length.arg = 1, integer.valued = TRUE) || 
      derivative < 0) 
    stop("argument 'derivative' must be a non-negative integer")
  ans <- y
  if (!is.Numeric(epsilon, length.arg = 1, positive = TRUE)) 
    stop("argument 'epsilon' must be a single positive number")
  L <- max(length(lambda), length(y))
  if (length(y) != L) 
    y <- rep_len(y, L)
  if (length(lambda) != L) 
    lambda <- rep_len(lambda, L)
  if (inverse) {
    if (derivative != 0) 
      stop("argument 'derivative' must 0 when inverse = TRUE")
    if (any(index <- y >= 0 & abs(lambda) > epsilon)) 
      ans[index] <- (y[index] * lambda[index] + 1)^(1/lambda[index]) - 
        1
    if (any(index <- y >= 0 & abs(lambda) <= epsilon)) 
      ans[index] <- expm1(y[index])
    if (any(index <- y < 0 & abs(lambda - 2) > epsilon)) 
      ans[index] <- 1 - (-(2 - lambda[index]) * y[index] + 
                           1)^(1/(2 - lambda[index]))
    if (any(index <- y < 0 & abs(lambda - 2) <= epsilon)) 
      ans[index] <- -expm1(-y[index])
    return(ans)
  }
  if (derivative == 0) {
    if (any(index <- y >= 0 & abs(lambda) > epsilon)) 
      ans[index] <- ((y[index] + 1)^(lambda[index]) - 1)/lambda[index]
    if (any(index <- y >= 0 & abs(lambda) <= epsilon)) 
      ans[index] <- log1p(y[index])
    if (any(index <- y < 0 & abs(lambda - 2) > epsilon)) 
      ans[index] <- -((-y[index] + 1)^(2 - lambda[index]) - 
                        1)/(2 - lambda[index])
    if (any(index <- y < 0 & abs(lambda - 2) <= epsilon)) 
      ans[index] <- -log1p(-y[index])
  }
  else {
    psi <- Recall(y = y, lambda = lambda, derivative = derivative - 
                    1, epsilon = epsilon, inverse = inverse)
    if (any(index <- y >= 0 & abs(lambda) > epsilon)) 
      ans[index] <- ((y[index] + 1)^(lambda[index]) * (log1p(y[index]))^(derivative) - 
                       derivative * psi[index])/lambda[index]
    if (any(index <- y >= 0 & abs(lambda) <= epsilon)) 
      ans[index] <- (log1p(y[index]))^(derivative + 1)/(derivative + 
                                                          1)
    if (any(index <- y < 0 & abs(lambda - 2) > epsilon)) 
      ans[index] <- -((-y[index] + 1)^(2 - lambda[index]) * 
                        (-log1p(-y[index]))^(derivative) - derivative * 
                        psi[index])/(2 - lambda[index])
    if (any(index <- y < 0 & abs(lambda - 2) <= epsilon)) 
      ans[index] <- (-log1p(-y[index]))^(derivative + 1)/(derivative + 
                                                            1)
  }
  ans
}