#' Kolmogorov-Smirnov distance
#'
#' @param data A `data.frame` containing the columns specified by the truth and 
#' estimate arguments.
#' @param truth The column identifier for the true results (that is numeric). 
#' This should be an unquoted column name although this argument is passed by 
#' expression and supports quasiquotation (you can unquote column names). For 
#' `⁠_vec(`)⁠ functions, a numeric vector.
#' @param estimate The column identifier for the predicted results (that is 
#' also numeric). As with truth this can be specified different ways but the 
#' primary method is to use an unquoted variable name. For `⁠_vec(`)⁠ functions, 
#' a numeric vector.
#' @param na_rm A `logical` value indicating whether `NA` values should be 
#' stripped before the computation proceeds.
#' @param case_weights This is a placeholder for now and will be added when 
#' case_weights are added to tidysynthesis.
#' @param ... Not currently used.
#'
#' @return For `ks_distance_vec()`, a single numeric value (or `NA`).
#' 
#' @examples
#' 
#' ks1 <- data.frame(x = 1:100, y = 101:200)
#' 
#' ks_distance(data = ks1, truth = x, estimate = y)
#' 
#' @export
ks_distance <- function(data, ...) {
  UseMethod("ks_distance")
}

ks_distance <- yardstick::new_numeric_metric(
  ks_distance,
  direction = "minimize"
)

#' @rdname ks_distance
#' 
#' @return A single numeric value (or `NA`).
#' 
#' @examples
#' 
#' ks1 <- data.frame(x = 1:100, y = 101:200)
#' 
#' ks_distance(data = ks1, truth = x, estimate = y)
#' 
#' @export
ks_distance.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  yardstick::numeric_metric_summarizer(
    name = "ks_distance",
    fn = ks_distance_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights)
  )
  
}

#' @rdname ks_distance
#' 
#' @return A single numeric value (or `NA`).
#' 
#' @examples
#' 
#' ks1 <- data.frame(x = 1:100, y = 101:200)
#' 
#' ks_distance_vec(truth = ks1$x, estimate = ks1$y)
#' 
#' @export
ks_distance_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  
  yardstick::check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
    
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    
    return(NA_real_)
    
  }
  
  ks_distance_impl(truth, estimate, case_weights = case_weights)
  
}

ks_distance_impl <- function(truth, estimate, case_weights) {
  
  # find the eCDFs for both variables
  ecdf_truth_function <- stats::ecdf(truth)
  ecdf_estimate_function <- stats::ecdf(estimate)
  
  
  # calculate the minimum and maximum across both variables
  combined_data <- c(truth, estimate)
  
  minimum <- min(combined_data)
  maximum <- max(combined_data)
  
  # calculate the distances between the two
  ecdf_truth <- ecdf_truth_function(combined_data)
  ecdf_estimate <- ecdf_estimate_function(combined_data)
  
  absolute_differences <- abs(ecdf_truth - ecdf_estimate)
  
  D <- max(absolute_differences)
  
  return(D)
  
}


