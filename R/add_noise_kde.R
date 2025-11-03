#' Calculate bandwidths for each ntile in a confidential vector
#'
#' @param baseline A numeric vector of confidential data
#' @param n An integer for the number of ntiles
#' @param ties_method The ntiles approach to adding noise requuires a one-to-one
#' mapping from model-generated values to ntiles in the original data. The 
#' methods "collapse", "random", and "exclusions" deal with situations where the
#' ntiles lack unique bounds.
#'
#' @return A data frame with ntiles and bandwidths
#'
.calc_bandwidths <- function(baseline, n, ties_method = "collapse") {
  
  # 1. break the baseline vector into ntiles based on bounds from the baseline data
  baseline_ntiles <- tibble::tibble(
    baseline = baseline,
    ntile = .create_ntiles(
      x_bounds = baseline, 
      y_ntiles = baseline, 
      n = n, 
      na.rm = TRUE, 
      ties_method = ties_method)$ntiles
  )
  
  ntiles <- sort(unique(baseline_ntiles$ntile))
  
  baseline_ntiles <- split(x = baseline_ntiles, f = baseline_ntiles$ntile)
  
  # 2. estimate the KDE bandwidths using the baseline data
  bandwidths <- tibble::tibble(
    ntile = ntiles,
    bandwidth = purrr::map_dbl(.x = baseline_ntiles, 
                               .f = ~ density(.$baseline, 
                                              bw = "nrd0", 
                                              kernel = "gaussian")$bw)
  ) |>
    dplyr::mutate(bandwidth = unname(.data$bandwidth))
  
  return(bandwidths)
  
}

#' Calculate ntiles on confidential data and apply to predicted values
#'
#' Parts of the x distribution with many repeated values can lead to duplicated 
#' bounds. Duplicated bounds are combined into one ntile for the y distribution.
#'
#' @param x_bounds A numeric vector used to calculate the ntile bounds
#' @param y_ntiles A numeric vector to which the bounds from x_bounds are applied
#' @param n The number of ntiles to create
#' @param ties_method The ntiles approach to adding noise requires a one-to-one
#' mapping from model-generated values to ntiles in the original data. The 
#' methods "collapse", "random", and "exclusions" deal with situations where the
#' ntiles lack unique bounds.
#' @param na.rm logical; if true, any `NA` and `NaN`'s are removed from x_bounds 
#' before the quantiles are computed.
#'
#' @return A numeric vector of ntiles
#'
.create_ntiles <- function(x_bounds, 
                           y_ntiles, 
                           n,
                           ties_method = c("collapse", "random", "exclusions"),
                           na.rm = FALSE) {
  
  # set inputs
  ties_method <- match.arg(ties_method)
  
  # test inputs
  stopifnot(is.numeric(x_bounds))
  stopifnot(is.numeric(y_ntiles))
  stopifnot(n %% 1 == 0)
  stopifnot(is.numeric(n))
  
  if (n > length(x_bounds)) {
    
    stop("`n` can't exceed the length of the confidential variable")
    
  }
  
  if (na.rm) {
    
    x_bounds <- x_bounds[!is.na(x_bounds)]
    
  } else {
    
    stopifnot(!any(is.na(x_bounds)))
    
  }
  
  
  # calculate bounds ------------------------------------------------------
  
  if (ties_method == "random") {
    
    # find the smallest magnitude value
    smallest_bound <- min(abs(c(x_bounds, y_ntiles)))
    
    # find the maximum for runif
    # Use an arbitrary small value when 0 is the smallest bound
    arbitrary_small_value <- 0.0000001
    b <- max(smallest_bound / 1000000, arbitrary_small_value)
    
    # create a vector of random errors
    random_error <- stats::runif(n = length(x_bounds), min = -b, max = b)
    
    # add random errors so bound uniquely belong to ntiles
    x_bounds <- x_bounds + random_error
    y_ntiles <- y_ntiles + random_error
    
    # nocov start
    if (length(unique(x_bounds)) != length(x_bounds)) {
      
      warning("Not all values for calculating bounds are unique")
      
    }
    
    if (length(unique(y_ntiles)) != length(y_ntiles)) {
      
      warning("Not all values for calculating bounds are unique")
      
    }
    # nocov end
    
  }
  
  # calculate candidate bounds using the ntile functions
  # we'll still need to deal with duplicates
  ntile_extremes <- data.frame(x_bounds = x_bounds, 
                               ntile = dplyr::ntile(x_bounds, n = n)) |>
    dplyr::group_by(.data$ntile) |>
    dplyr::summarise(
      ntile_min = min(.data$x_bounds), 
      ntile_max = max(.data$x_bounds)
    ) 
  
  derived_exclusions <- NULL
  if (ties_method == "exclusions") {
    
    derived_exclusions <- ntile_extremes |>
      dplyr::filter(.data$ntile_min == .data$ntile_max) |>
      dplyr::pull(.data$ntile_min) |>
      unique()
    
    if (length(derived_exclusions) == 0) { 
      
      derived_exclusions <- NULL 
      
    }
    
  }
  
  candidate_bounds <- ntile_extremes |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(-dplyr::any_of("ntile"), 
                        names_to = "key", 
                        values_to = "value") |>
    dplyr::arrange(.data$ntile, dplyr::desc(.data$key)) 
  
  bounds <- candidate_bounds |>
    # drop the minimum value, which will be replaced with -Inf
    dplyr::mutate(lead = dplyr::lead(.data$value, n = 1L)) |>
    dplyr::filter(.data$key == "ntile_max") |>
    # calculating bounds as the average between the obs on either side of the cutpoint
    dplyr::mutate(bounds = (.data$value + .data$lead) / 2) |>
    dplyr::slice(-dplyr::n()) |>
    # only consider unique bounds
    dplyr::filter(.data$value != .data$lead) |>
    dplyr::count(.data$bounds) |>
    dplyr::filter(.data$n == 1) |>
    dplyr::pull(.data$bounds)
  
  bounds <- c(-Inf, bounds, Inf)
  
  # apply the bounds to the new vector
  ntiles <- cut(
    x = y_ntiles, 
    breaks = bounds, 
    include.lowest = TRUE, 
    right = FALSE, 
    labels = FALSE
  )
  
  return(
    list(
      ntiles = ntiles,
      derived_exclusions = derived_exclusions
    )
  )
  
}

#' 
#' Add normal noise to predicted values with variances calculated for ntiles 
#' using Gaussian kernel density estimators
#'
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param pred A vector of values predicted by the model
#' @param exclusions Numeric values that should not receive extra noise
#' @param n_ntiles The number of ntiles
#' @param obs_per_ntile A numeric for the minimum number of observations to be 
#' in an ntile. Cannot be used in conjunction with the `n_ntiles` argument.
#' @param ties_method The ntiles approach to adding noise requires a one-to-one
#' mapping from model-generated values to ntiles in the original data. The 
#' methods "collapse", "random", and "exclusions" deal with situations where the
#' ntiles lack unique bounds. "collapse" collapses ntile breaks to preserve the
#' one-to-one relationship; "random" adds a small random perturbation to the 
#' derived boundaries; finally, "exclusions" treats ntile tie values as derived
#' exclusions.
#' @param sd_scale float, a positive number to scale the estimated KDE variance. 
#' Defaults to 1.0
#'
#' @return A numeric vector with noise added to each prediction
#' 
#' @examples
#' 
#' add_noise_kde(
#'   model = NULL,
#'   new_data = tibble::tibble(x = 1:100),
#'   conf_model_data = tibble::tibble(x = 1:100),
#'   outcome_var = "x",
#'   col_schema = NULL,
#'   pred = 1:100,
#'   n_ntiles = 4
#' )
#'
#' @export
add_noise_kde <- function(model,
                          new_data,
                          conf_model_data,
                          outcome_var,
                          col_schema,
                          pred,
                          exclusions = NULL,
                          n_ntiles = NULL, 
                          obs_per_ntile = NULL,
                          ties_method = "collapse",
                          sd_scale = 1.0) {
  
  # check args
  if (!is.null(n_ntiles) & !is.null(obs_per_ntile)) {
    
    stop("`n_ntiles` and `obs_per_ntile` cannot be set together")
    
  }
  
  if (is.null(n_ntiles) & is.null(obs_per_ntile)) {
    
    stop("`n_ntiles` and `obs_per_ntile` are both NULL")
    
  }
  
  if (is.null(n_ntiles)) {
    
    n_ntiles <- floor(nrow(conf_model_data) / obs_per_ntile)
    
  }
  
  valid_ties_method <- c("collapse", "exclusions", "random")
  
  if (!ties_method %in% valid_ties_method) {
    stop(
      "`ties_method` argument must be one of: ",
      paste0(valid_ties_method, collapse = ", ")
    )
  }
  
  stopifnot("`sd_scale` must be a positive number" = {
    sd_scale > 0
  })
  
  
  # 1 + 2: extract baseline data and calculate confidential KDE bandwidths
  baseline <- dplyr::pull(conf_model_data, outcome_var)
  
  bandwidths <- .calc_bandwidths(baseline = baseline, 
                                 n = n_ntiles, 
                                 ties_method = ties_method)
  
  # 3. find the ntiles of the predicted vector based on breaks from the baseline data
  ntiles <- .create_ntiles(
    x_bounds = baseline, 
    y_ntiles = pred, 
    n = n_ntiles, 
    ties_method = ties_method,
    na.rm = TRUE
  )
  
  pred_ntiles <- tibble::tibble(
    pred,
    ntile = ntiles$ntiles
  )
  
  # combine specified and derived exclusions
  exclusions <- unique(c(exclusions, ntiles$derived_ntiles))
  
  # 4. join the bandwidths from the baseline data to the predicted data
  pred_ntiles <- dplyr::left_join(
    x = pred_ntiles, 
    y = bandwidths, 
    by = "ntile",
    relationship = "many-to-one"
  )
  
  # 5. sample from normal distributions with means at the predicted value and standard deviations of the bandwidths
  if (is.null(exclusions)) {
    
    pred_with_noise <- dplyr::bind_cols(
      pred_ntiles,
      pred_with_noise = purrr::map2_dbl(.x = pred_ntiles$pred, 
                                        .y = pred_ntiles$bandwidth * sd_scale, 
                                        .f = ~ rnorm(n = 1, mean = .x, sd = .y))
    ) |>
      dplyr::pull(pred_with_noise)
  } else {
    
    pred_with_noise <- dplyr::bind_cols(
      pred_ntiles,
      pred_with_noise = purrr::map2_dbl(
        .x = pred_ntiles$pred, 
        .y = pred_ntiles$bandwidth * sd_scale, 
        .f = ~ dplyr::if_else(condition = .x %in% exclusions, 
                              true = as.numeric(.x), 
                              false = rnorm(n = 1, mean = .x, sd = .y))
      )
    ) |>
      dplyr::pull(pred_with_noise)
    
  }
  
  return(pred_with_noise)
  
}