#' Specify a resampling scheme for start_data
#' 
#' @param start_data A `data.frame`
#' @param n An optional integer sample size. If unspecified, `n = nrow(start_data)`
#' @param inv_noise_scale An optional parameter to set randomized noise to the proportions
#' of records with different `start_data` characteristics. Corresponds to a privacy loss
#' budget under epsilon differential privacy.
#' @param support A string that specifies the method of resampling from the `start_data` 
#' domain. 
#'  
#' @return A `start_method` object for resampling starting data
#'  
#' @examples  
#'  
#' start_method(
#'   start_func = start_resample, n = 1000
#' )
#'  
#' @export
start_resample <- function(
    start_data,
    n = NULL,
    inv_noise_scale = NULL,
    support = c("observed", "all")
) {
  
  # argument parsing and warning messages
  stopifnot("`start_data` must be a data.frame" = { is.data.frame(start_data) }) 
  support <- match.arg(support)

  # default sample size matches start_data size
  if (is.null(n)) { n <- nrow(start_data) }
  
  # raise message if start_data size matches n
  if (n == nrow(start_data)) {
    
    message(
      "Synthetic data will be the same size as start_data, which may contain
      confidential information."
    )
    
  }
  
  # if using observed support...
  if (support == "observed") {
    
    # raise message that observed support may contain confidential information
    message(
      "Synthetic data observed support will be the same as start_data, which 
      may contain confidential information." 
    )
    
    # if no resampling noise...
    if (is.null(inv_noise_scale)) {
      
      # uniformly sample from observed data
      return(
        dplyr::slice_sample(start_data, n = n, replace = TRUE)
      )
    
    # if maximum allowable noise...
    } else if (inv_noise_scale == 0) {
    
      # uniformly resample from support of observed data
      return(
        dplyr::slice_sample(
          dplyr::distinct(start_data), n = n, replace = TRUE
        )
      )
      
    } else {
      
      # create noise function for record proportions
      p <- 1.0 - exp(-inv_noise_scale)
      geom_noise <- function(n) {
        stats::rgeom(n = n, prob = p) - stats::rgeom(n = n, prob = p)
      }
      
      result_card <- start_data |> 
        # first, compute cardinality of all observed records
        dplyr::group_by(dplyr::across(dplyr::everything())) |> 
        dplyr::tally(name = "obs_n") |>
        dplyr::ungroup()
      
      # next, add geometric noise according to inverse noise scale, clipped
      # from below at 0
      result <- result_card |>
        dplyr::mutate(
          noisy_n = pmax(0, .data[["obs_n"]] + geom_noise(dplyr::n())),
          prop_n = .data[["noisy_n"]] / sum(.data[["noisy_n"]]),
          # renormalize and resample the record cardinalities
          weight_n = c(
            stats::rmultinom(n = 1, size = n, prob = .data[["prop_n"]]))
        ) |>
        # "uncount" records and remove auxiliary columns
        tidyr::uncount(weights = .data[["weight_n"]]) |>
        dplyr::select(
          -dplyr::all_of(c("obs_n", "noisy_n", "prop_n", "weight_n")))
      
      return(result) 
      
    }
  
  } else {
    
    # complete support fails if no noise scale specified
    if (is.null(inv_noise_scale)) {
      
      stop(
        "Cannot resample from complete support with unspecified inv_noise_scale."
      )
      
    # uniform resampling from complete support 
    } else {
      
      stopifnot(inv_noise_scale > 0.)
      
      # create noise function for record proportions
      p <- 1.0 - exp(-inv_noise_scale)
      geom_noise <- function(n) {
        stats::rgeom(n = n, prob = p) - stats::rgeom(n = n, prob = p)
      }
      
      result_card <- start_data |> 
        # recast each level as factor 
        dplyr::mutate(
          dplyr::across(
            -dplyr::where(base::is.factor),
            \(x) { factor(x, exclude = NULL) }
          )) |>
        # compute cardinality of all observed records
        dplyr::group_by(
          dplyr::across(dplyr::everything()), 
          .drop = FALSE) |> 
        dplyr::tally(name = "obs_n") |>
        dplyr::ungroup() 
      
      # next, add geometric noise according to inverse noise scale, clipped
      # from below at 0. See https://randorithms.com/2020/10/09/geometric-mechanism.html
      # for a description
      result <- result_card |> 
        dplyr::mutate(
          noisy_n = pmax(0, .data[["obs_n"]] + geom_noise(dplyr::n())),
          prop_n = .data[["noisy_n"]] / sum(.data[["noisy_n"]]),
          # renormalize and resample the record cardinalities
          weight_n = c(
            stats::rmultinom(n = 1, size = n, prob = .data[["prop_n"]]))
        ) |>
        # "uncount" records and remove auxiliary columns
        tidyr::uncount(weights = .data[["weight_n"]]) |>
        dplyr::select(
          -dplyr::all_of(c("obs_n", "noisy_n", "prop_n", "weight_n")))
      
      return(result) 
      
    }
    
  }
  
}