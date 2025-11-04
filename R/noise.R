#' Create a noise object
#' 
#' @param add_noise Boolean, TRUE if adding noise
#' @param mode String, one of "regression" or "classification"
#' @param noise_func A function that adds noise to 
#' @param ... Optional named additional arguments to pass to `noise_func(...)`
#' 
#' @returns A `noise` object
#' 
#' @examples
#' 
# create default noise object
#' noise()
#' 
#' # create noise object for classification
#' noise(
#'   add_noise = TRUE,
#'   mode = "classification",
#'   noise_func = add_noise_cat_unif
#' )
#' 
#' # create noise object for regression
#' noise(
#'   add_noise = TRUE,
#'   mode = "regression",
#'   noise_func = add_noise_kde,
#'   n_ntiles = 10
#' )
#' 
#' @export 
noise <- function(add_noise = FALSE,
                  mode = "regression", 
                  noise_func = NULL,
                  ...) {
  
  stopifnot(is.logical(add_noise))
  stopifnot(mode %in% c("regression", "classification"))
  stopifnot(is.null(noise_func) | is.function(noise_func))
  
  if (add_noise & is.null(noise_func)) {
    
    stop("`add_noise` is TRUE but no `noise_func` specified.")
    
  }
  
  # capture passed arguments
  noise_params <- list(...)
  
  noise <- list(
    add_noise = add_noise,
    mode = mode,
    noise_func = noise_func,
    noise_params = noise_params
  )
  
  noise <- structure(noise, class = "noise")
  
}

is_noise <- function(x) {
  inherits(x, "noise")
}


#' Print the noise object to the console with formatting
#'
#' @param x A `noise` object
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#'
#' @examples
#' 
#' print(noise())
#' 
#' @export
print.noise <- function(x, ...) {
  
  cat("Noise \n")
  cat(sprintf("add_noise: %s", x$add_noise))
  
  invisible(x)
  
}

#'
#' Execute a noise function within `generate_predictions()`
#' 
#' @param model A `model_spec` or a list of `model_spec`s from `library(parsnip)`
#' @param new_data A data frame used to generate predictions
#' @param conf_model_data A data frame for estimating the predictive model
#' @param outcome_var A string name representing the outcome variable
#' @param col_schema A list of column schema specifications for the new variable
#' @param pred A vector of values predicted by the model
#' @param noise A `noise` S3 object
#' 
#' @return A vector of noisy predictions with the same length as `pred`
#' 
exec_noise_func <- function(model,
                            new_data,
                            conf_model_data,
                            outcome_var,
                            col_schema,
                            pred,
                            noise) {
  
  all_kwargs = c(
    list(model = model,
         new_data = new_data,
         conf_model_data = conf_model_data,
         outcome_var = outcome_var,
         col_schema = col_schema,
         pred = pred),
     noise$noise_params
  )
  
  return(
    rlang::exec(noise$noise_func, 
                !!!all_kwargs)
  )
  
}
