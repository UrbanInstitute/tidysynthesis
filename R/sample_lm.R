#' Sample the conditional distribution created by a linear model
#'
#' @param model A "model_fit" object created by parsnip::linear_reg()
#' @param new_data A data frame with predictors
#' @param conf_data A data frame with original confidential predictors
#'
#' @return A numeric vector of predictions
#' 
#' @examples
#' 
#' lm_mod <- parsnip::linear_reg() |>
#'   parsnip::set_engine("lm") |>
#'   parsnip::set_mode(mode = "regression")
#' 
#' regression_rec <- recipes::recipe(inctot ~ ., data = acs_conf)
#' 
#' model_reg <- workflows::workflow() |>
#'   workflows::add_model(spec = lm_mod) |>
#'   workflows::add_recipe(recipe = regression_rec) |>
#'   parsnip::fit(data = acs_conf)
#' 
#' set.seed(1)
#' sample1 <- sample_lm(
#'   model = model_reg, 
#'   new_data = acs_conf[1:3, ], 
#'   conf_data = acs_conf
#' )
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
