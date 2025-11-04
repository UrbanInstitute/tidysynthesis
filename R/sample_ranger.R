#' Sample the conditional distribution created by a ranger rf model
#'
#' @param model A "model_fit" object created by parsnip::ranger()
#' @param new_data A data frame with predictors
#' @param conf_data A data frame with original confidential predictors
#' 
#' @return A numeric vector of predictions
#' 
#' @examples
#' 
#' rf_mod_regression <- parsnip::rand_forest(trees = 500, min_n = 1) %>%
#'   parsnip::set_engine(engine = "ranger") %>%
#'   parsnip::set_mode(mode = "regression") %>%
#'   parsnip::set_args(quantreg = TRUE)
#' 
#' regression_rec <- recipes::recipe(age ~ ., data = acs_conf)
#' 
#' model_reg <- workflows::workflow() %>%
#'   workflows::add_model(spec = rf_mod_regression) %>%
#'   workflows::add_recipe(recipe = regression_rec) %>%
#'   parsnip::fit(data = acs_conf)
#' 
#' set.seed(1)
#' sample1 <- sample_ranger(
#'   model = model_reg, 
#'   new_data = acs_conf[1:3, ], 
#'   conf_data = acs_conf
#' )
#' 
#' @export
sample_ranger <- function(model, new_data, conf_data) {
  
  if (model$fit$fit$spec$mode == "classification") {
    
    # create an nrow x nvalue matrix of predicted probabilities
    # the probabilities are averaged from the trees
    prediction_matrix <- stats::predict(
      object = model$fit$fit$fit, 
      data = new_data)$predictions
    
    # sample a prediction from for each row
    y_hat <- 
      purrr::map_chr(
        .x = 1:nrow(prediction_matrix),
        .f = ~sample(
          x = names(prediction_matrix[.x, ]), 
          prob = prediction_matrix[.x, ],
          size = 1
        )
      )
    
  } else if (model$fit$fit$spec$mode == "regression") {
    
    # create an nrow x ntree matrix of predictions from each tree
    prediction_matrix <- stats::predict(
      object = model$fit$fit$fit, 
      data = new_data, 
      predict.all = TRUE)$predictions
    
    # sample a prediction from a tree from each row
    y_hat <- 
      purrr::map_dbl(
        .x = 1:nrow(prediction_matrix),
        .f = ~sample(prediction_matrix[.x, ], size = 1)
      )
    
  }
  
  return(y_hat)
  
}






