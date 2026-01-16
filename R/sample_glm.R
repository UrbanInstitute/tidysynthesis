#' Sample the conditional distribution created by a generalized linear model
#' 
#' Currently, logistic and poisson regression are supported using `parsnip` and
#' the standard `glm` engine. Note that poisson regression requires the suggested
#' `poissonreg` library. 
#'
#' @param model A "model_fit" object created by parsnip
#' @param new_data A data frame with predictors
#' @param conf_data A data frame with original confidential predictors
#'
#' @return A numeric vector of predictions
#' 
#' @examples
#' 
#' acs_conf <- acs_conf |>
#'   tidyr::drop_na()
#' 
#' logistic_mod <- parsnip::logistic_reg() |>
#'   parsnip::set_engine("glm") |>
#'   parsnip::set_mode(mode = "classification")
#' 
#' classification_rec <- recipes::recipe(hcovany ~ ., data = acs_conf)
#' 
#' model_class <- workflows::workflow() |>
#'   workflows::add_model(spec = logistic_mod) |>
#'   workflows::add_recipe(recipe = classification_rec) |>
#'   parsnip::fit(data = acs_conf)
#' 
#' set.seed(1)
#' sample1 <- sample_glm(
#'   model = model_class, 
#'   new_data = acs_conf[1:3, ], 
#'   conf_data = acs_conf
#' )
#' 
#' @export
sample_glm <- function(model, new_data, conf_data) {
  
  if (model$fit$fit$spec$mode == "classification") {
    
    if (!("logistic_reg" %in% class(model$fit$fit$spec))) {
      
      stop("GLM classification only supported for logistic regression")
      
    }
      
    probs <- stats::predict(
      model, new_data = new_data, type = "prob") |> 
      dplyr::rename_all(~ stringr::str_remove(string = ., pattern = "^.pred_"))
    
    levels <- names(probs)
    
    y_hat <- purrr::pmap(probs, ~ c(...)) |>
      purrr::map_chr(~ sample(levels, size = 1, prob = .x)) |>
      factor(levels = levels)
    
    return(y_hat)
    
  } else {
    
    if (!("poisson_reg" %in% class(model$fit$fit$spec))) {
      
      stop("GLM regression only supported for Poisson regression")
      
    }
    
    pred_i <- stats::predict(model, new_data = new_data)
    
    y_hat <- stats::rpois(n = nrow(new_data), lambda = dplyr::pull(pred_i))
    
    return(y_hat)
    
  } 
}



