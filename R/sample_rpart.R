#' Sample the conditional distribution created by a CART model
#'
#' @param model A "model_fit" object created by rpart
#' @param new_data A data frame with predictors
#' @param conf_data A data frame with original confidential predictors
#' @param ignore_zeros Should a vector of all 0 observations return NA for 
#' the l-diversity calculation. Defaults to `TRUE`.
#' 
#' @return A numeric vector of predictions
#' 
#' @examples
#' 
#' rpart_mod_reg <- parsnip::decision_tree() %>%
#'   parsnip::set_engine("rpart") %>%
#'   parsnip::set_mode(mode = "regression")
#' 
#'  regression_rec <- recipes::recipe(inctot ~ ., data = acs_conf)
#'  
#'  model_reg <- workflows::workflow() %>%
#'    workflows::add_model(spec = rpart_mod_reg) %>%
#'    workflows::add_recipe(recipe = regression_rec) %>%
#'    parsnip::fit(data = acs_conf)
#'  
#'  set.seed(1)
#'  sample1 <- sample_rpart(
#'    model = model_reg, 
#'    new_data = acs_conf[1:3, ], 
#'    conf_data = acs_conf
#'  )
#'  
#' @examples
#' 
#' rpart_mod_class <- parsnip::decision_tree() %>%
#'   parsnip::set_engine("rpart") %>%
#'   parsnip::set_mode(mode = "classification")
#' 
#' classification_rec <- recipes::recipe(hcovany ~ ., data = acs_conf)
#' 
#' model_reg <- workflows::workflow() %>%
#'   workflows::add_model(spec = rpart_mod_class) %>%
#'   workflows::add_recipe(recipe = classification_rec) %>%
#'   parsnip::fit(data = acs_conf)
#' 
#' set.seed(1)
#' sample1 <- sample_rpart(
#'   model = model_reg, 
#'   new_data = acs_conf[1:10, ], 
#'   conf_data = acs_conf
#' )
#' 
#' @export
sample_rpart <- function(model, new_data, conf_data, ignore_zeros = TRUE) {
  
  # helper function to calculate l-diversity
  calc_ldiversity <- function(x, ignore_zeros = TRUE) {
    
    # ignore vectors that are all zeros
    if (ignore_zeros) {
      
      if (all(x == 0)) {
        
        return(NA)
        
      }
      
    }
    
    # calculate l diversity
    return(length(unique(x)))
    
  }
  
  if (model$fit$fit$spec$mode == "classification") {
    
    # create a matrix of predictions where each column is a probability of a class
    predictions <- stats::predict(
      object = model, 
      new_data = new_data, 
      type = "prob"
    )
    
    # convert the tibble a vector
    pred_mat <- as.matrix(predictions)
    
    # get a vector of classes
    class <- model$fit$fit$lvl
    
    # sample the predicted value
    # y_hat <- vector(mode = "character", length = nrow(pred_mat))
    y_hat <- purrr::map_chr(
      .x = 1:nrow(pred_mat), 
      .f = ~sample(class, prob = pred_mat[.x, ], size = 1)
    )
    
    ldiversity_conf <- purrr::map_dbl(
      .x = 1:nrow(pred_mat), 
      .f = ~ length(unique(pred_mat[.x, ]))
    )
    
    y_hat <- factor(y_hat, levels = class)
    
  } else {
    
    # replace the node mean with an identifier for the node
    model$fit$fit$fit$frame$yval <- as.numeric(
      row.names(model$fit$fit$fit$frame)
    )
    
    # predict the node for each observation in the synthetic data
    nodes_synth <- stats::predict(
      object = model,  
      new_data = new_data, 
      type = "raw"
    )
    
    # create a table with the number of observations from each node
    nodes_synth <- unname(nodes_synth)
    
    unique_nodes <- dplyr::count(
      data.frame(nodes = nodes_synth), 
      .data$nodes
    )
    
    # create an index to store the order of the predicted values
    # we will sort by this index later
    nodes_synth <- data.frame(
      index = seq.int(length(nodes_synth)),
      nodes_synth_before = nodes_synth
    ) %>%
      dplyr::arrange(.data$nodes_synth_before)
    
    # predict the node for each observation in the confidential data
    nodes_conf <- stats::predict(
      object = model, 
      new_data = conf_data, 
      type = "raw" 
    )
    
    # create a data frame with the node number and values within each node
    # we will use this data frame to sample values from each node
    nodes_conf <- data.frame(
      nodes = as.vector(nodes_conf),
      y = dplyr::pull(model$pre$mold$outcomes)
    )
    
    # create a data frame with the node id, the number of observations to 
    # sample, and values to sample
    donors <- 
      dplyr::left_join(
        x = unique_nodes, 
        y = nodes_conf, 
        by = "nodes",
        relationship = "one-to-many"
      ) 
    
    # sample values from the donors and calculate ldiversity
    sampled_values <- donors %>%
      dplyr::group_by(.data$nodes) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        yhat = purrr::map(
          .x = .data$data, 
          .f = function(i) i$y[sample(x = length(i$y), size = i$n[1], replace = TRUE)]
        ),
        ldiversity = purrr::map(
          .x = .data$data, 
          .f = function(i) calc_ldiversity(i$y)
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c("nodes", "yhat", "ldiversity")))
    
    # unnest the sampled values
    sampled_values <- sampled_values %>%
      tidyr::unnest(cols = c("yhat", "ldiversity")) %>%
      dplyr::arrange(.data$nodes)
    
    # combine and sort so the predicted values match the order of the synthetic 
    # data
    sampled_values <- 
      dplyr::bind_cols(
        nodes_synth,
        sampled_values,
      ) %>%
      dplyr::arrange(.data$index)
    
    # pull and return the vectors or interest
    y_hat <- sampled_values$yhat

    ldiversity_conf <- sampled_values$ldiversity
    
  }
  
  return(list(y_hat = y_hat, ldiversity = ldiversity_conf))
  
}