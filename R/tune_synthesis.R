#' Generate syntheses from multiple `presynth` objects.
#'
#' @param presynths A list of `presynth` objects
#' @param postprocessing_func A function with arguments "synth_id", "synth_name", 
#' and "postsynth" that performs any desired postprocessing operations, like writing
#" synthetic data to file or computing privacy-utility metrics. 
#' @param metadata_func An optional function with argument "presynth" that extracts
#' specified information from each `presynth` object and returns a list. Each list
#' element becomes an additional column in the output metadata.
#' @param simplify_post Boolean that, if true, expects `postprocessing_func` to 
#' return a list corresponding to the row of the output dataframe (one per synthesis).
#' @param seed A RNG seed to pass to `set.seed()`
#'
#' @return A `post_tunesynth` object.
#' 
#' @examples
#' 
#' rm <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' ) 
#' 
#' dt_mod_reg <- parsnip::decision_tree() |>
#'   parsnip::set_engine(engine = "rpart") |>
#'   parsnip::set_mode(mode = "regression")
#' 
#' dt_mod_reg_cp <- parsnip::decision_tree(cost_complexity = 0.01) |>
#'   parsnip::set_engine(engine = "rpart") |>
#'   parsnip::set_mode(mode = "regression")
#' 
#' dt_mod_class <- parsnip::decision_tree() |>
#'   parsnip::set_engine(engine = "rpart") |>
#'   parsnip::set_mode(mode = "classification")
#' 
#' # synth specs
#' synth_spec1 <- synth_spec(
#'   default_regression_model = dt_mod_reg,
#'   default_regression_sampler = sample_rpart,
#'   default_classification_model = dt_mod_class,
#'   default_classification_sampler = sample_rpart
#' )
#' 
#' synth_spec2 <- synth_spec(
#'   default_regression_model = dt_mod_reg_cp,
#'   default_regression_sampler = sample_rpart,
#'   default_classification_model = dt_mod_class,
#'   default_classification_sampler = sample_rpart
#' )
#' 
#' 
#' presynth1 <- presynth(
#'   roadmap = rm,
#'   synth_spec = synth_spec1
#' )
#' 
#' presynth2 <- presynth(
#'   roadmap = rm,
#'   synth_spec = synth_spec2
#' )
#' 
#' postproc_f_null <- function(synth_id, synth_name, postsynth) {
#'   return(postsynth[["synthetic_data"]])
#' }
#' 
#' tune_synthesis(
#'   presynths = list(presynth1, presynth2),
#'   postprocessing_func = postproc_f_null,
#'   seed = 12345
#' )
#' 
#' 
#' 
#' @export
tune_synthesis <- function(presynths, 
                           postprocessing_func,
                           metadata_func = NULL,
                           simplify_post = FALSE,
                           seed = NULL) {
  
  # check types and function arguments
  stopifnot(
    "`presynths` elements must be presynth objects" = {
      all("presynth" %in% purrr::map(presynths, ~ class(.)))
    }
  )
  
  stopifnot(
    "`postprocessing_func` must have required arguments: synth_id, synth_name, 
    and postsynth" = {
      all(
        c("synth_id", "synth_name", "postsynth") %in% 
          names(formals(postprocessing_func))
      )
    }
  )
  
  if (!is.null(metadata_func)) {
    
    stopifnot(
      "`metadata_func` must have required argument presynth" = {
        "presynth" %in% names(formals(metadata_func))
      } 
    )
    
  }
  
  if (is.null(seed)) {
    
    seed <- 123
    warning("Using default seed = 123; specify RNG seed manually for alternative")
    
  }
  
  set.seed(seed)
  
  # create synth IDs
  synth_ids <- seq(length(presynths))
  
  # name presynths if not already present
  if (is.null(names(presynths))) {
    
    synth_names <- paste0(
      "synth_", stringr::str_pad(synth_ids, width = 4, pad = "0")
    )
    
  } else {
    
    synth_names <- names(presynths)
    
  }
  
  # add metadata to presynths
  metadata <- dplyr::bind_cols(
    "id" = synth_ids, 
    "name" = synth_names
  )
  
  if (!is.null(metadata_func)) {
    
    metadata <- dplyr::bind_cols(
      metadata, 
      dplyr::bind_rows(
        purrr::map(presynths, metadata_func))
    )
    
  }
  
  # helper function for ith synthesis
  synth_i <- function(synth_id, synth_name, presynth) {
    
    # call synthesize
    synth <- synthesize(presynth)
    
    # return output of postprocessing function
    results <- postprocessing_func(synth_id = synth_id, 
                                   synth_name = synth_name, 
                                   postsynth = synth)
    return(results)
    
  }
  
  # apply to all presynths
  results <- purrr::pmap(
    list("synth_id" = synth_ids, 
         "synth_name" = synth_names,
         "presynth" = presynths), 
    synth_i
  )
  
  if (simplify_post) {
    
    results <- dplyr::bind_cols(
      metadata, 
      dplyr::bind_rows(results)
    )
    
  }
  
  post_tunesynth <- list(
    metadata = metadata,
    results = results
  )
  
  post_tunesynth <- structure(post_tunesynth, 
                              class = "post_tunesynth")
  
  return(post_tunesynth)
  
}

