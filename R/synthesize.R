#' Synthesize a data set
#'
#' @param presynth A `presynth` object created by `presynth()`.
#' @param progress A single logical. Should a progress be displayed?
#'
#' @return A postsynth object.
#' 
#' @examples
#' 
#' # create roadmap
#' roadmap <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' ) 
#' 
#' rpart_mod_reg <- parsnip::decision_tree() |>
#'   parsnip::set_engine(engine = "rpart") |>
#'   parsnip::set_mode(mode = "regression")
#' 
#' rpart_mod_class <- parsnip::decision_tree() |>
#'   parsnip::set_engine(engine = "rpart") |>
#'   parsnip::set_mode(mode = "classification")
#' 
#' synth_spec1 <- synth_spec(
#'   default_regression_model = rpart_mod_reg,
#'   default_regression_sampler = sample_rpart,
#'   default_classification_model = rpart_mod_class,
#'   default_classification_sampler = sample_rpart
#' )
#' 
#' # create a presynth object
#' # use defaults for noise, constraints, and replicates
#' presynth1 <- presynth(
#'   roadmap = roadmap,
#'   synth_spec = synth_spec1
#' )
#' 
#' # synthesize!
#' set.seed(1)
#' postsynth1 <- synthesize(presynth = presynth1)
#' 
#' @export
synthesize <- function(presynth, progress = FALSE) {
  
  # this block is exclusively used to handle end-to-end replicates
  # all main functionality occurs below in .synthesize()
  end2end_reps <- presynth$roadmap$replicates$end_to_end_replicates
  
  # when no end-to-end replicates provided...
  if (end2end_reps == 1) {
    
    # use default synthesis
    return(.synthesize(presynth = presynth, progress = progress))
    
  } else {
    
    # else, return list of end-to-end replicate syntheses
    return(
      purrr::map(
        .x = 1:end2end_reps,
        .f = \(x) {
          .synthesize(presynth = presynth, progress = progress)
        }
      )
    )
    
  }
  
}

.synthesize <- function(presynth, progress = FALSE) {
  
  # start overall synthesis time
  synth_start_time <- Sys.time()
  
  # check presynth
  stopifnot(
    "`presynth` must be a presynth object" = { is_presynth(presynth) } 
  )
  
  # unpack roadmap data
  conf_data <- presynth$roadmap$conf_data
  start_data <- presynth$roadmap$start_data
  start_vars <- names(start_data)
  vs_names <- presynth$roadmap$visit_sequence$visit_sequence
  vs_len <- length(vs_names)
  
  # create a vector of synthesis time
  jth_synthesis_time <- vector(mode = "numeric", length = vs_len)
  names(jth_synthesis_time) <- vs_names
  
  extractions <- vector(mode = "list", length = vs_len)
  names(extractions) <- vs_names
  
  # create a list of preprocessing parameters
  jth_preprocessing <- vector(mode = "list", length = vs_len)
  names(jth_preprocessing) <- vs_names
  
  # create a list of ldiversity
  ldiversity <- vector(mode = "list", length = vs_len)
  ldiversity <- purrr::map(
    .x = ldiversity, 
    .f = \(x) { rep(NA_integer_, nrow(conf_data)) }
  )
  names(ldiversity) <- vs_names
  
  # process start_data according to start_method
  processed_start_data <- exec_start_method(presynth$roadmap)
  
  # save empty factor levels then drop empty factor levels
  full_factor_levels <- NULL
  all_var_types <- purrr::map(
    .x = presynth$roadmap$schema$col_schema, 
    .f = ~ .x[["dtype"]]
    ) %>% 
      purrr::flatten_chr()
  
  if ("fct" %in% all_var_types) {
    
    # subset to factors
    conf_data_subset <- dplyr::select(conf_data, dplyr::where(is.factor)) 
    
    # find empty levels
    empty_levels_lgl <- purrr::map_lgl(
      .x = conf_data_subset, 
      .f = \(.x) { setequal(levels(.x), unique(.x)) }
    ) 
    empty_levels_names <- names(empty_levels_lgl)[!empty_levels_lgl]
    
    if (length(empty_levels_names) > 0) {
      
      # subset to factors with empty levels
      conf_data_empty <- dplyr::select(conf_data_subset, 
                                       dplyr::any_of(empty_levels_names))
      
      # store full factor levels
      full_factor_levels <- purrr::map(.x = conf_data_empty, .f = levels)
      
      # drop empty levels
      conf_data <- dplyr::mutate(
        conf_data, 
        dplyr::across(names(full_factor_levels), droplevels)
      )
      
      processed_start_data <- dplyr::mutate(
        processed_start_data, 
        dplyr::across(dplyr::any_of(names(full_factor_levels)), droplevels)
      )
      
    }
    
  }
  
  # iterate model estimation and prediction  
  synth_data <- processed_start_data
  
  # wrapper function to pass to furrr::map
  synthesizer_engine <- function(replicates,
                                 conf_data,
                                 synth_data,
                                 col_schema,
                                 models,
                                 recipes,
                                 samplers,
                                 noises,
                                 tuners,
                                 extractors,
                                 invert_transformations,
                                 constraints,
                                 jth_preprocessing,
                                 jth_synthesis_time,
                                 ldiversity,
                                 p) {
    
    engine_output <- list(synth_data = synth_data,
                          jth_preprocessing = jth_preprocessing,
                          jth_synthesis_time = jth_synthesis_time,
                          extractions = extractions,
                          ldiversity = ldiversity)
    
    for (var_j in seq_along(models)) {
      
      message(paste0("Synthesizing ", var_j, "/", length(vs_names), " ", vs_names[[var_j]], "... "))
      
      jth_variable <- synthesize_j(
        conf_data = conf_data,
        synth_data = engine_output$synth_data,
        col_schema = col_schema[[var_j]], 
        model = models[[var_j]],
        recipe = recipes[[var_j]],
        sampler = samplers[[var_j]],
        noise = noises[[var_j]],
        tuner = tuners[[var_j]],
        extractor = extractors[[var_j]],
        constraints = constraints[[var_j]],
        invert_transformations = invert_transformations,
        p = p
      )
      
      # put together synthetic data set
      engine_output$synth_data <- dplyr::bind_cols(
        engine_output$synth_data, jth_variable$predictions
      )
      
      # use _NA variables to add NA to their corresponding variables
      if (presynth$synth_spec$enforce_na) {
        
        engine_output$synth_data <- enforce_na(data = engine_output$synth_data)
        
      }
      
      # add estimated model for the jth variable
      engine_output$jth_preprocessing[[var_j]] <- (
        jth_variable[["estimated_model"]][["pre"]][["mold"]][[
          "blueprint"]][["recipe"]]
      )
      
      # add synthesis time for the jth variable
      engine_output$jth_synthesis_time[var_j] <- jth_variable$jth_synthesis_time
      
      engine_output$extractions[[var_j]] <- jth_variable$extraction
      
      # add ldiversity for the jth variable
      if (!is.null(jth_variable$ldiversity)) {
        
        engine_output$ldiversity[[var_j]] <- jth_variable$ldiversity
        
      }
        
    }
    
    return(engine_output)
    
  }
  
  # set args for synthesizer engine
  
  # Note that this is passed into `pmap()` which takes a list of elements.
  # `replicates` is a vector of length `model_sample_replicates`, 
  # which results in the code iterating that many times. 
  # All other elements are length 1, and they are repeated for each iteration.
  
  synth_args <- list(
    replicates = seq_len(presynth$roadmap$replicates$model_sample_replicates),
    conf_data = list(conf_data),
    synth_data = list(synth_data),
    col_schema = list(presynth$workflows$built_col_schema),
    models = list(presynth$workflows$built_models),
    recipes = list(presynth$workflows$built_recipes),
    samplers = list(presynth$workflows$built_samplers),
    noises = list(presynth$workflows$built_noises),
    tuners = list(presynth$workflows$built_tuners),
    extractors = list(presynth$workflows$built_extractors),
    invert_transformations = list(presynth$synth_spec$invert_transformations),
    constraints = list(presynth$workflows$built_constraints),
    jth_preprocessing = list(jth_preprocessing),
    jth_synthesis_time = list(jth_synthesis_time),
    ldiversity = list(ldiversity)
  )
  
  # run synthesis
  
  # future::plan(future::multisession)
  # .options <- set_furrr_options(seed = TRUE)
  
  if (progress) {
  
  progressr::with_progress({
    
    p <- progressr::progressor(steps = length(synth_args$col_schema[[1]]))
    
    syntheses <- purrr::pmap(
      .l = synth_args, 
      .f = synthesizer_engine,
      # .options = .options,
      p = p
    )
    
  })
    
  } else {
    
    p <- function() NULL
    
    syntheses <- purrr::pmap(
      .l = synth_args, 
      .f = synthesizer_engine,
      # .options = .options,
      p = p
    )
    
  }
  
  # future::plan(future::sequential)
  
  # add back empty factor levels
  # 
  # @param x - a dataframe of synthetic data 
  # @param var - a character variable reflecting a column in the synthetic data 
  # inputted in x 
  # @param levels_lookup - a named list whose names are factor variables from 
  # the synthetic data inputted in x and whose values are a vector of character 
  # strings representing the levels those factors can take in the confidential 
  # data
  add_levels <- function(x, var, levels_lookup) {

    if (!var %in% names(levels_lookup)) {

      return(x)

    }

    levels(x) <- levels_lookup[[var]]

    return(x)
    
  }
  
  for (replicate_i in seq_along(syntheses)) {
    
    syntheses[[replicate_i]]$synth_data <- purrr::map2_dfr(
      .x = syntheses[[replicate_i]]$synth_data, 
      .y = names(syntheses[[replicate_i]]$synth_data),
      .f = add_levels, 
      levels_lookup = full_factor_levels
    )
    
  }
  
  # if only one replicate, return a postsynth object directly
  if (length(syntheses) == 1) {
    
    syntheses <- syntheses[[1]] 
    
    synth_end_time <- Sys.time()
    
    jth_synthesis_time <- tibble::tibble(
      j = 1:length(syntheses$jth_synthesis_time),
      variable = factor(x = names(syntheses$jth_synthesis_time), 
                        levels = names(syntheses$jth_synthesis_time), 
                        ordered = TRUE),
      synthesis_time = unname(syntheses$jth_synthesis_time)
    )
    
    # create postsynth object
    postsynth <- postsynth(
      synthetic_data = syntheses$synth_data,
      jth_preprocessing = syntheses$jth_preprocessing,
      total_synthesis_time = as.numeric(synth_end_time - synth_start_time, 
                                        units = "secs"),
      jth_synthesis_time = jth_synthesis_time,
      extractions = syntheses$extractions,
      ldiversity = tibble::as_tibble(data.frame(syntheses$ldiversity))
    )
    
    return(postsynth)
    
  }
  
  # multiple replicates
  if (length(syntheses) > 1) {
    
    postsynths <- vector(mode = "list", length = length(syntheses))
    
    for (replicate_number in seq_along(postsynths)) {
    
      synth_end_time <- Sys.time()
      as.numeric(synth_end_time - synth_start_time, units = "secs")
      
      # create the postsynth object
      postsynths[[replicate_number]] <- postsynth(
        synthetic_data = syntheses[[replicate_number]]$synth_data,
        jth_preprocessing = syntheses[[replicate_number]]$jth_preprocessing,
        total_synthesis_time = as.numeric(synth_end_time - synth_start_time, 
                                          units = "secs"),
        jth_synthesis_time = syntheses[[replicate_number]]$jth_synthesis_time,
        extractions = syntheses[[replicate_number]]$extractions,
        ldiversity = tibble::as_tibble(
          data.frame(syntheses[[replicate_number]]$ldiversity)
        )
      )

    }
  
    return(postsynths)
    
  }

}
