#' Fit the models of a synth object, optionally caching them to disk
#'
#' Fits the synthesis models for the requested variables, one variable at a
#' time, without sampling any synthetic data. Fitted models can be cached to
#' disk, one `.rds` file per variable, so that a later call can reuse them
#' instead of refitting and so that peak memory is bounded by one fitted model.
#'
#' Each cache file holds a fingerprint of the `roadmap` and `synth_spec`
#' alongside the fitted model and its extraction. An existing cache file is 
#' reused only when `cache_valid = TRUE` and its fingerprint matches the
#' current `roadmap` and `synth_spec`; otherwise the file is deleted and the
#' model is refit. A `synth` without a `roadmap` or `synth_spec` cannot be
#' fingerprinted or refit, so its existing cache files are trusted.
#' 
#' Functions in the `synth_spec`, such as samplers and extractors, are 
#' fingerprinted by their arguments and code only. A function can also depend on 
#' variables from the place where it was created, for example `k` in 
#' `make_sampler <- function(k) { function(x) { x + k } }`, where 
#' `make_sampler(5)` and `make_sampler(10)` have the same code but behave 
#' differently. Changing such a variable does not change the fingerprint, so 
#' cached models are reused. Use `cache_valid = FALSE` to force a refit in that 
#' case.
#'
#' @param synth A `synth` object, such as the output of `presynth()`.
#' @param j An optional single variable name to fit. Defaults to `NULL`, which
#'   fits every variable that has not yet been fitted, in visit sequence order.
#' @param cache_path An optional directory path for caching fitted models, one
#'   file per variable. The directory is created if it does not exist. Defaults
#'   to `NULL`, which does no caching.
#' @param cache_valid A logical for whether existing cache files, if any, 
#'   should be treated as valid. If `FALSE`, existing cache files are replaced.
#'   Defaults to `TRUE`.
#' @param discard_after_cache A logical for whether a fitted model is 
#'   discarded from memory after it is cached. Defaults to `TRUE`. Models are
#'   always retained when `cache_path` is `NULL`.
#'
#' @return A `synth` object with the fitted variables marked `"fitted"`, the
#'   `extractions` updated, and the caching arguments recorded.
#'
#' @examples
#' 
#' rm <- roadmap(
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
#' presynth1 <- presynth(roadmap = rm, synth_spec = synth_spec1)
#' 
#' fitted1 <- synth_fit(presynth1)
#' 
#' @export
synth_fit <- function(synth,
                      j = NULL,
                      cache_path = NULL,
                      cache_valid = TRUE,
                      discard_after_cache = TRUE) {
  
  stopifnot(
    "`synth` must be a synth object" = { is_synth(synth) },
    "`cache_path` must be NULL or a single string" = {
      is.null(cache_path) || 
        (is.character(cache_path) && length(cache_path) == 1 && 
           !is.na(cache_path))
    },
    "`cache_valid` must be a single logical value" = {
      is.logical(cache_valid) && length(cache_valid) == 1 && !is.na(cache_valid)
    },
    "`discard_after_cache` must be a single logical value" = {
      is.logical(discard_after_cache) && length(discard_after_cache) == 1 && 
        !is.na(discard_after_cache)
    }
  )
  
  roles <- synth[["roles"]]
  vs_names <- names(synth[["workflows"]][["built_models"]])
  has_spec <- !is.null(synth[["roadmap"]]) && !is.null(synth[["synth_spec"]])
  
  if (has_spec) {
    
    .validate_synth(synth)
    
  }
  
  # resolve the variables to fit, in visit sequence order
  if (is.null(j)) {
    
    j <- vs_names[roles[vs_names] == "unsynthesized"]
    
  } else {
    
    stopifnot(
      "`j` must be a single visit sequence variable name" = {
        is.character(j) && length(j) == 1 && j %in% vs_names
      },
      "`j` must not be a variable that is already synthesized" = {
        roles[[j]] != "synthesized"
      }
    )
    
  }
  
  if (!is.null(cache_path) && length(j) > 0) {
    
    dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
    
  }
  
  # models and extractions are always retained in memory without a cache
  discard <- !is.null(cache_path) && discard_after_cache
  
  fingerprint <- NULL
  conf_data <- NULL
  
  if (has_spec) {
    
    fingerprint <- .synth_fingerprint(
      list(roadmap = synth[["roadmap"]], synth_spec = synth[["synth_spec"]])
    )
    
    conf_data <- .drop_empty_levels(synth[["roadmap"]][["conf_data"]])
    
  }
  
  for (var_j in j) {
    
    idx <- match(var_j, vs_names)
    cache_file <- if (is.null(cache_path)) {
      NULL
    } else {
      file.path(cache_path, paste0(var_j, ".rds"))
    }
    
    cached <- NULL
    
    if (!is.null(cache_file) && file.exists(cache_file)) {
      
      if (!has_spec) {
        
        cached <- readRDS(cache_file)
        
      } else if (cache_valid) {
        
        cached <- readRDS(cache_file)
        
        if (!identical(cached[["fingerprint"]], fingerprint)) {
          
          cached <- NULL
          
        }
        
      }
      
      if (is.null(cached)) {
        
        unlink(cache_file)
        
      }
      
    }
    
    if (is.null(cached)) {
      
      stopifnot(
        "Without a cached model, `synth` needs a `roadmap` and `synth_spec` to fit from" = { 
          has_spec 
        }
      )
      
      message(paste0("Fitting ", idx, "/", length(vs_names), " ", var_j, "... "))
      
      fit <- .fit_j(
        conf_data = conf_data,
        col_schema = synth[["workflows"]][["built_col_schema"]][[idx]],
        model = synth[["workflows"]][["built_models"]][[idx]],
        recipe = synth[["workflows"]][["built_recipes"]][[idx]],
        tuner = synth[["workflows"]][["built_tuners"]][[idx]],
        extractor = synth[["workflows"]][["built_extractors"]][[idx]]
      )
      
      cached <- list(
        fingerprint = fingerprint,
        fitted = fit[["fitted"]],
        extraction = fit[["extraction"]]
      )
      
      if (!is.null(cache_file)) {
        
        saveRDS(cached, file = cache_file)
        
      }
      
    }
    
    # `[<-` with `list(NULL)` keeps the entry instead of deleting it
    if (discard) {
      
      synth[["fitted_models"]][var_j] <- list(NULL)
      
    } else {
      
      synth[["fitted_models"]][var_j] <- list(cached[["fitted"]])
      
    }
    
    synth[["extractions"]][var_j] <- list(cached[["extraction"]])
    roles[[var_j]] <- "fitted"
    
    # free the model before fitting the next variable
    rm(cached)
    
  }
  
  synth[["roles"]] <- roles
  synth[["state"]] <- .derive_synth_state(roles)
  synth[["cache_path"]] <- cache_path
  synth[["cache_valid"]] <- cache_valid
  synth[["discard_after_cache"]] <- discard_after_cache
  
  return(synth)
  
}

#' Drop empty factor levels from confidential data
#'
#' @param conf_data A data frame of confidential data.
#'
#' @return A data frame
#' @noRd
#'
.drop_empty_levels <- function(conf_data) {
  
  conf_data <- dplyr::mutate(
    conf_data,
    dplyr::across(dplyr::where(is.factor), droplevels)
  )
  
  return(conf_data)
  
}

#' Strip session-specific detail from an object so it hashes reproducibly
#' 
#' Functions are replaced by their formals and body, which are unaffected by
#' `options(keep.source)`, and environments, which vary by session, are
#' dropped. Quosures and formulas keep only their expression.
#'
#' @param x An object.
#'
#' @return A normalized object.
#' @noRd
#'
.normalize_for_hash <- function(x) {
  
  if (is.function(x)) {
    
    if (is.primitive(x)) {
      
      return(list(primitive = deparse(x)))
      
    }
    
    return(
      list(
        formals = deparse(formals(x)),
        body = deparse(body(x))
      )
    )
    
  } else if (rlang::is_quosure(x)) {
    
    return(list(quosure = deparse(rlang::quo_get_expr(x))))
    
  } else if (inherits(x, "formula") || is.language(x)) {
    
    return(list(expression = deparse(x)))
    
  } else if (is.environment(x)) {
    
    return("environment")
    
  } else if (is.data.frame(x)) {
    
    return(x)
    
  } else if (is.list(x)) {
    
    # preserves names and attributes such as class
    x[] <- lapply(x, .normalize_for_hash)
    
    return(x)
    
  }
  
  return(x)
  
}

#' Fingerprint a synthesis specification
#'
#' @param x An object, usually a list with a `roadmap` and `synth_spec`.
#'
#' @return A string hash that is stable across sessions and `keep.source`
#'   settings, and changes when data or function logic changes.
#' @noRd
#'
.synth_fingerprint <- function(x) {
  
  return(rlang::hash(.normalize_for_hash(x)))
  
}
