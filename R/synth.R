#'
#' Derive the object-level `state` for a `synth` object from its `roles`
#'
#' @param roles A named vector of `presynth`/`synth` roles.
#'
#' @return A single string: one of `"presynth"`, `"incomplete"`, `"fitted"`,
#'   or `"postsynth"`.
#' @noRd
#'
.derive_synth_state <- function(roles) {

  # "start" variables never change role, so only visit-sequence roles inform
  # the overall fit/sample state of the object
  seq_roles <- roles[roles != "start"]

  if (length(seq_roles) == 0 || all(seq_roles == "unsynthesized")) {

    return("presynth")

  } else if (all(seq_roles == "fitted")) {

    return("fitted")

  } else if (all(seq_roles == "synthesized")) {

    return("postsynth")

  } else {

    return("incomplete")

  }

}

#'
#' Create a synth object
#'
#' `synth` objects hold the in-progress state between fitting and sampling
#' models one variable at a time (see `synth_fit()` and `synth_sample()`).
#' They are automatically created by `presynth()`; package users should
#' rarely need to manually create instances.
#'
#' @param roadmap A `roadmap` object.
#' @param synth_spec A `synth_spec` object.
#' @param cache_path An optional directory path used to cache fitted models,
#'   one file per variable. Defaults to `NULL` (no caching).
#' @param cache_valid A logical for whether existing cache files, if any,
#'   should be treated as valid. Defaults to `TRUE`.
#' @param discard_after_cache A logical for whether a fitted model is
#'   discarded from memory immediately after being cached. Defaults to
#'   `TRUE`, which bounds peak memory to one fitted model at a time.
#'
#' @return A `synth` object.
#' @noRd
#'
synth <- function(roadmap,
                  synth_spec,
                  cache_path = NULL,
                  cache_valid = TRUE,
                  discard_after_cache = TRUE) {

  # test inputs before using roadmap/synth_spec to build workflows/roles
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`synth_spec` must be a synth_spec object" = { is_synth_spec(synth_spec) }
  )

  # first, enforce schema if needed
  if (roadmap[["schema"]][["enforce"]]) {

    roadmap <- enforce_schema(roadmap)

  }

  # create workflows and roles exactly like new_presynth()
  workflows <- .construct_workflows(roadmap, synth_spec)
  roles <- .init_roles(roadmap)

  synth <- new_synth(
    roadmap = roadmap,
    synth_spec = synth_spec,
    workflows = workflows,
    roles = roles,
    cache_path = cache_path,
    cache_valid = cache_valid,
    discard_after_cache = discard_after_cache
  )

  return(synth)

}

#'
#' `synth` constructor
#'
#' Unlike `synth()`, `new_synth()` accepts `workflows` and `roles` directly
#' rather than constructing them from `roadmap`/`synth_spec`. This lets
#' `synth_fit()`/`synth_sample()` (and tests) construct or update a `synth`
#' object at any point along its fit/sample lifecycle.
#'
#' @param roadmap A `roadmap` object.
#' @param synth_spec A `synth_spec` object.
#' @param workflows A named list of constructed synthesis components.
#' @param roles A named vector mapping variable names to roles.
#' @param fitted_models An optional named list, keyed by visit-sequence
#'   variable, holding fitted `workflows::workflow` objects. Defaults to a
#'   list of `NULL` entries, one per visit-sequence variable.
#' @param synthetic_data An optional synthetic data set. Defaults to an
#'   empty tibble.
#' @param jth_preprocessing An optional list of recipes from `library(recipe)`.
#'   Defaults to an empty list.
#' @param total_synthesis_time An optional double with the total time of
#'   synthesis.
#' @param jth_synthesis_time An optional vector or dataframe of synthesis
#'   times.
#' @param extractions An optional list with extracted output from the
#'   synthesis model. Defaults to an empty list.
#' @param ldiversity An optional vector or dataframe of ldiversity stats.
#' @param cache_path An optional directory path used to cache fitted models.
#' @param cache_valid A logical for whether existing cache files, if any,
#'   should be treated as valid. Defaults to `TRUE`.
#' @param discard_after_cache A logical for whether a fitted model is
#'   discarded from memory immediately after being cached. Defaults to
#'   `TRUE`.
#'
#' @return A `synth` object.
#' @noRd
#'
new_synth <- function(roadmap,
                      synth_spec,
                      workflows,
                      roles,
                      fitted_models = NULL,
                      synthetic_data = NULL,
                      jth_preprocessing = NULL,
                      total_synthesis_time = NULL,
                      jth_synthesis_time = NULL,
                      extractions = NULL,
                      ldiversity = NULL,
                      cache_path = NULL,
                      cache_valid = TRUE,
                      discard_after_cache = TRUE) {

  # test inputs
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`synth_spec` must be a synth_spec object" = { is_synth_spec(synth_spec) }
  )

  visit_vars <- roadmap[["visit_sequence"]][["visit_sequence"]]

  # default fitted_models to a NULL entry per visit-sequence variable
  if (is.null(fitted_models)) {

    fitted_models <- stats::setNames(
      vector(mode = "list", length = length(visit_vars)),
      visit_vars
    )

  }

  if (is.null(synthetic_data)) {

    synthetic_data <- tibble::tibble()

  }

  if (is.null(jth_preprocessing)) {

    jth_preprocessing <- list()

  }

  if (is.null(extractions)) {

    extractions <- list()

  }

  # create synth
  synth <- list(
    roadmap = roadmap,
    synth_spec = synth_spec,
    workflows = workflows,
    roles = roles,
    fitted_models = fitted_models,
    synthetic_data = synthetic_data,
    jth_preprocessing = jth_preprocessing,
    total_synthesis_time = total_synthesis_time,
    jth_synthesis_time = jth_synthesis_time,
    extractions = extractions,
    ldiversity = ldiversity,
    cache_path = cache_path,
    cache_valid = cache_valid,
    discard_after_cache = discard_after_cache,
    state = .derive_synth_state(roles)
  )

  # `synth` inherits `postsynth` and `presynth` so that `is_presynth()` and
  # `is_postsynth()` remain `TRUE` for a `synth` object, keeping the existing
  # `presynth()` + `synthesize()` API backward-compatible
  synth <- structure(synth, class = c("synth", "postsynth", "presynth"))

  return(synth)

}

#'
#' Check if object is `synth`
#'
#' @param x Object
#' @return Logical
#' @noRd
#'
is_synth <- function(x) {
  inherits(x, "synth")
}

#'
#' `synth` validator
#'
#' Ensures internal consistency of a `synth` object, raising an error if not.
#' Called lazily (e.g. before fitting/sampling), not at construction, per the
#' pattern of `.validate_presynth()`.
#'
#' @param synth A `synth` object
#'
#' @return NULL
#' @noRd
#'
.validate_synth <- function(synth) {

  stopifnot("`synth` must be a synth object" = { is_synth(synth) })

  # reuse presynth-level checks (workflow lengths/names, model-type matching)
  .validate_presynth(synth)

  visit_vars <- synth[["roadmap"]][["visit_sequence"]][["visit_sequence"]]

  stopifnot(
    "`state` must be one of \"presynth\", \"incomplete\", \"fitted\", or \"postsynth\"" = {
      synth[["state"]] %in% c("presynth", "incomplete", "fitted", "postsynth")
    },
    "`discard_after_cache` must be a single logical value" = {
      is.logical(synth[["discard_after_cache"]]) &&
        length(synth[["discard_after_cache"]]) == 1 &&
        !is.na(synth[["discard_after_cache"]])
    },
    "`cache_valid` must be a single logical value" = {
      is.logical(synth[["cache_valid"]]) &&
        length(synth[["cache_valid"]]) == 1 &&
        !is.na(synth[["cache_valid"]])
    },
    "`cache_path` must be NULL or a single string" = {
      is.null(synth[["cache_path"]]) ||
        (is.character(synth[["cache_path"]]) && length(synth[["cache_path"]]) == 1)
    },
    "`fitted_models` must be a named list matching the visit sequence" = {
      is.list(synth[["fitted_models"]]) &&
        setequal(names(synth[["fitted_models"]]), visit_vars)
    }
  )

  return(invisible(NULL))

}

#' Print the synth object to the console with formatting
#'
#' @param x A `synth` object
#' @param ... further arguments passed to or from other methods (not
#'   currently used).
#'
#' @return Invisibly returns the input `synth` object.
#'
#' @examples
#'
#' # create roadmap
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
#' # `synth()` is an internal constructor (not exported); `synth` objects are
#' # created automatically, e.g. by `presynth()`.
#' # create a synth object (empty synthetic_data, state == "presynth")
#' synth1 <- tidysynthesis:::synth(
#'   roadmap = rm,
#'   synth_spec = synth_spec1
#' )
#'
#' print(synth1)
#'
#' @export
print.synth <- function(x, ...) {

  cat("Synth \n")
  cat(base::sprintf("State: %s \n", x[["state"]]))

  if (x[["state"]] == "presynth") {

    print(x[["roadmap"]])
    print(x[["synth_spec"]])

  } else if (x[["state"]] %in% c("incomplete", "fitted")) {

    cat(
      base::sprintf(
        "Variables: %s unsynthesized, %s fitted, %s synthesized \n",
        sum(x[["roles"]] == "unsynthesized"),
        sum(x[["roles"]] == "fitted"),
        sum(x[["roles"]] == "synthesized")
      )
    )

  } else {

    cat(
      base::sprintf(
        "Synthetic Data: %s synthetic observations, %s variables \n",
        dim(x[["synthetic_data"]])[1],
        dim(x[["synthetic_data"]])[2]
      )
    )
    cat(base::sprintf("Total Synthesis Time: %s seconds",
                      x[["total_synthesis_time"]]))

  }

  invisible(x)

}
