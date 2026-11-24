# set up
start_data <- dplyr::select(mtcars, cyl, vs, am, gear, carb)

roadmap <- roadmap(conf_data = mtcars,
                   start_data = start_data) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

dt_mod <- parsnip::decision_tree() |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")

synth_spec <- synth_spec(default_regression_model = dt_mod,
                         default_regression_sampler = sample_rpart,
                         default_extractor = workflows::extract_fit_parsnip)

new_test_synth <- function(rm = roadmap, ss = synth_spec) {
  
  suppressWarnings(
    synth <- presynth(roadmap = rm, synth_spec = ss)
  )
  
  return(synth)
  
}

# a fresh, uniquely named cache directory inside the session's tempdir()
new_cache_dir <- function(name) {
  
  cache_dir <- file.path(tempdir(), name)
  unlink(cache_dir, recursive = TRUE)
  
  return(cache_dir)
  
}

vs_names <- roadmap[["visit_sequence"]][["visit_sequence"]]

# tests -------------------------------------------------------------------

test_that("synth_fit() fits all unsynthesized variables by default", {
  
  synth <- new_test_synth()
  
  # with no `j`, every visit sequence variable is fitted
  fitted <- synth_fit(synth)
  
  expect_true(is_synth(fitted))
  expect_true(all(fitted[["roles"]][vs_names] == "fitted"))
  expect_equal(fitted[["state"]], "fitted")
  
  # no cache path, so models must be retained
  expect_equal(names(fitted[["fitted_models"]]), vs_names)
  
  for (n in vs_names) {
    
    expect_true(workflows::is_trained_workflow(fitted[["fitted_models"]][[n]]))
    expect_s3_class(fitted[["extractions"]][[n]], "model_fit")
    
  }
  
  # start variables and synthetic data are untouched
  expect_true(all(fitted[["roles"]][names(start_data)] == "start"))
  expect_equal(nrow(fitted[["synthetic_data"]]), 0)
  
})

test_that("synth_fit() fits only the requested variables", {
  
  synth <- new_test_synth()
  
  # fit one variable: only its role changes and the state is "incomplete"
  fitted <- synth_fit(synth, j = vs_names[2])
  
  expect_equal(fitted[["roles"]][[vs_names[2]]], "fitted")
  expect_true(all(fitted[["roles"]][vs_names[-2]] == "unsynthesized"))
  expect_equal(fitted[["state"]], "incomplete")
  
  expect_true(
    workflows::is_trained_workflow(fitted[["fitted_models"]][[vs_names[2]]])
  )
  expect_null(fitted[["fitted_models"]][[vs_names[1]]])
  
  # fitting the remaining variables by default skips the fitted one, so its
  # model is unchanged
  fitted2 <- synth_fit(fitted)
  
  expect_equal(fitted2[["state"]], "fitted")
  expect_identical(
    fitted2[["fitted_models"]][[vs_names[2]]],
    fitted[["fitted_models"]][[vs_names[2]]]
  )
  
})

test_that("synth_fit() validates inputs", {
  
  synth <- new_test_synth()
  
  # each invalid argument is rejected with a message naming it
  expect_error(synth_fit(synth = list()), "`synth` must be a synth object")
  # `j` must be NULL or a single visit sequence variable
  expect_error(synth_fit(synth, j = "not_a_variable"), "`j` must")
  expect_error(synth_fit(synth, j = vs_names[1:2]), "`j` must")
  expect_error(synth_fit(synth, discard_after_cache = NA), "`discard_after_cache`")
  expect_error(synth_fit(synth, cache_valid = "yes"), "`cache_valid`")
  
  # synthesized variables cannot be refit
  synth[["roles"]][[vs_names[1]]] <- "synthesized"
  
  expect_error(synth_fit(synth, j = vs_names[1]), "synthesized")
  
})

test_that("synth_fit() records no cache path when none is supplied", {
  
  # without a cache path nothing is cached, and none is recorded on the object
  fitted <- synth_fit(new_test_synth())
  
  expect_null(fitted[["cache_path"]])
  
})

test_that("synth_fit() writes one cache file per variable and discards models", {
  
  cache_dir <- new_cache_dir("synth_fit_discard")
  synth <- new_test_synth()
  
  # fit with a cache path and the default discard_after_cache = TRUE
  fitted <- synth_fit(synth, cache_path = cache_dir)
  
  # one file per variable is written
  expect_setequal(list.files(cache_dir), paste0(vs_names, ".rds"))
  
  # models are discarded by default, but roles and extractions are updated
  expect_true(all(purrr::map_lgl(fitted[["fitted_models"]], is.null)))
  expect_true(all(fitted[["roles"]][vs_names] == "fitted"))
  expect_equal(fitted[["state"]], "fitted")
  expect_equal(fitted[["cache_path"]], cache_dir)
  expect_true(fitted[["discard_after_cache"]])
  expect_equal(names(fitted[["extractions"]]), vs_names)
  
  cached <- readRDS(file.path(cache_dir, paste0(vs_names[1], ".rds")))
  
  expect_named(cached, c("fingerprint", "fitted", "extraction"))
  expect_true(workflows::is_trained_workflow(cached[["fitted"]]))
  
  unlink(cache_dir, recursive = TRUE)
  
})

test_that("synth_fit() retains models with discard_after_cache = FALSE", {
  
  cache_dir <- new_cache_dir("synth_fit_retain")
  synth <- new_test_synth()
  
  # keep the models in memory even though they are also cached
  fitted <- synth_fit(
    synth, 
    cache_path = cache_dir, 
    discard_after_cache = FALSE
  )
  
  expect_setequal(list.files(cache_dir), paste0(vs_names, ".rds"))
  
  for (n in vs_names) {
    
    expect_true(workflows::is_trained_workflow(fitted[["fitted_models"]][[n]]))
    
  }
  
  expect_true(all(fitted[["roles"]][vs_names] == "fitted"))
  expect_false(fitted[["discard_after_cache"]])
  
  unlink(cache_dir, recursive = TRUE)
  
})

test_that("synth_fit() reuses a valid cache without refitting", {
  
  cache_dir <- new_cache_dir("synth_fit_reuse")
  synth <- new_test_synth()
  
  # first fit populates the cache
  synth_fit(synth, cache_path = cache_dir)
  
  # replace the cached extractions with a marker; a refit would overwrite it
  # with a real extraction, so the marker surviving proves the cache was used
  cache_files <- file.path(cache_dir, paste0(vs_names, ".rds"))
  
  for (f in cache_files) {
    
    cached <- readRDS(f)
    cached[["extraction"]] <- "from cache"
    saveRDS(cached, f)
    
  }
  
  # same spec, so the fingerprints match and the cache is reused; models are
  # discarded by default but extractions come from the cache
  fitted <- synth_fit(synth, cache_path = cache_dir)
  
  expect_true(all(purrr::map_lgl(fitted[["fitted_models"]], is.null)))
  expect_true(all(fitted[["extractions"]] == "from cache"))
  expect_true(all(fitted[["roles"]][vs_names] == "fitted"))
  
  # valid caches are loaded into fitted_models when models are retained
  fitted_keep <- synth_fit(
    synth, 
    cache_path = cache_dir, 
    discard_after_cache = FALSE
  )
  
  for (n in vs_names) {
    
    expect_true(
      workflows::is_trained_workflow(fitted_keep[["fitted_models"]][[n]])
    )
    
  }
  
  expect_true(all(fitted_keep[["extractions"]] == "from cache"))
  
  unlink(cache_dir, recursive = TRUE)
  
})

test_that("synth_fit() refits when the fingerprint does not match", {
  
  cache_dir <- new_cache_dir("synth_fit_mismatch")
  synth <- new_test_synth()
  
  # cache the rpart fits
  synth_fit(synth, cache_path = cache_dir)
  
  # change the synthesis specification so the fingerprint no longer matches;
  # the model type changes so a reused cache would be detectable
  lm_mod <- parsnip::linear_reg() |>
    parsnip::set_engine(engine = "lm")
  
  synth2 <- new_test_synth(
    ss = synth_spec(default_regression_model = lm_mod,
                    default_regression_sampler = sample_lm,
                    default_extractor = workflows::extract_fit_parsnip)
  )
  
  # models are now lm fits, not the cached rpart fits, so a refit happened
  fitted <- synth_fit(
    synth2, 
    cache_path = cache_dir, 
    discard_after_cache = FALSE
  )
  
  for (n in vs_names) {
    
    expect_s3_class(
      workflows::extract_fit_parsnip(fitted[["fitted_models"]][[n]]), 
      "_lm"
    )
    
    # the stale cache file is replaced with the new fit
    cached <- readRDS(file.path(cache_dir, paste0(n, ".rds")))
    
    expect_s3_class(
      workflows::extract_fit_parsnip(cached[["fitted"]]), 
      "_lm"
    )
    
  }
  
  unlink(cache_dir, recursive = TRUE)
  
})

test_that("synth_fit() refits and replaces the cache when cache_valid = FALSE", {
  
  cache_dir <- new_cache_dir("synth_fit_invalid")
  synth <- new_test_synth()
  
  synth_fit(synth, cache_path = cache_dir)
  
  # first fit populates the cache with valid files
  cache_files <- file.path(cache_dir, paste0(vs_names, ".rds"))
  
  # mark the cached extractions so that a refit can be detected
  for (f in cache_files) {
    
    cached <- readRDS(f)
    cached[["extraction"]] <- "from cache"
    saveRDS(cached, f)
    
  }
  
  # even though the fingerprints match, cache_valid = FALSE forces a refit
  fitted <- synth_fit(synth, cache_path = cache_dir, cache_valid = FALSE)
  
  # no marker remains in memory...
  expect_false(
    any(purrr::map_lgl(fitted[["extractions"]], identical, "from cache"))
  )
  expect_setequal(list.files(cache_dir), paste0(vs_names, ".rds"))
  expect_false(fitted[["cache_valid"]])
  
  # ...or on disk, where the files were replaced with real extractions
  for (f in cache_files) {
    
    expect_s3_class(readRDS(f)[["extraction"]], "model_fit")
    
  }
  
  unlink(cache_dir, recursive = TRUE)
  
})

test_that("synth_fit() trusts existing caches without a specification", {
  
  cache_dir <- new_cache_dir("synth_fit_trust")
  empty_dir <- new_cache_dir("synth_fit_trust_empty")
  synth <- new_test_synth()
  
  synth_fit(synth, cache_path = cache_dir)
  
  # strip the synthesis specification, so no fingerprint can be computed
  bare <- synth
  bare[["roadmap"]] <- NULL
  bare[["synth_spec"]] <- NULL
  
  # the existing cache files are trusted and loaded
  fitted <- synth_fit(bare, cache_path = cache_dir)
  
  expect_true(all(fitted[["roles"]][vs_names] == "fitted"))
  expect_equal(fitted[["state"]], "fitted")
  expect_s3_class(fitted[["extractions"]][[vs_names[1]]], "model_fit")
  
  # without a cache file there is nothing to fit from
  expect_error(synth_fit(bare, cache_path = empty_dir), "roadmap")
  
  unlink(c(cache_dir, empty_dir), recursive = TRUE)
  
})

test_that("synth_fit() handles identity models for constant variables", {
  
  # a constant variable (`cst`) gets an identity model, which has nothing to fit
  conf <- dplyr::mutate(mtcars, cst = 1)
  
  rm_const <- roadmap(
    conf_data = conf,
    start_data = dplyr::select(conf, cyl, vs, am, gear, carb)
  ) |> 
    add_sequence_manual(mpg, cst)
  
  synth <- new_test_synth(rm = rm_const)
  
  fitted <- synth_fit(synth)
  
  # identity variables are fitted as "identity", other variables as usual
  expect_equal(fitted[["fitted_models"]][["cst"]], "identity")
  expect_equal(fitted[["extractions"]][["cst"]], "identity")
  expect_true(
    workflows::is_trained_workflow(fitted[["fitted_models"]][["mpg"]])
  )
  
})

test_that(".synth_fingerprint() ignores keep.source but not function logic", {
  
  # Cross-session stability was verified manually rather than in a test, since
  # a test would have to launch Rscript against the installed package. The
  # fingerprint of a real `roadmap` and `synth_spec` (mtcars, decision tree 
  # with `sample_rpart` and `extract_fit_parsnip`) was computed in 3 separate 
  # Rscript sessions with `tidysynthesis:::.synth_fingerprint()`. All 3 
  # fingerprints were identical, so environments and other session-specific 
  # detail do not leak into the hash.
  
  # the same function parsed with and without source references must hash the
  # same, since the fingerprint should depend on logic, not on session options
  fingerprint_with_source <- function(keep_source) {
    
    old_options <- options(keep.source = keep_source)
    on.exit(options(old_options))
    
    f <- eval(parse(text = "function(x) { x + 1 }", keep.source = keep_source))
    
    return(.synth_fingerprint(list(roadmap = roadmap, f = f)))
    
  }
  
  expect_equal(fingerprint_with_source(TRUE), fingerprint_with_source(FALSE))
  
  # identical logic gives identical fingerprints, changed logic does not
  sampler_a <- function(x) { x + 1 }
  sampler_b <- function(x) { x + 2 }
  
  expect_equal(
    .synth_fingerprint(list(roadmap = roadmap, f = sampler_a)),
    .synth_fingerprint(list(roadmap = roadmap, f = function(x) { x + 1 }))
  )
  expect_false(
    identical(
      .synth_fingerprint(list(roadmap = roadmap, f = sampler_a)),
      .synth_fingerprint(list(roadmap = roadmap, f = sampler_b))
    )
  )
  
  # changing a data field changes the fingerprint
  roadmap2 <- roadmap
  roadmap2[["conf_data"]] <- mtcars[1:30, ]
  
  expect_false(
    identical(
      .synth_fingerprint(list(roadmap = roadmap, f = sampler_a)),
      .synth_fingerprint(list(roadmap = roadmap2, f = sampler_a))
    )
  )
  
})
