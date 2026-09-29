data <- dplyr::select(mtcars, cyl, mpg, disp, hp, gear)

# roadmap
roadmap <- roadmap(conf_data = data,
                   start_data = dplyr::select(data, cyl, gear)) |>
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

# synth_spec
dt_mod <- parsnip::decision_tree() |>
  parsnip::set_engine("rpart") |>
  parsnip::set_mode("regression")

synth_spec <- synth_spec(default_regression_model = dt_mod,
                         default_regression_sampler = sample_rpart)

test_that("synth() creates a basic synth object", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  expect_s3_class(synth1, "synth")
  expect_s3_class(synth1, "postsynth")
  expect_s3_class(synth1, "presynth")

  expect_true(is_synth(synth1))
  expect_true(is_postsynth(synth1))
  expect_true(is_presynth(synth1))

  expect_s3_class(synth1[["roadmap"]], "roadmap")
  expect_s3_class(synth1[["synth_spec"]], "synth_spec")

})

test_that("synth() defaults discard_after_cache to TRUE and stores it", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  expect_true(synth1[["discard_after_cache"]])

  expect_warning(
    synth2 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec,
                    discard_after_cache = FALSE)
  )

  expect_false(synth2[["discard_after_cache"]])

})

test_that("synth() defaults fitted_models to a NULL entry per variable", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  expect_equal(
    names(synth1[["fitted_models"]]),
    roadmap[["visit_sequence"]][["visit_sequence"]]
  )

  expect_true(all(purrr::map_lgl(synth1[["fitted_models"]], is.null)))

})

test_that("synth() creates expected roles and a presynth state", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  expect_true(
    all(synth1[["roles"]][names(roadmap[["start_data"]])] == "start")
  )

  expect_true(
    all(
      synth1[["roles"]][
        roadmap[["visit_sequence"]][["visit_sequence"]]] == "unsynthesized"
    )
  )

  expect_equal(synth1[["state"]], "presynth")

})

test_that(".derive_synth_state() covers all four states", {

  roles_presynth <- c(start = "start", a = "unsynthesized", b = "unsynthesized")
  roles_incomplete <- c(start = "start", a = "fitted", b = "unsynthesized")
  roles_fitted <- c(start = "start", a = "fitted", b = "fitted")
  roles_postsynth <- c(start = "start", a = "synthesized", b = "synthesized")

  expect_equal(.derive_synth_state(roles_presynth), "presynth")
  expect_equal(.derive_synth_state(roles_incomplete), "incomplete")
  expect_equal(.derive_synth_state(roles_fitted), "fitted")
  expect_equal(.derive_synth_state(roles_postsynth), "postsynth")

})

test_that("synth input errors", {

  expect_error(
    synth(
      roadmap = roadmap,
      synth_spec = "not a synth_spec"
    ),
    regexp = "`synth_spec` must be a synth_spec object",
    fixed = TRUE
  )

  expect_error(
    synth(
      roadmap = "not a roadmap",
      synth_spec = synth_spec
    ),
    regexp = "`roadmap` must be a roadmap object",
    fixed = TRUE
  )

})

test_that(".validate_synth() passes for a freshly created synth object", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  expect_null(.validate_synth(synth1))

})

test_that(".validate_synth() catches an invalid state", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  synth1[["state"]] <- "not-a-state"

  expect_error(
    .validate_synth(synth1),
    regexp = "`state` must be one of",
    fixed = TRUE
  )

})

test_that(".validate_synth() catches a malformed discard_after_cache", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  synth1[["discard_after_cache"]] <- "not-a-logical"

  expect_error(
    .validate_synth(synth1),
    regexp = "`discard_after_cache` must be a single logical value",
    fixed = TRUE
  )

})

test_that(".validate_synth() catches malformed fitted_models", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  synth1[["fitted_models"]] <- list(not_a_visit_var = NULL)

  expect_error(
    .validate_synth(synth1),
    regexp = "`fitted_models` must be a named list matching the visit sequence",
    fixed = TRUE
  )

})

test_that("print.synth() works for the presynth state", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  expect_output(print(synth1), "Synth")
  expect_output(print(synth1), "State: presynth")

})

test_that("print.synth() works for the incomplete state", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  visit_vars <- roadmap[["visit_sequence"]][["visit_sequence"]]
  synth1[["roles"]][visit_vars[[1]]] <- "fitted"
  synth1[["state"]] <- .derive_synth_state(synth1[["roles"]])

  expect_equal(synth1[["state"]], "incomplete")
  expect_output(print(synth1), "State: incomplete")
  expect_output(print(synth1), "unsynthesized")
  expect_output(print(synth1), "fitted")

})

test_that("print.synth() works for the fitted state", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  visit_vars <- roadmap[["visit_sequence"]][["visit_sequence"]]
  synth1[["roles"]][visit_vars] <- "fitted"
  synth1[["state"]] <- .derive_synth_state(synth1[["roles"]])

  expect_equal(synth1[["state"]], "fitted")
  expect_output(print(synth1), "State: fitted")

})

test_that("print.synth() works for the postsynth state without erroring", {

  expect_warning(
    synth1 <- synth(roadmap = roadmap,
                    synth_spec = synth_spec)
  )

  visit_vars <- roadmap[["visit_sequence"]][["visit_sequence"]]
  synth1[["roles"]][visit_vars] <- "synthesized"
  synth1[["state"]] <- .derive_synth_state(synth1[["roles"]])
  synth1[["synthetic_data"]] <- data
  synth1[["total_synthesis_time"]] <- 1.23

  expect_equal(synth1[["state"]], "postsynth")
  expect_output(print(synth1), "State: postsynth")
  expect_output(print(synth1), "Synthetic Data")
  expect_output(print(synth1), "Total Synthesis Time: 1.23 seconds")

})
