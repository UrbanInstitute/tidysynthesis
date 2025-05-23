data <- dplyr::select(mtcars, cyl, mpg, disp, hp, gear)

# roadmaps
roadmap <- roadmap(conf_data = data,
                   start_data = dplyr::select(data, cyl, gear)) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

roadmap2 <- roadmap(conf_data = data,
                    start_data = dplyr::select(mtcars, cyl)) |>
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

# synth_spec
dt_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine("rpart") %>%
  parsnip::set_mode("regression")

dt_class_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine("rpart") %>%
  parsnip::set_mode("classification")

rf_mod_regression <- parsnip::rand_forest(trees = 500, min_n = 1) %>%
  parsnip::set_engine(engine = "ranger") %>%
  parsnip::set_mode(mode = "regression") %>%
  parsnip::set_args(quantreg = TRUE)

synth_spec <- synth_spec(default_regression_model = dt_mod,
                         default_regression_sampler = sample_rpart)

synth_spec2 <- synth_spec(default_regression_model = rf_mod_regression,
                          default_regression_sampler = sample_ranger)


# noise
noise <- noise(
  add_noise = TRUE,
  noise_func = add_noise_kde,
  noise_params = list(
   exclusions = c(0, 100),
   ntiles = 4
  )
)

# constraints
constraints_df <- (
  tibble::tribble(~var, ~min, ~max, ~conditions,
                  "mpg", 0, Inf, "TRUE",
                  "mpg", -Inf, 15, "cyl == 6",
                  "mpg", -Inf, 12, "cyl == 8",
                  "disp", 0, Inf, "TRUE"
  ) 
)

test_that("presynth() creates a basic presynth object", {
  
  # presynth
  expect_warning(
    presynth <- presynth(roadmap = roadmap,
                         synth_spec = synth_spec)
  )

  
  expect_s3_class(presynth, "presynth")
  expect_s3_class(presynth$roadmap, "roadmap")
  expect_s3_class(presynth$synth_spec , "synth_spec")
  

})

test_that("presynth input errors", {
  
  expect_error(
    presynth(
      roadmap = roadmap,
      synth_spec = "not a synth_spec"
    )
  )
  
  expect_error(
    presynth(
      roadmap = "not a roadmap",
      synth_spec = synth_spec
    )
  )
  
})

test_that("update_presynth", {
  
  expect_warning(
    old_ps <- presynth(roadmap = roadmap,
                       synth_spec = synth_spec)
  )
  
  # construct new presynth with new roadmap synthesizing an additional variable
  expect_warning(
    new_ps <- update_presynth(
      old_ps,
      roadmap = roadmap2
    )
  )

  # expect new workflow components 
  for (w in old_ps$workflows) {
    expect_equal(length(w), 3)
  }

  for (w in new_ps$workflows) {
    expect_equal(length(w), 4)
  }
  
  # construct presynth with new synth_spec
  expect_warning(
    new_ps2 <- update_presynth(
      old_ps,
      synth_spec = synth_spec2
    )
  )
  
  expect_identical(new_ps2$synth_spec$default_regression_sampler,
                   sample_ranger)
  
  
})


test_that("variable location by type validation", {
  
  expect_warning(
    ps <- presynth(roadmap = roadmap,
                   synth_spec = synth_spec)
  )
  ps$workflows$built_models$mpg <- dt_class_mod
  
  expect_error(.validate_presynth(ps))
  
})
