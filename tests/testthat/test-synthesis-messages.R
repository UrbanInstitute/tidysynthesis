# roadmap 
data <- dplyr::select(mtcars, cyl, mpg, disp, hp) |>
  dplyr::mutate(
    low_var = rep(c(1, 2), times = 16),
    identity_var = 1
  )

start_data <- dplyr::select(data, cyl)

roadmap <- roadmap(conf_data = data, start_data = start_data) |>
  add_sequence_numeric(dplyr::everything(), method = "proportion") |>
  update_constraints(
    constraints_df_num = tibble::tribble(
      ~var, ~min, ~max, ~conditions,
      "mpg", 0, Inf, "TRUE",
      "mpg", -Inf, 15, "cyl == 6",
      "mpg", -Inf, 12, "cyl == 8",
      "disp", 0, Inf, "TRUE"
    )
  )


# synth_spec
step1 <- function(x) recipes::step_pca(x, recipes::all_predictors())
step2 <- function(x) recipes::step_center(x, recipes::all_predictors())
step3 <- function(x) recipes::step_YeoJohnson(
  x, recipes::all_outcomes(), id = "outcome yj", skip = TRUE
)

rpart_mod <- parsnip::decision_tree() |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")

lm_mod <- parsnip::linear_reg() |>
  parsnip::set_engine(engine = "lm")

synth_spec <- synth_spec(
  default_regression_model = rpart_mod,
  default_classification_model = rpart_mod,
  custom_models = list(
    list(vars = c("hp"),
         model = lm_mod)
  ),
  default_regression_steps = step1, 
  default_classification_steps = step1, 
  custom_steps = list(
    list(
      vars = c("mpg", "low_var"),
      steps = step3
    ),
    list(
      vars = "hp",
      steps = step2
    )
  ),
  default_regression_sampler = sample_rpart,
  default_classification_sampler = sample_rpart,
  custom_samplers = list(
    list(vars = c("hp"), 
         sampler = sample_lm)
  ),
  default_regression_noise = noise(
    add_noise = TRUE,
    noise_func = add_noise_kde,
    exclusions = c(0, 100),
    n_ntiles = 4
  )
)

# presynth
suppressWarnings(
  presynth <- presynth(roadmap = roadmap,
                       synth_spec = synth_spec)
)


test_that("objects print without error", {
  
  capture.output(expect_no_error(print(visit_sequence)))
  
  capture.output(expect_no_error(print(roadmap)))
  
  capture.output(expect_no_error(print(synth_spec)))
  
  capture.output(expect_no_error(print(noise)))
  
  capture.output(expect_no_error(print(constraints)))
  
  capture.output(expect_no_error(print(replicates)))
  
  capture.output(expect_no_error(print(presynth)))
  
})

test_that("synthesize() runs without error", {
  
  synth <- synthesize(presynth)
  
  expect_true(is_postsynth(synth))
  expect_equal(nrow(roadmap$start_data), nrow(synth$synthetic_data))
  
})

test_that("synthesize() runs with progress", {
  
  quiet_progress <- purrr::quietly(
    \(x) { synthesize(presynth, progress = TRUE) }
  )
  output <- quiet_progress()

  expected_output <- c(
    "Synthesizing 1/5 mpg... \n",
    "Synthesizing 2/5 disp... \n",
    "Synthesizing 3/5 hp... \n",
    "Synthesizing 4/5 low_var... \n",
    "Synthesizing 5/5 identity_var... \n"
  )
  
  expect_identical(output$messages, expected_output)
  
})

