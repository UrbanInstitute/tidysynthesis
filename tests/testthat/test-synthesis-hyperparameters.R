data <- dplyr::select(mtcars, cyl, mpg, disp, hp) |>
  dplyr::mutate(identity_var = 1)

start_data <- dplyr::select(data, cyl)

# roadmap
roadmap <- roadmap(
  conf_data = data,
  start_data = start_data
) |>
  add_sequence_numeric(everything(), method = "proportion")

# recipes
step1 <- function(x) {
  recipes::step_pca(x, recipes::all_predictors())
}
step2 <- function(x) {
  recipes::step_center(x, recipes::all_numeric(), -recipes::all_outcomes())
}

rpart_mod <- parsnip::decision_tree(cost_complexity = tune::tune()) |>
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
  default_classification_steps = step2, 
  custom_steps = list(
    list(vars = c("hp"),
         steps = step2)
  ),  
  default_regression_sampler = sample_rpart,
  default_classification_sampler = sample_rpart,
  custom_sampler = list(
    list(vars = "hp",
         sampler = sample_lm)
  ),
  default_regression_tuner = list(
    v = 3, 
    grid = 5, 
    metrics = yardstick::metric_set(yardstick::rmse)
  ),
  custom_tuners = list(
    list(vars = c("hp"),
         tuner = NA)
  ),
  default_extractor = workflows::extract_fit_parsnip
)


test_that("hyperparameter tuning results in different cost complexity parameters ", {
  
  
  set.seed(1)
  
  expect_warning(
    presynth <- presynth(
      roadmap = roadmap,
      synth_spec = synth_spec
    )
  )
  
  # expect warnings for non-specified hyperparameter tuning metrics
  expect_warning(
    synth <- synthesize(presynth)
  )

  
  expect_equal(synth$extractions$identity_var, "identity")
  expect_true(
    as.numeric(rlang::quo_text(synth$extractions$mpg$spec$args$cost_complexity)) !=
      as.numeric(rlang::quo_text(synth$extractions$disp$spec$args$cost_complexity))
  )
    
})
