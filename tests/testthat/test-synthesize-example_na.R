# create "starting data"
starting_data <- example_na |>
  dplyr::select(age)

# create schema
roadmap <- roadmap(
  conf_data = dplyr::select(example_na, -health),
  start_data = starting_data
) 

rpart_mod <- parsnip::decision_tree() |> 
  parsnip::set_mode("regression") |>
  parsnip::set_engine("rpart")

rpart_mod_cat <- parsnip::decision_tree() |> 
  parsnip::set_mode("classification") |>
  parsnip::set_engine("rpart")

synth_spec <- synth_spec(
  default_regression_model = rpart_mod,
  default_classification_model = rpart_mod_cat,
  default_regression_sampler = sample_rpart,
  default_classification_sampler = sample_rpart
)

test_that("Basic synthesis with enforce = TRUE", {
  
  synth_spec <- synth_spec(
    default_regression_model = rpart_mod,
    default_classification_model = rpart_mod_cat,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart
  )
  
  expect_warning(
    presynth <- presynth(
      roadmap = roadmap, 
      synth_spec = synth_spec
    )
  )
  
  expect_no_error(
    synth1 <- synthesize(presynth)
  )
  
})

test_that("synthesis with enforce_na = TRUE", {
  
  step1 <- function(x) {
    recipes::step_impute_knn(x, recipes::all_predictors())
  }
  
  synth_spec <- synth_spec(
    default_regression_model = rpart_mod,
    default_classification_model = rpart_mod_cat,
    custom_models = list(
      list("vars" = c("sex", "labor_force", "hours_NA", "wages_NA"),
           "model" = rpart_mod_cat)
    ),
    default_regression_step = step1, 
    default_classification_step = step1,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart,
    enforce_na = TRUE
  )
  
  expect_warning(
    presynth_with_enforce_na <- presynth(
      roadmap = roadmap,
      synth_spec = synth_spec
    )
  )

  set.seed(321)
  
  synth_with_enforce_na <- synthesize(presynth_with_enforce_na)
  
  synth1 <- synth_with_enforce_na[["synthetic_data"]] |>
    collapse_na() |>
    convert_level_to_na()

  missing_values1 <- purrr::map_dbl(synth1, ~sum(is.na(.x))) |>
    unname()
  
  expect_equal(missing_values1,  c(0, 23, 20, 13, 122))
  
})

test_that("synthesis with enforce_na = FALSE", {
  
  synth_spec <- synth_spec(
    default_regression_model = rpart_mod,
    default_regression_sampler = sample_rpart,
    default_classification_model = rpart_mod_cat,
    default_classification_sampler = sample_rpart,
    enforce_na = FALSE
  )
  
  expect_warning(
    presynth_without_enforce_na <- presynth(
      roadmap = roadmap,
      synth_spec = synth_spec
    )
  )

  set.seed(321)
  
  synth_without_enforce_na <- synthesize(presynth_without_enforce_na)
  
  synth2 <- synth_without_enforce_na[["synthetic_data"]] |>
    collapse_na() |>
    convert_level_to_na()
  
  missing_values2 <- purrr::map_dbl(synth2, ~sum(is.na(.x))) |>
    unname()
  
  expect_equal(missing_values2,  c(0, 23, 20, 13, 122))
  
})
