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

test_that("postsynth_to_roadmap works with partial synthesis containing _NA variables", {

 # This test addresses a bug where _NA indicator variables are added to
 # synth_vars but not to col_schema in enforce_schema(). This causes
 # postsynth_to_roadmap() to fail when called on a partial synthesis result
 # where _NA variables remain unsynthesized. The error was thrown in 
 # constraints.R when calling schema[["col_schema"]][[x]][["dtype"]]. 

  # Setup: use data with missing values (wages has NAs, so wages_NA will be created)
  starting_data <- example_na |>
    dplyr::select(age)

  roadmap_test <- roadmap(
    conf_data = dplyr::select(example_na, -health),
    start_data = starting_data
  )

  synth_spec_test <- synth_spec(
    default_regression_model = rpart_mod,
    default_classification_model = rpart_mod_cat,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart
  )

  expect_warning(
    presynth_test <- presynth(
      roadmap = roadmap_test,
      synth_spec = synth_spec_test
    )
  )

  # Corrupt age (early in visit sequence) to cause synthesis failure
  presynth_test$roadmap$conf_data$age[1:5] <- "corrupted"

  # Synthesize with keep_partial = TRUE and keep_workflows = TRUE
  # This will produce warnings but continue
  expect_warning(
    result <- synthesize(presynth_test, 
                         keep_partial = TRUE, 
                         keep_workflows = TRUE)
  )

  # This should give us the returned roadmap and work successfully to return a value
  expect_no_error(
    new_roadmap <- postsynth_to_roadmap(result)
  )


})
