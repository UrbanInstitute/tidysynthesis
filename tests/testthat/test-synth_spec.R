conf_data <- dplyr::select(mtcars, mpg, cyl, disp, carb)
start_data <- dplyr::select(mtcars, carb)

roadmap <- roadmap(
  conf_data = conf_data,
  start_data = start_data
) |>
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

mtcars_rec <- recipes::recipe(data = conf_data, formula = mpg ~ .) |>
  recipes::step_center(mpg)

dt_reg_mod <- parsnip::decision_tree() |>
  parsnip::set_engine("rpart") |>
  parsnip::set_mode("regression")

dt_class_mod <- parsnip::decision_tree() |>
  parsnip::set_engine("rpart") |>
  parsnip::set_mode("classification")

step1 <- function(x) {
  x |> recipes::step_center(recipes::all_predictors(), id = "center")
}

step2 <- function(x) {
  x |> recipes::step_scale(recipes::all_predictors(), id = "scale")
}

noise1 <- noise(
  add_noise = TRUE, 
  noise_func = add_noise_kde,
  noise_params = list(
    n_ntiles = 2
  )
)

noise2 <- noise(add_noise = FALSE)

tuner1 <- list(
  v = 3,
  grid = 3,
  metrics = yardstick::metric_set(yardstick::rmse)
)

tuner2 <- list(
  v = 5,
  grid = 5,
  metrics = yardstick::metric_set(yardstick::mae)
)

extractor1 <- parsnip::extract_fit_engine
extractor2 <- hardhat::extract_recipe


test_that("synth_spec() fails with improper inputs", {
  
  # default input type checking
  expect_error(
    synth_spec(default_regression_model = "notamodel"),
    regexp = "`default_regression_model` must be a parsnip model_spec, \n    if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_classification_model = "notamodel"),
    regexp = "`default_classification_model` must be a parsnip model_spec, \n    if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_regression_step = "notastep"),
    regexp = "`default_regression_steps` must be a function, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_classification_step = "notastep"),
    regexp = "`default_classification_steps` must be a function, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_regression_sampler = "notasampler"),
    regexp = "`default_regression_sampler` must be a function, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_classification_sampler = "notasampler"),
    regexp = "`default_classification_sampler` must be a function, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_regression_noise = "notnoise"),
    regexp = "`default_regression_noise` must be a noise object, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_classification_noise = "notnoise"),
    regexp = "`default_classification_noise` must be a noise object, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_regression_tuner = "notatuner"),
    regexp = "`default_regression_tuner` must be a list, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_classification_tuner = "notatuner"),
    regexp = "`default_classification_tuner` must be a list, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(default_extractor = "notanextractor"),
    regexp = "`default extractor` must be a function, if specified",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(invert_transformations = "no"),
    regexp = "`invert_transformations` must be logical",
    fixed = TRUE
  )
  
  expect_error(
    synth_spec(enforce_na = "no"),
    regexp = "`enforce_na` must be logical",
    fixed = TRUE
  )
  
})

test_that("update_synth_spec", {
  
  old_ss <- synth_spec()
  
  # expected update behavior
  new_ss1 <- update_synth_spec(old_ss, enforce_na = FALSE)
  expect_false(new_ss1[["enforce_na"]])
  
  # invalid argument names
  expect_error(
    update_synth_spec(old_ss, not_an_arg = TRUE),
    regexp = "Unexpected argument(s) to update_synth_spec(): not_an_arg",
    fixed = TRUE
  )
  expect_error(
    update_synth_spec(old_ss,
                      not_an_arg = TRUE,
                      enforce_na = FALSE),
    regexp = "Unexpected argument(s) to update_synth_spec(): not_an_arg",
    fixed = TRUE
  )
  expect_error(
    expect_warning(update_synth_spec(old_ss, custom_models = list())),
    regexp = "Unexpected argument(s) to update_synth_spec(): custom_models",
    fixed = TRUE
  )
  
  # invalid inpupt types
  expect_error(
    update_synth_spec(old_ss, default_regression_model = "notamodel"),
    regexp = "Default model parameter must be a parsnip model_spec",
    fixed = TRUE
  )
  
  expect_error(
    update_synth_spec(old_ss, default_regression_steps = "notastep"),
    regexp = "Default steps parameter must be a function",
    fixed = TRUE
  )
  
  expect_error(
    update_synth_spec(old_ss, default_regression_sampler = "notasampler"),
    regexp = "Default sampler parameter must be a function",
    fixed = TRUE
  )
  
  expect_error(
    update_synth_spec(old_ss, default_regression_noise = "notnoise"),
    regexp = "Default noise parameter must be a noise object",
    fixed = TRUE
  )
  
  expect_error(
    update_synth_spec(old_ss, default_regression_tuner = "notatuner"),
    regexp = "Default tuner parameter must be a list",
    fixed = TRUE
  )
  
  expect_error(
    update_synth_spec(old_ss, default_regression_extractor = "notanextractor"),
    regexp = "Unexpected argument(s) to update_synth_spec(): default_regression_extractor",
    fixed = TRUE
  )
  
})

test_that("add_custom_models", {
  
  old_ss <- synth_spec()
  
  new_ss <- add_custom_models(
    old_ss, list("vars" = c("a", "b", "c"), "model" = dt_reg_mod)
  )
  
  expect_identical(
    new_ss[["custom_models"]],
    list(list("vars" = c("a", "b", "c"), "model" = dt_reg_mod))
  )
  
  expect_error(
    add_custom_models(
      old_ss, list(
        "invalid" = c("a", "b", "c"),
        "model" = dt_reg_mod
      )
    ),
    regexp = "Some custom model elements are missing the two required \n         sublist names, 'vars' and 'model'",
    fixed = TRUE
  )
  
})

test_that("update_custom_models", {
  
  old_ss <- synth_spec(
    custom_models = list(list("vars" = c("a", "b"), "model" = dt_reg_mod))
  )
  
  new_ss <- update_custom_models(
    old_ss, list("vars" = c("c"), "model" = dt_class_mod)
  )
  
  expect_identical(
    new_ss[["custom_models"]],
    list(list("vars" = c("a", "b"), "model" = dt_reg_mod),
         list("vars" = c("c"), "model" = dt_class_mod))
  )
  
})

test_that("remove_custom_models", {
  
  old_ss <- synth_spec(
    custom_models = list(list(
      "vars" = c("a", "b", "c"),
      "model" = dt_reg_mod)
    )
  )
  
  new_ss <- remove_custom_models(old_ss)
  
  expect_true(is.null(new_ss[["custom_models"]]))
  
})


test_that("add_custom_steps", {
  
  old_ss <- synth_spec()
  
  new_ss <- add_custom_steps(
    old_ss, list("vars" = c("a", "b", "c"), "steps" = step1)
  )
  
  expect_identical(
    new_ss[["custom_steps"]],
    list(list("vars" = c("a", "b", "c"), "steps" = step1))
  )
  
  expect_error(
    add_custom_models(
      old_ss, list(
        "invalid" = c("a", "b", "c"),
        "steps" = step1
      )
    ),
    regexp = "Some custom model elements are missing the two required \n         sublist names, 'vars' and 'model'",
    fixed = TRUE
  )
  
})

test_that("update_custom_steps", {
  
  old_ss <- synth_spec(
    custom_steps = list(list("vars" = c("a", "b"), "steps" = step1))
  )
  
  new_ss <- update_custom_steps(
    old_ss, list("vars" = c("c"), "steps" = step2)
  )
  
  expect_identical(
    new_ss[["custom_steps"]],
    list(list("vars" = c("a", "b"), "steps" = step1),
         list("vars" = "c", "steps" = step2))
  )
  
})

test_that("remove_custom_steps", {
  
  old_ss <- synth_spec(
    custom_steps = list(
      list(
        "vars" = c("a", "b", "c"),
        "steps" = step1
      )
    )
  )
  
  new_ss <- remove_custom_steps(old_ss)
  
  expect_true(is.null(new_ss[["custom_steps"]]))
  
})


test_that("add_custom_samplers", {
  
  old_ss <- synth_spec()
  
  new_ss <- add_custom_samplers(
    old_ss, list("vars" = c("a", "b", "c"), "sampler" = sample_rpart)
  )
  
  expect_identical(
    new_ss[["custom_samplers"]],
    list(list("vars" = c("a", "b", "c"), "sampler" = sample_rpart))
  )
  
  expect_error(
    add_custom_samplers(
      old_ss, list(
        "invalid" = c("a", "b", "c"),
        "sampler" = sample_rpart
      )
    ),
    regexp = "Some custom sampler elements are missing the two required \n         sublist names, 'vars' and 'sampler'",
    fixed = TRUE
  )
  
})

test_that("update_custom_samplers", {
  
  old_ss <- synth_spec(
    custom_samplers = list(list("vars" = c("a", "b"), "sampler" = sample_rpart))
  )
  
  new_ss <- update_custom_samplers(
    old_ss, list("vars" = c("c"), "sampler" = extractor2)
  )
  
  expect_identical(
    new_ss[["custom_samplers"]],
    list(list("vars" = c("a", "b"), "sampler" = sample_rpart),
         list("vars" = "c", "sampler" = extractor2))
  )
  
})

test_that("remove_custom_samplers", {
  
  old_ss <- synth_spec(
    custom_samplers = list(
      list(
        "vars" = c("a", "b", "c"),
        "sampler" = sample_rpart
      )
    )
  )
  
  new_ss <- remove_custom_samplers(old_ss)
  
  expect_true(is.null(new_ss[["custom_samplers"]]))
  
})



test_that("add_custom_noises", {
  
  old_ss <- synth_spec()
  
  new_ss <- add_custom_noise(
    old_ss, list("vars" = c("a", "b", "c"), "noise" = noise1)
  )
  
  expect_identical(
    new_ss[["custom_noise"]],
    list(list("vars" = c("a", "b", "c"), "noise" = noise1))
  )
  
  expect_error(
    add_custom_noise(
      old_ss, list(
        "invalid" = c("a", "b", "c"),
        "noise" = noise1
      )
    ),
    regexp = "Some custom noise elements are missing the two required \n         sublist names, 'vars' and 'noise'",
    fixed = TRUE
  )
  
})

test_that("update_custom_noise", {
  
  old_ss <- synth_spec(
    custom_noise = list(list("vars" = c("a", "b"), "noise" = noise1))
  )
  
  new_ss <- update_custom_noise(
    old_ss, list("vars" = c("c"), "noise" = noise2)
  )
  
  expect_identical(
    new_ss[["custom_noise"]],
    list(list("vars" = c("a", "b"), "noise" = noise1),
         list("vars" = "c", "noise" = noise2))
  )
  
})

test_that("remove_custom_noise", {
  
  old_ss <- synth_spec(
    custom_noise = list(
      list(
        "vars" = c("a", "b", "c"),
        "noise" = noise2
      )
    )
  )
  
  new_ss <- remove_custom_noise(old_ss)
  
  expect_true(is.null(new_ss[["custom_noises"]]))
  
})

test_that("add_custom_tuners", {
  
  old_ss <- synth_spec()
  
  new_ss <- add_custom_tuners(
    old_ss, list("vars" = c("a", "b", "c"), "tuner" = tuner1)
  )
  
  expect_identical(
    new_ss[["custom_tuners"]],
    list(list("vars" = c("a", "b", "c"), "tuner" = tuner1))
  )
  
  expect_error(
    add_custom_tuners(
      old_ss, list(
        "invalid" = c("a", "b", "c"),
        "tuner" = tuner1
      )
    ),
    regexp = "Some custom tuner elements are missing the two required \n         sublist names, 'vars' and 'tuner'",
    fixed = TRUE
  )
  
})

test_that("update_custom_tuners", {
  
  old_ss <- synth_spec(
    custom_tuners = list(list("vars" = c("a", "b"), "tuner" = tuner1))
  )
  
  new_ss <- update_custom_tuners(
    old_ss, list("vars" = c("c"), "tuner" = tuner2)
  )
  
  expect_identical(
    new_ss[["custom_tuners"]],
    list(list("vars" = c("a", "b"), "tuner" = tuner1),
         list("vars" = "c", "tuner" = tuner2))
  )
  
})

test_that("remove_custom_tuners", {
  
  old_ss <- synth_spec(
    custom_tuners = list(
      list(
        "vars" = c("a", "b", "c"),
        "tuner" = tuner1
      )
    )
  )
  
  new_ss <- remove_custom_tuners(old_ss)
  
  expect_true(is.null(new_ss[["custom_tuners"]]))
  
})



test_that("add_custom_extractors", {
  
  old_ss <- synth_spec()
  
  new_ss <- add_custom_extractors(
    old_ss, list("vars" = c("a", "b", "c"), "extractor" = extractor1)
  )
  
  expect_identical(
    new_ss[["custom_extractors"]],
    list(list("vars" = c("a", "b", "c"), "extractor" = extractor1))
  )
  
  expect_error(
    add_custom_extractors(
      old_ss, list(
        "invalid" = c("a", "b", "c"),
        "extractor" = extractor1
      )
    ),
    regexp = "Some custom extractor elements are missing the two required \n         sublist names, 'vars' and 'extractor'",
    fixed = TRUE
  )
  
})

test_that("update_custom_extractors", {
  
  old_ss <- synth_spec(
    custom_extractors = list(list("vars" = c("a", "b"), "extractor" = extractor1))
  )
  
  new_ss <- update_custom_extractors(
    old_ss, list("vars" = c("c"), "extractor" = extractor2)
  )
  
  expect_identical(
    new_ss[["custom_extractors"]],
    list(list("vars" = c("a", "b"), "extractor" = extractor1),
         list("vars" = "c", "extractor" = extractor2))
  )
  
})

test_that("remove_custom_extractors", {
  
  old_ss <- synth_spec(
    custom_extractors = list(
      list(
        "vars" = c("a", "b", "c"),
        "extractor" = extractor1
      )
    )
  )
  
  new_ss <- remove_custom_extractors(old_ss)
  
  expect_true(is.null(new_ss[["custom_extractors"]]))
  
})

test_that("print.synth_spec", {
  
  ss <- synth_spec(
    default_regression_model = dt_reg_mod,
    default_classification_model = dt_class_mod,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart,
    default_regression_tuner = tuner1,
    custom_extractors = list(
      list(
        "vars" = c("a", "b", "c"),
        "extractor" = extractor1
      )
    )
  )
  
  expected_output <- c(
    "* default_regression_model",
    "* default_classification_model",
    "* default_regression_sampler",
    "* default_classification_sampler",
    "* default_regression_tuner",
    "* custom_extractors"
  )
  
  for (eo in expected_output) {
    
    expect_output(print(ss), eo)
    
  }
  
})