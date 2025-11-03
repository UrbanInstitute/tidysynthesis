dt_reg_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine("rpart") %>%
  parsnip::set_mode("regression")

dt_class_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine("rpart") %>%
  parsnip::set_mode("classification")

lm_mod <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm") %>%
  parsnip::set_mode(mode = "regression")

logistic_mod <- parsnip::logistic_reg() %>%
  parsnip::set_engine("glm") %>%
  parsnip::set_mode(mode = "classification")

rf_mod <- parsnip::rand_forest(trees = 500, min_n = 1) %>%
  parsnip::set_engine(engine = "ranger") %>%
  parsnip::set_mode(mode = "classification")



step1 <- function(x) {
  x %>%
    recipes::step_center(recipes::all_predictors(), id = "center")
}

# helper functions 
test_that("type inspections work as expected", {
  
  expect_true(.is_model(dt_reg_mod))
  expect_false(.is_model("not a model"))
  
  expect_true(.is_recipe(step1))
  expect_false(.is_recipe("not a recipe"))
  
  expect_true(.is_steps(step1))
  expect_false(.is_steps("not steps"))
  
  expect_true(.is_sampler(sample_rpart))
  expect_false(.is_sampler("not a sampler"))
  
  expect_true(.is_tuner(list(v = 3)))
  expect_false(.is_tuner("not a tuner"))
  
  expect_true(.is_extractor(parsnip::extract_fit_engine))
  expect_false(.is_extractor("not an extractor"))
  
})

test_that("validate_custom_components works as expected", {
  
  # no error when validating proper specification
  expect_no_error(
    .validate_custom_components(
      list(list("vars" = c("a", "b"), "model" = dt_reg_mod)), "model"
    )
  )
  
  # error when providing non-list elements
  expect_error(
    .validate_custom_components(
      list(list("vars" = c("a", "b"), "model" = dt_reg_mod),
           "not_a_list"), 
      "model"
    ),
    regexp = "Some custom model elements are not lists.",
    fixed = TRUE
  )

  # error when providing incorrect list names
  expect_error(
    .validate_custom_components(
      list(list("notvars" = c("a", "b"), "model" = dt_reg_mod)), "model"
    ),
    regexp = "Some custom model elements are missing the two required \n         sublist names, 'vars' and 'model'",
    fixed = TRUE
  )
  
  expect_error(
    .validate_custom_components(
      list(list("vars" = c("a", "b"), "notmodel" = dt_reg_mod)), "model"
    ),
    regexp = "Some custom model elements are missing the two required \n         sublist names, 'vars' and 'model'",
    fixed = TRUE
  )
  
  expect_error(
    .validate_custom_components(
      list(list("vars" = c("a", "b"), "model" = dt_reg_mod, "a" = 1)), "model"
    ),
    regexp = "Some custom model elements are missing the two required \n         sublist names, 'vars' and 'model'",
    fixed = TRUE
  )
  
  # error when providing incorrect types
  expect_error(
    .validate_custom_components(
      list(list("vars" = c(1, 2), "model" = dt_reg_mod)), "model"
    ),
    regexp = "Some custom model variable names are not strings.",
    fixed = TRUE
  )
  
  expect_error(
    .validate_custom_components(
      list(list("vars" = c("a", "b"), "model" = "not a model")), "model"
    ),
    regexp = "Some custom model elements have incorrect type.",
    fixed = TRUE
  )
  
  expect_error(
    .validate_custom_components(
      list(list("vars" = c("a", "b"), "model" = dt_reg_mod)), "tuner"
    ),
    regexp = "Some custom tuner elements are missing the two required \n         sublist names, 'vars' and 'tuner'",
    fixed = TRUE
  )
  
})

test_that(".map_model_to_default_sampler", {
  
  # expected behavior
  expect_true(
    .identical_funcs(.map_model_to_default_sampler(dt_reg_mod), sample_rpart)
  )
  expect_true(
    .identical_funcs(.map_model_to_default_sampler(lm_mod), sample_lm)
  )
  expect_true(
    .identical_funcs(.map_model_to_default_sampler(logistic_mod), sample_glm)
  )
  expect_true(
    .identical_funcs(.map_model_to_default_sampler(rf_mod), sample_ranger)
  )
  
  # type checking
  expect_error(
    .map_model_to_default_sampler("not a model"),
    regexp = ".is_model(model) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    .map_model_to_default_sampler(parsnip::boost_tree()),
    regexp = "Unrecognized engine: xgboost. Please either supply\n         a specific sampler or use a recognized engine:rpart, ranger, lm, glm",
    fixed = TRUE
  )
  
})

test_that(".update_custom_components", {
  
  old_ss <- synth_spec()
  
  new_ss1 <- .update_custom_components(
    old_ss, 
    "model",
    "custom_models",
    list(
      "vars" = c("a", "b", "c"),
      "model" = dt_reg_mod
    )
  )
  
  expect_identical(
    new_ss1[["custom_models"]],
    list(list("vars" = c("a", "b", "c"), "model" = dt_reg_mod))
  )
  
  # update removes variables from previous models
  new_ss2 <- .update_custom_components(
    new_ss1, 
    "model",
    "custom_models",
    list(
      "vars" = c("b", "d"),
      "model" = dt_class_mod
    )
  )
  
  expect_identical(
    new_ss2[["custom_models"]],
    list(list("vars" = c("a", "c"), "model" = dt_reg_mod), 
         list("vars" = c("b", "d"), "model" = dt_class_mod))
  )
  
  # update works independent of model changing specification
  new_ss3 <- .update_custom_components(
    new_ss2, 
    "model",
    "custom_models",
    list(
      "vars" = c("c", "e"),
      "model" = dt_reg_mod
    )
  )
  
  expect_identical(
    new_ss3[["custom_models"]],
    list(list("vars" = c("a"), "model" = dt_reg_mod), 
         list("vars" = c("b", "d"), "model" = dt_class_mod),
         list("vars" = c("c", "e"), "model" = dt_reg_mod))
  )
  
})
