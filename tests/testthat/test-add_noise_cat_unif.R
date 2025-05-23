model <- NULL
new_data <- NULL
conf_model_data <- mtcars %>% 
  dplyr::mutate(gear = factor(.data[["gear"]]))
col_schema <- list(
  "dtype" = "fct",
  "levels" = c("3", "4", "5"),
  "na_prop" = 0
)
outcome_var <- "gear"
pred <- factor(c(rep("3", 10), rep("4", 10), rep("5", 10)))

test_that("add_noise_cat_unif reproduces with seed", {
  
  set.seed(1)
  res1 <- add_noise_cat_unif(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    unif_prop = .5
  )
  
  set.seed(1)
  res2 <- add_noise_cat_unif(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    unif_prop = .5
  )
  
  expect_true(all(res1 == res2))
  expect_true(all(levels(res1) == levels(res2)))
  
})

test_that("add_noise_cat_unif basic functionality", {
  
  # zero uniform proportion equates to no noise
  res1 <- add_noise_cat_unif(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    unif_prop = 0.
  )
  expect_true(all(res1 == pred))
  expect_true(all(levels(res1) == c("3", "4", "5")))
  
  # resample_props determines how levels get sampled
  set.seed(1)
  res2 <- add_noise_cat_unif(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    unif_prop = 1,
    resample_props = c("3" = 1, "4" = 0, "5" = 0)
  )
  expect_true(all(res2 == "3"))
  expect_true(all(levels(res2) == c("3", "4", "5")))
  
  # observed_levels respects observed levels
  set.seed(1)
  expect_warning(
    res3 <- add_noise_cat_unif(
      model = model,
      new_data = new_data,
      conf_model_data = head(conf_model_data, 5),
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      unif_prop = 1,
      observed_levels = TRUE
    )
  )
  
  expect_true(all(res3 != "5"))  # no level 5 observed in data.frame head
  expect_true(all(levels(res3) == c("3", "4", "5")))
  
})

test_that("add_noise_cat_unif error handling", {
  
  # must provide uniform sampling proportion
  expect_error(
    add_noise_cat_unif(
      model = model,
      new_data = new_data,
      conf_model_data = head(conf_model_data, 5),
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred
    )
  )
  
  # unif_prop must be between 0 and 1
  expect_error(
    add_noise_cat_unif(
      model = model,
      new_data = new_data,
      conf_model_data = head(conf_model_data, 5),
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      unif_prop = 2
    )
  )
  
  # resample_props names must be correct
  expect_error(
    add_noise_cat_unif(
      model = model,
      new_data = new_data,
      conf_model_data = head(conf_model_data, 5),
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      unif_prop = .5,
      resample_props = c("notalevel" = 1)
    )
  )
  
  # observed_levels drops a level specified in resample_props
  expect_error(
    expect_warning(
      add_noise_cat_unif(
        model = model,
        new_data = new_data,
        conf_model_data = head(conf_model_data, 5),
        outcome_var = outcome_var,
        col_schema = col_schema,
        pred = pred,
        unif_prop = 1,
        observed_levels = TRUE,
        resample_props = c("3" = 1, "4" = 0, "5" = 1)
      ) 
    )
  )
  
})