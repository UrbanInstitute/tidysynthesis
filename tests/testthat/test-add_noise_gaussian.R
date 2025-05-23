# specify default arguments for add_noise_gaussian
# note that NULL values may be supplied to the function and left unused
model <- NULL
new_data <- NULL
conf_model_data <- NULL
outcome_var <- NULL
col_schema <- NULL
pred <- 1:100

test_that("add_noise_gaussian reproduces noise", {
  
  set.seed(1)
  noisy_preds1 <- add_noise_gaussian(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    variance = 3
  )
  
  set.seed(1)
  noisy_preds2 <- add_noise_gaussian(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    variance = 3
  )
  
  expect_equal(noisy_preds1, noisy_preds2)
  
})

test_that("add_noise_gaussian reproduces with either direct var or zCDP", {
  
  set.seed(1)
  noisy_preds1 <- add_noise_gaussian(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    variance = 2
  )
  
  set.seed(1)
  noisy_preds2 <- add_noise_gaussian(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    rho = 1,
    sensitivity = 2
  )
  
  expect_equal(noisy_preds1, noisy_preds2)
  
})

test_that("add_noise_gaussian error handling", {
  
  # no variance parameters specified
  expect_error(
    add_noise_gaussian(
      model = model,
      new_data = new_data,
      conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred
    )
  )
  
  # too many variance parameters specified
  expect_error(
    add_noise_gaussian(
      model = model,
      new_data = new_data,
      conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      variance = 1,
      rho = 2
    )
  )
  
  # not enough variance parameters specified
  expect_error(
    add_noise_gaussian(
      model = model,
      new_data = new_data,
      conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      rho = 2
    )
  )
  
})
