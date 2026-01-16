# specify default arguments for add_noise_laplace
# note that NULL values may be supplied to the function and left unused
model <- NULL
new_data <- NULL
conf_model_data <- NULL
outcome_var <- NULL
col_schema <- NULL
pred <- 1:100

test_that("add_noise_disc_lapalce reproduces noise", {
  
  set.seed(1)
  noisy_preds1 <- add_noise_disc_laplace(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    variance = 3
  )
  
  set.seed(1)
  noisy_preds2 <- add_noise_disc_laplace(
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

test_that("add_noise_disc_laplace reproduces with either direct var or eDP", {
  
  # compute theoretical variance based on distribution properties
  test_eps <- 1
  test_sens <- 2
  test_p <- test_eps / test_sens
  test_var <- 2 * exp(-test_p) / (1 - exp(-test_p))**2
  
  set.seed(1)
  noisy_preds1 <- add_noise_disc_laplace(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    variance = test_var
  )

  set.seed(1)
  noisy_preds2 <- add_noise_disc_laplace(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    epsilon = test_eps,
    sensitivity = test_sens
  )
  
  expect_equal(noisy_preds1, noisy_preds2)
  
})

test_that("add_noise_disc_laplace increment rescaling", {
  
  set.seed(1)
  noisy_preds1 <- add_noise_disc_laplace(
    model = model,
    new_data = new_data,
    conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    variance = 1,
    increment = 5
  )
  
  # all noise increments should be 0 modulo 5 (the increment)
  expect_true(all((noisy_preds1 - pred) %% 5 == 0))
  
})

test_that("add_noise_disc_laplace error handling", {
  
  # no variance parameters specified
  expect_error(
    add_noise_disc_laplace(
      model = model,
      new_data = new_data,
      conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred
    ),
    regexp = "Must specify either `variance` or both `epsilon` and `sensitivity`."
  )
  
  # too many variance parameters specified
  expect_error(
    add_noise_disc_laplace(
      model = model,
      new_data = new_data,
      conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      variance = 1,
      epsilon = 2
    ),
    regexp = "If using variance, epsilon and sensitivity cannot be specified."
  )
  
  # not enough variance parameters specified
  expect_error(
    add_noise_disc_laplace(
      model = model,
      new_data = new_data,
      conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      epsilon = 2
    ),
    regexp = "Must specify either `variance` or both `epsilon` and `sensitivity`."
  )
  
  # incorrect increment 
  expect_error(
    add_noise_disc_laplace(
      model = model,
      new_data = new_data,
      conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      variance = 1,
      increment = 0
    ),
    regexp = "add_noise_disc_laplace increment must be greater than 0."
  )
  
})
