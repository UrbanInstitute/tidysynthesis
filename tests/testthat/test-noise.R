test_that("noise() creates a basic noise object with length 1 inputs", {
  
  noise <- noise(add_noise = FALSE)
  
  expect_s3_class(noise, "noise")
  expect_equal(noise$add_noise, FALSE)
  expect_equal(noise$mode, "regression")
  expect_true(is.null(noise$noise_func))
  expect_true(rlang::is_empty(noise$noise_params))
  
})

test_that("noise() input tests guard against input error", {
  
  # input type checking
  expect_error(
    noise(add_noise = "not_logical")
  )
  
  expect_error(
    noise(add_noise = FALSE,
          mode = "invalid_mode")
  )
  
  expect_error(
    noise(add_noise = TRUE,
          mode = "regression",
          noise_func = "not_a_function")
  )
  
  # check for supplied noise_func if add_noise = TRUE
  expect_error(
    noise(add_noise = TRUE)
  )
  
})

test_that("print.noise", {
  
  tn <- noise(add_noise = FALSE)
  
  expect_output(
    print(tn), "add_noise: FALSE"
  )
  
})


test_that("exec_noise_func basic functionality", {
  
  # use same setup as test-add_noise_kde.R
  model <- NULL
  new_data <- NULL
  conf_model_data <- data.frame(myvar = c(1, 2, 3, 4))
  outcome_var <- "myvar"
  col_schema <- NULL
  pred <- 1:3
  n_ntiles <- 2
  
  # create noise object
  noise_ob <- noise(
    add_noise = TRUE,
    mode = "regression",
    noise_func = add_noise_kde,
    n_ntiles = n_ntiles
  )
  
  # use `exec_noise_func` to get noisy predictions
  set.seed(1)
  noisy_preds_via_exec <- exec_noise_func(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    noise = noise_ob
  )
  
  # use the noise function directly to get noisy predictions
  set.seed(1)
  noisy_preds_via_direct <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    n_ntiles = n_ntiles
  )
  
  # expect the same results
  expect_true(all(noisy_preds_via_exec == noisy_preds_via_direct))
  
})
