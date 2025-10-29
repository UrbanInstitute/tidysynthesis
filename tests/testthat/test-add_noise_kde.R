# specify default arguments for add_noise_kde
# note that NULL values may be supplied to the function and left unused
model <- NULL
new_data <- NULL
conf_model_data <- data.frame(myvar = c(1, 2, 3, 4))
outcome_var <- "myvar"
col_schema <- NULL
pred <- 1:3
n_ntiles <- 2
obs_per_ntile <- 2
obs_per_ntile_roundtest <- 1.5

test_that("add_noise_kde basic reproducibility", {
  
  set.seed(1)
  sample1 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    n_ntiles = n_ntiles
  )
  
  expect_true(length(sample1) == length(pred))
  
  set.seed(1)
  sample2 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    n_ntiles = n_ntiles
  )
  
  expect_true(length(sample2) == length(pred))
  
  expect_true(all(sample1 == sample2))
  
})

test_that("add_noise_kde with exclusions", {
  expect_equal(
    add_noise_kde(
      model = model,
      new_data = new_data,
      conf_model_data = conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      n_ntiles = n_ntiles,
      exclusions = c(1, 2, 3)
    ),
    c(1, 2, 3)
  )
})


# low heterogeneity
# create_ntiles used to fail when there isn't enough heteroegenity in the 
# confidential vector
test_that("add_noise_kde passes with low heterogeneity", {
  
  pred_with_noise <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = data.frame(
      myvar = c(0, 0, 0, 0, 0, 10, 10, 10, 10, 10)
    ),
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = 1:10,
    n_ntiles = 3,
    ties_method = "random"
  )
  
  expect_false(any(is.na(pred_with_noise)))
  
})


test_that("add_noise_kde error checking", {
  
  # XOR for n_ntiles and obs_per_ntile
  expect_error(
    add_noise_kde(
      model = model,
      new_data = new_data,
      conf_model_data = conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      n_ntiles = NULL,
      obs_per_ntile = NULL
    )
  )
  
  expect_error(
    add_noise_kde(
      model = model,
      new_data = new_data,
      conf_model_data = conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      n_ntiles = 2,
      obs_per_ntile = 2
    )
  )
  
  # invalid ties method
  expect_error(
    add_noise_kde(
      model = model,
      new_data = new_data,
      conf_model_data = conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      n_ntiles = 2,
      ties_method = "invalid"
    )
  )
  
  # invalid sd_scale
  expect_error(
    add_noise_kde(
      model = model,
      new_data = new_data,
      conf_model_data = conf_model_data,
      outcome_var = outcome_var,
      col_schema = col_schema,
      pred = pred,
      n_ntiles = 2,
      sd_scale = -1
    )
  )
  
})


test_that("add_noise_kde basic reproducibility with obs_per_ntile", {
  
  set.seed(1)
  sample1 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    n_ntiles = n_ntiles
  )
  
  set.seed(1)
  sample2 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    obs_per_ntile = obs_per_ntile
  )
  
  expect_true(all(sample1 == sample2))
  
})



test_that("add_noise_kde basic reproducibility with obs_per_ntile rounding", {
  
  set.seed(1)
  sample1 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    obs_per_ntile = obs_per_ntile
  )
  
  set.seed(1)
  sample2 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = pred,
    obs_per_ntile = obs_per_ntile_roundtest
  )
  
  expect_true(all(sample1 == sample2))
  
})


test_that("add_noise_kde sd_scale", {
  
  test_preds <- rep(pred, 100)
  
  set.seed(1)
  sample1 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = test_preds,
    n_ntiles = n_ntiles,
    sd_scale = 1
  )
  
  set.seed(1)
  sample2 <- add_noise_kde(
    model = model,
    new_data = new_data,
    conf_model_data = conf_model_data,
    outcome_var = outcome_var,
    col_schema = col_schema,
    pred = test_preds,
    n_ntiles = n_ntiles,
    sd_scale = 10
  )
  
  # expect ratio in empirical SDs to be `sd_scale`
  expect_equal(
    sd(sample2 - test_preds) / sd(sample1 - test_preds), 10.0
  )
  
})