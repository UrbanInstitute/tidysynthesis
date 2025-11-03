set.seed(20250220)
x_vals <- runif(100, -5, 5)

test_that("yeo_johnson expected errors", {
  
  # derivative negative value
  expect_error(
    yeo_johnson(x_vals, lambda = 1, derivative = -1),
    regexp = "argument 'derivative' must be a non-negative integer",
    fixed = TRUE
  )
  
  # epsilon negative value
  expect_error(
    yeo_johnson(x_vals, lambda = 1, epsilon = -1),
    regexp = "argument 'epsilon' must be a single positive number",
    fixed = TRUE
  )
  
  # inverse with non-zero derivative
  expect_error(
    yeo_johnson(x_vals, lambda = 1, derivative = 1, inverse = TRUE),
    regexp = "argument 'derivative' must 0 when inverse = TRUE",
    fixed = TRUE
  )
  
})

test_that("yeo_johnson expected behavior", {
  
  # derivative = 0
  expect_no_error(
    yeo_johnson(x_vals, lambda = 0, derivative = 0)
  )
  
  # multiple lambdas
  expect_no_error(
    yeo_johnson(x_vals[1], lambda = c(0, 1), derivative = 0)
  )
  
  expect_no_error(
    yeo_johnson(x_vals, lambda = 1, derivative = 0)
  )
  
  expect_no_error(
    yeo_johnson(x_vals, lambda = 2, derivative = 0)
  )
  
  # derivative > 0
  expect_no_error(
    yeo_johnson(x_vals, lambda = 0, derivative = 1)
  )
  
  expect_no_error(
    yeo_johnson(x_vals, lambda = 1, derivative = 1)
  )
  
  expect_no_error(
    yeo_johnson(x_vals, lambda = 2, derivative = 1)
  )
  
  # inverse = TRUE
  expect_no_error(
    yeo_johnson(x_vals, lambda = 0, derivative = 0, inverse = TRUE)
  )
  
  expect_no_error(
    yeo_johnson(x_vals, lambda = 1, derivative = 0, inverse = TRUE)
  )
  
  expect_no_error(
    yeo_johnson(x_vals, lambda = 2, derivative = 0, inverse = TRUE)
  )
  
})