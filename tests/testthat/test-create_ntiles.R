# expected ntiles
test_that(".create_ntiles returns expected values", {
  expect_equal(
    .create_ntiles(x_bounds = 1:10, y_ntiles = 1:10, n = 1)$ntiles, 
    rep(1, 10)
  )
  expect_equal(
    .create_ntiles(x_bounds = 1:10, y_ntiles = 1:10, n = 2)$ntiles, 
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  )
  expect_equal(
    .create_ntiles(x_bounds = rep(0, 10), y_ntiles = 1:10, n = 2)$ntiles, 
    rep(1, 10) # exclusions = 0
  )
  expect_equal(
    .create_ntiles(x_bounds = c(1:10, NA, NA), y_ntiles = 1:10, n = 2, na.rm = TRUE)$ntiles,    
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  )
  expect_equal(
    .create_ntiles(x_bounds = 1:10, 
                  y_ntiles = c(-10, -5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1000), 
                  n = 5)$ntiles,    
    c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5)
  )
})

test_that(".create_ntiles returns expected values with exclusions", {
  expect_equal(
    .create_ntiles(x_bounds = 1:10, y_ntiles = 1:10, ties_method = "exclusions", n = 1), 
    list(
      ntiles = rep(1, 10),
      derived_exclusions = NULL
    )
  )
  expect_equal(
    .create_ntiles(x_bounds = 1:10, y_ntiles = 1:10, ties_method = "exclusions", n = 2), 
    list(
      ntiles = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
      derived_exclusions = NULL
    )
  )
  expect_equal(
    .create_ntiles(x_bounds = rep(0, 10), y_ntiles = 1:10, ties_method = "exclusions", n = 2), 
    list(
      ntiles = rep(1, 10),
      derived_exclusions = 0
    )
  )
  expect_equal(
    .create_ntiles(x_bounds = c(1:10, NA, NA), y_ntiles = 1:10, ties_method = "exclusions", n = 2, na.rm = TRUE),    
    list(
      ntiles = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
      derived_exclusions = NULL
    )
  )
  expect_equal(
    .create_ntiles(x_bounds = 1:10, 
                  y_ntiles = c(-10, -5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1000), 
                  n = 5),  
    list(
      ntiles = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5),
      derived_exclusions = NULL
    )
  )
})

# missing values and illegal values
test_that(".create_ntiles fails with incorrect inputs", {
  expect_error(
    .create_ntiles(x_bounds = c(1:10, NA, NA), y_ntiles = 1:10, n = 2)$ntiles,
    regexp = "!any(is.na(x_bounds)) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    .create_ntiles(x_bounds = c("a", "b", "c"), y_ntiles = 1:10, n = 2)$ntiles,
    regexp = "is.numeric(x_bounds) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    .create_ntiles(x_bounds = 1:10, y_ntiles = c("a", "b", "c"), n = 2)$ntiles,
    regexp = "is.numeric(y_ntiles) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    .create_ntiles(x_bounds = seq(1, 10, by = 1), y_ntiles = seq(1, 10, by = 1), n = 20)$ntiles,
    regexp = "`n` can't exceed the length of the confidential variable",
    fixed = TRUE
  )
})

# low heterogeneity
# .create_ntiles used to fail when there isn't enough heteroegenity in the 
# confidential vector
test_that(".create_ntiles passes with low heterogeneity", {
  
  conf_data <- c(0, 0, 0, 0, 10, 10, 10, 10, 10, 10)
  pred_data <- 1:10
  expect_equal(
    .create_ntiles(x_bounds = conf_data, y_ntiles = pred_data, n = 5)$ntiles, 
    c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
  )
  
  conf_data <- c(0, 0, 10, 10, 10, 10, 10, 10, 10, 10)
  pred_data <- 1:10
  expect_equal(
    .create_ntiles(x_bounds = conf_data, y_ntiles = pred_data, n = 3)$ntiles,
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )
  
  conf_data <- c(-2500, 0, 0, 0, 0, 0, 0, 0, 0, 100)
  pred_data <- 1:10
  expect_equal(
    .create_ntiles(x_bounds = conf_data, y_ntiles = pred_data, n = 5)$ntiles,
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  )
})

