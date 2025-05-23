variance_by_hand <- 
  tibble::tribble(
    ~ntile, ~bandwidth,
    as.integer(1), density(c(1, 2))$bw,
    as.integer(2), density(c(3, 5))$bw
  )

variance_by_function <- .calc_bandwidths(baseline = c(1, 2, 3, 5, NA), n = 2)

test_that("calc_var_kde returns expected values", {
  expect_equal(variance_by_hand, variance_by_function)
})

# low heterogeneity
# calc_var_kde used to fail when there isn't enough heteroegenity in the 
# confidential vector
test_that("calc_var_kde passes with low heterogeneity", {
  
  conf_data <- c(0, 0, 0, 0, 0, 10, 10, 10, 10, 10)
  expect_equal(
    nrow(.calc_bandwidths(baseline = conf_data, n = 2)),
    2
  )
  
  expect_equal(
    nrow(.calc_bandwidths(baseline = conf_data, n = 3)),
    1
  )
  
})