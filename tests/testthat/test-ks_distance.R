test_that("Kolmogorov-Smirnov distance", {
  
  ks0 <- data.frame(x = 1:100, y = 1:100)
  
  ks1 <- data.frame(x = 1:100, y = 101:200)
  
  ks0.5 <- data.frame(x = 1:100, y = 51:150) 
  
  expect_equal(
    ks_distance(data = ks0, truth = x, estimate = y)$.estimate,
    0
  )
  expect_equal(
    ks_distance(data = ks1, truth = x, estimate = y)$.estimate,
    1
  )
  expect_equal(
    ks_distance(data = ks0.5, truth = x, estimate = y)$.estimate,
    0.5
  )
  
})

test_that("KS distance NA handling", {
  
  ks_NA <- data.frame(x = 1:100, y = 1:100)
  ks_NA[3:100, "x"] <- NA
  
  expect_true(
    is.na(
      ks_distance(
        data = ks_NA, truth = x, estimate = y, na_rm = FALSE
      )$.estimate
    )
  )
  
  
  expect_true(
    ks_distance(
      data = ks_NA, truth = x, estimate = y, na_rm = TRUE
    )$.estimate == 0
  )
  
  
})