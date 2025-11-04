test_that("Convert 'NA' to NA", {
  
  data <- data.frame(
    x1 = c(1, 2, NA),  
    x2 = c("1", "2", "NA"),
    x3 = factor(c("1", "2", "NA")),
    x4 = factor(c("b", "NA", "a"), ordered = TRUE)
  )
  
  data <- convert_level_to_na(data)

  expect_equal(data$x1, c(1, 2, NA))
  expect_equal(data$x2, c("1", "2", NA))
  expect_equal(data$x3, factor(c("1", "2", NA)))
  expect_equal(data$x4, factor(c("b", NA, "a"), ordered = TRUE))
  
})


test_that("'NA' already in the data throws an error", {

  expect_error(
    convert_level_to_na(data = data.frame(x2 = c("1", "2", NA))),
    regexp = "Vector already contains NA; cannot call convert_level_to_NA",
    fixed = FALSE
  )
  expect_error(
    convert_level_to_na(data = data.frame(x3 = factor(c("1", "2", NA)))),
    regexp = "Vector already contains NA; cannot call convert_level_to_NA",
    fixed = FALSE
  )
  expect_error(
    convert_level_to_na(data = data.frame(factor(c("b", NA, "a"), ordered = TRUE))),
    regexp = "Vector already contains NA; cannot call convert_level_to_NA",
    fixed = FALSE
  )

})

test_that("convert_na_to_level() is the inverse of convert_level_to_na()", {
  
  data <- data.frame(
    x1 = c(1, 2, NA),  
    x2 = c("1", "2", NA),
    x3 = factor(c("1", "2", NA)),
    x4 = factor(c("b", NA, "a"), ordered = TRUE)
  )

  expect_equal(
    data, 
    convert_level_to_na(convert_na_to_level(data))
  )
  
})

