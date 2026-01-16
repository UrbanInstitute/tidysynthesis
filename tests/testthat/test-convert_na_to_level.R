test_that("Convert NA to 'NA'", {
  
  data <- data.frame(
    x1 = c(1, 2, NA),
    x2 = c("1", "2", NA),
    x3 = factor(c("1", "2", NA)),
    x4 = factor(c("b", NA, "a"), levels = c("b", NA, "a"), ordered = TRUE)
  )
  
  data <- convert_na_to_level(data)

  expect_equal(data$x1, c(1, 2, NA))
  expect_equal(data$x2, c("1", "2", "NA"))
  expect_equal(data$x3, factor(c("1", "2", "NA")))
  expect_equal(data$x4, factor(x = c("b", "NA", "a"), levels = c("b", "a", "NA"), ordered = TRUE))
  
})


test_that("'NA' already in the data throws an error", {

  expect_error(
    convert_na_to_level(data = data.frame(x2 = c("1", "2", "NA"))),
    regexp = "Can't convert NA to level 'NA' because level 'NA' already exists",
    fixed = FALSE
  )
  expect_error(
    convert_na_to_level(data = data.frame(x3 = factor(c("1", "2", "NA")))),
    regexp = "Can't convert NA to level 'NA' because level 'NA' already exists",
    fixed = FALSE
  )
  expect_error(
    convert_na_to_level(data = data.frame(factor(c("b", "NA", "a"), ordered = TRUE))),
    regexp = "Can't convert NA to level 'NA' because level 'NA' already exists",
    fixed = FALSE
  )

})
