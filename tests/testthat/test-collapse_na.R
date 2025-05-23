example_na_expanded <- expand_na(data = example_na)

example_na_collapsed <- collapse_na(data = example_na_expanded)

test_that("Collapsed example_na returns the correct variable names", {

  expect_equal(
    names(example_na_collapsed),
    c("age", "sex", "labor_force", "health", "hours", "wages")
  )

})

test_that("collapse_na() is the inverse of expand_na()", {
  
  expect_equal(
    example_na,
    collapse_na(expand_na(example_na))
  )
  
})

