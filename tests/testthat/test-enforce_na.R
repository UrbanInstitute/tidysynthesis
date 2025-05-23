example_na_expanded <- expand_na(data = example_na)

example_na_enforced <- enforce_na(data = example_na_expanded)

test_that("Enforced example_na returns the correct variable names", {

  expect_equal(
    names(example_na_enforced),
    c("age", "sex_NA", "sex", "labor_force_NA", "labor_force", "health_NA", 
      "health", "hours_NA", "hours", "wages_NA", "wages")
  )

})

test_that("enforce_na() is the inverse of expand_na()", {
  
  expect_equal(
    example_na_expanded,
    enforce_na(expand_na(example_na))
  )
  
})

test_that("enforce_na() and collapse_na() generate similar results", {
  
  # scramble one column since we don't have synthetic data
  example_na_expanded$wages_NA <- sample(
    x = example_na_expanded$wages_NA,
    size = nrow(example_na_expanded)
  )
  
  # the only difference should be the presence of "_NA" columns
  expect_equal(
    collapse_na(example_na_expanded),
    enforce_na(example_na_expanded) |>
      dplyr::select(-dplyr::ends_with("_NA"))
  )
  
})




