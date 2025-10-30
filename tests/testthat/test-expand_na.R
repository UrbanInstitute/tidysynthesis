example_na_expanded <- expand_na(data = example_na)

test_that("Expanded example_na returns the correct variable names", {

  expect_equal(
    names(example_na_expanded), 
    c("age", "sex_NA", "sex", "labor_force_NA", "labor_force", "health_NA", 
      "health", "hours_NA", "hours", "wages_NA", "wages")
  )

})

test_that("Expanded example_na correctly labels missing and nonmissing values", {

  expect_true(all(example_na_expanded$sex_NA[is.na(example_na$sex)] == "missing value"))
  expect_true(all(example_na_expanded$labor_force_NA[is.na(example_na$labor_force)] == "missing value"))
  expect_true(all(example_na_expanded$hours_NA[is.na(example_na$hours)] == "missing value"))
  expect_true(all(example_na_expanded$wages_NA[is.na(example_na$wages)] == "missing value"))
    
  expect_true(all(example_na_expanded$sex_NA[!is.na(example_na$sex)] == "nonmissing value"))
  expect_true(all(example_na_expanded$labor_force_NA[!is.na(example_na$labor_force)] == "nonmissing value"))
  expect_true(all(example_na_expanded$hours_NA[!is.na(example_na$hours)] == "nonmissing value"))
  expect_true(all(example_na_expanded$wages_NA[!is.na(example_na$wages)] == "nonmissing value"))
  
})

test_that("Variable with name containing '_NA' throws an error", {

  expect_error(
    expand_na(data = data.frame(x2_NA = c("1", "2", NA))),
    regexp = "If using incomplete data, variable names cannot end in '_NA'",
    fixed = FALSE
)

})

test_that("collapse_na() is the inverse of expand_na()", {

  expect_equal(
    example_na,
    collapse_na(expand_na(example_na))
  )

})

test_that("Test type argument in expand_na", {
  
  example_na_expanded <- expand_na(data = example_na, type = c("dbl", "int"))
  
  expect_equal(
    names(example_na_expanded), 
    c("age", "sex", "labor_force", "health", "hours_NA", "hours", "wages_NA", "wages")
  )
  
})


test_that("Custom na_value conversion", {
  
  # create custom NA filter
  example_na_custom <- example_na %>%
    tidyr::replace_na(
      list("wages" = -999)
    )
  
  # expect no NA values and no wages_NA when using custom value for NA wages
  example_na_expanded_no_custom <- expand_na(
    data = example_na_custom
  )
  expect_false("wages_NA" %in% names(example_na_expanded_no_custom))
  
  # convert custom value to NA
  example_na_expanded_custom <- enforce_custom_na(
    data = example_na_custom,
    col_schema = list(
      "wages" = list(
        dtype = "dbl",
        na_value = -999
      )
    )
  )
  
  # confirm wages identical to original
  expect_identical(
    example_na_expanded[["wages"]],
    example_na_expanded_custom[["wages"]]
  )

})

test_that("expand_na skip_vars", {
  
  example_na_expanded_skip <- expand_na(data = example_na,
                                        skip_vars = c("sex", 
                                                      "labor_force"))
  
  expect_equal(
    names(example_na_expanded_skip), 
    c("age", "sex", "labor_force", "health_NA", 
      "health", "hours_NA", "hours", "wages_NA", "wages")
  )
  
})