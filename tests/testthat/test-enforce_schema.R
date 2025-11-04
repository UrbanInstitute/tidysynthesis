test_that("sum_to_cast_categorical basic functionality", {
  
  test_col <- acs_conf[["county"]]
  
  # expect normal factor casting returns the same values
  expect_equal(.sum_to_cast_categorical(test_col, "fct") |> class(), "factor")
  expect_true(all(.sum_to_cast_categorical(test_col, "fct") == test_col))
  
  # expect new specified levels are maintained
  expect_identical(
    .sum_to_cast_categorical(
      test_col, "fct", levels = c("Other", "Douglas")) |> levels(), 
    c("Other", "Douglas")
  )
  
  # expect character casting works 
  expect_equal(.sum_to_cast_categorical(test_col, "chr") |> class(), "character")
  
  # expect logical casting works 
  test_col_lgl <- test_col |> 
    forcats::fct_collapse(Douglas = "Douglas", other_level = "Not Douglas") |> 
    as.integer() - 1
  expect_equal(.sum_to_cast_categorical(test_col_lgl, "lgl") |> class(), "logical")
  
  # expect invalid casting with incorrect type
  expect_error(
    .sum_to_cast_categorical(test_col, "not_a_dtype_sum"),
    regexp = "Cannot enforce_schema() with unsupported categorical type: not_a_dtype_sum",
    fixed = TRUE
  )
  
})

test_that("sum_to_cast_numeric basic functionality", {
  
  test_col <- acs_conf[["age"]]
  
  # expect integer casting works
  expect_equal(.sum_to_cast_numeric(acs_conf[["age"]], "int") |> class(), "integer")
  
  # expect double casting works
  expect_equal(.sum_to_cast_numeric(acs_conf[["age"]], "dbl") |> class(), "numeric")
  
  # expect invalid casting with incorrect type
  expect_error(
    .sum_to_cast_numeric(test_col, "not_a_dtype_sum"),
    regexp = "Cannot enforce_schema() with unsupported numeric type: not_a_dtype_sum",
    fixed = TRUE
  )
  
})

test_that("schema coerce_to_doubles", {
  
  # create roadmap with modified numeric types...
  old_roadmap <- roadmap(conf_data = acs_conf |>
                            dplyr::mutate(age = as.integer(age),
                                          wgt = as.integer(wgt)),
                          start_data = acs_start |>
                            dplyr::mutate(wgt = as.integer(wgt))) |>
    # ...then specify coerce_to_doubles
    update_schema(coerce_to_doubles = TRUE)
  
  # enforce schema
  new_roadmap <- enforce_schema(old_roadmap)
  
  # expect successful conversions in data
  expect_equal(class(old_roadmap[["conf_data"]][["age"]]), "integer")
  expect_equal(class(new_roadmap[["conf_data"]][["age"]]), "numeric")
  expect_equal(class(old_roadmap[["start_data"]][["wgt"]]), "integer")
  expect_equal(class(new_roadmap[["start_data"]][["wgt"]]), "numeric")
  
  # expect col_schema updates
  expect_equal(
    old_roadmap[["schema"]][["col_schema"]][["age"]][["dtype"]], "int"
  )
  expect_equal(
    new_roadmap[["schema"]][["col_schema"]][["age"]][["dtype"]], "dbl"
  )
  expect_equal(
    old_roadmap[["schema"]][["col_schema"]][["wgt"]][["dtype"]], "int"
  )
  expect_equal(
    new_roadmap[["schema"]][["col_schema"]][["wgt"]][["dtype"]], "dbl"
  )
  
})

test_that("schema coerce_to_factors from chr and lgl", {
  
  # create roadmap with modified categorical types...
  old_roadmap <- roadmap(conf_data = acs_conf |>
                           dplyr::mutate(sex = (sex == "Female"),
                                         marst = as.character(marst)), 
                         start_data = acs_start |>
                           dplyr::mutate(sex = (sex == "Female"),
                                         marst = as.character(marst))) |>
    # ...then specify coerce_to_doubles
    update_schema(coerce_to_factors = TRUE)
  
  # enforce schema
  new_roadmap <- enforce_schema(old_roadmap)
  
  # expect columns properly converted to factors
  expect_equal(class(old_roadmap[["conf_data"]][["sex"]]), "logical")
  expect_equal(class(new_roadmap[["conf_data"]][["sex"]]), "factor")
  expect_equal(class(old_roadmap[["conf_data"]][["marst"]]), "character")
  expect_equal(class(new_roadmap[["conf_data"]][["marst"]]), "factor")
  expect_equal(class(old_roadmap[["start_data"]][["sex"]]), "logical")
  expect_equal(class(new_roadmap[["start_data"]][["sex"]]), "factor")
  expect_equal(class(old_roadmap[["start_data"]][["marst"]]), "character")
  expect_equal(class(new_roadmap[["start_data"]][["marst"]]), "factor")
  
  # expect updated col_schema
  expect_equal(
    old_roadmap[["schema"]][["col_schema"]][["sex"]][["dtype"]], "lgl"
  )
  expect_equal(
    new_roadmap[["schema"]][["col_schema"]][["sex"]][["dtype"]], "fct"
  )
  expect_equal(
    old_roadmap[["schema"]][["col_schema"]][["marst"]][["dtype"]], "chr"
  )
  expect_equal(
    new_roadmap[["schema"]][["col_schema"]][["marst"]][["dtype"]], "fct"
  )
  
})

test_that("schema enforce factors with updated levels", {
  
  # create roadmap with new factor levels
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start) |>
    update_schema(col_schema = list(
      "county" = list(
        # example with adding one new level
        "levels" = c("Other", "Douglas", "Lancaster", "Sarpy", "New")),
      "gq" = list(
        # example dropping an existing level `Institution`
        "levels" = c("Household", "Other GQ"))))
  
  # enforce schema
  new_roadmap <- enforce_schema(old_roadmap)
  
  # expect updated levels when adding new unobserved levels
  expect_identical(
    levels(old_roadmap[["conf_data"]][["county"]]),
    c("Other", "Douglas", "Lancaster", "Sarpy")
  )
  expect_identical(
    levels(new_roadmap[["conf_data"]][["county"]]),
    c("Other", "Douglas", "Lancaster", "Sarpy", "New")
  )
  expect_identical(
    levels(old_roadmap[["start_data"]][["county"]]),
    c("Other", "Douglas", "Lancaster", "Sarpy")
  )
  expect_identical(
    levels(new_roadmap[["start_data"]][["county"]]),
    c("Other", "Douglas", "Lancaster", "Sarpy", "New")
  )
  
  # expect updated levels when removing existing observed levels
  expect_identical(
    levels(old_roadmap[["conf_data"]][["gq"]]),
    c("Other GQ", "Household", "Institution")
  )
  expect_identical(
    levels(new_roadmap[["conf_data"]][["gq"]]),
    c("Household", "Other GQ", "NA") # missing values in conf_data
  )
  expect_identical(
    levels(old_roadmap[["start_data"]][["gq"]]),
    c("Other GQ", "Household", "Institution")
  )
  expect_identical(
    levels(new_roadmap[["start_data"]][["gq"]]),
    c("Household", "Other GQ") # no missing values in start_data
  )
  
})


test_that("schema custom na_value", {
  
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start) |>
    update_schema(col_schema = list(
      "age" = list("na_value" = 0),
      "classwkr" = list("na_value" = "N/A")),
      na_factor_to_level = FALSE)
  
  new_roadmap <- enforce_schema(old_roadmap)
  
  # check that NA values were properly inserted
  expect_true(mean(is.na(old_roadmap[["conf_data"]][["age"]])) == 0)
  expect_true(mean(is.na(new_roadmap[["conf_data"]][["age"]])) > 0)
  expect_true(mean(is.na(old_roadmap[["conf_data"]][["classwkr"]])) == 0)
  expect_true(mean(is.na(new_roadmap[["conf_data"]][["classwkr"]])) > 0)
  
  # check that the NA proportions were properly updated
  expect_true(
    old_roadmap[["schema"]][["col_schema"]][["age"]][["na_prop"]] == 0
  )
  expect_true(
    new_roadmap[["schema"]][["col_schema"]][["age"]][["na_prop"]] > 0
  )
  expect_true(
    old_roadmap[["schema"]][["col_schema"]][["classwkr"]][["na_prop"]] == 0
  )
  expect_true(
    new_roadmap[["schema"]][["col_schema"]][["classwkr"]][["na_prop"]] > 0
  )
  
})

test_that("schema with na_factor_to_level", {
  
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start) |>
    update_schema(col_schema = list(
      "age" = list("na_value" = 0),
      "classwkr" = list("na_value" = "N/A")),
      na_factor_to_level = TRUE)
  
  new_roadmap <- enforce_schema(old_roadmap)
  
  # check that "NA" was inserted as a level
  expect_true("NA" %in% levels(new_roadmap[["conf_data"]][["classwkr"]]))
  
  # check that this aligns with downstream metadata
  expect_true(mean(is.na(old_roadmap[["conf_data"]][["classwkr"]])) == 0)
  expect_true(mean(is.na(new_roadmap[["conf_data"]][["classwkr"]])) == 0)
  expect_true(
    old_roadmap[["schema"]][["col_schema"]][["classwkr"]][["na_prop"]] == 0
  )
  expect_true(
    new_roadmap[["schema"]][["col_schema"]][["classwkr"]][["na_prop"]] > 0
  )
  
})

test_that("schema updates without NA indicators", {
  
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start) |>
    update_schema(na_numeric_to_ind = FALSE)
  
  new_roadmap <- enforce_schema(old_roadmap)
  
  # ensure no new columns added by enforcing schema
  expect_identical(names(old_roadmap[["conf_data"]]),
                   names(new_roadmap[["conf_data"]]))
  
})

test_that("schema_updates with NA indicators", {
  
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start) |>
    update_schema(na_numeric_to_ind = TRUE)
  
  new_roadmap <- enforce_schema(old_roadmap)
  
  # ensure new column created for indicating missing numeric values
  expect_equal(
    setdiff(names(new_roadmap[["conf_data"]]), names(old_roadmap[["conf_data"]])),
    "inctot_NA"
  )
  
  # expect auxiliary schema elements reflect new indicator variable
  expect_true(
    "inctot_NA" %in% new_roadmap[["schema"]][["synth_vars"]]
  )
  
  expect_true(
    "inctot_NA" %in% names(new_roadmap[["schema"]][["no_variation"]])
  )

})


test_that("schema_updates with NA indicators in first position", {
  
  old_roadmap <- roadmap(
    conf_data = acs_conf |>
      dplyr::select(dplyr::all_of(c(names(acs_start), "inctot"))), 
    start_data = acs_start
  ) |>
    update_schema(na_numeric_to_ind = TRUE)
  
  new_roadmap <- enforce_schema(old_roadmap)
  
  # ensure new column created for indicating missing numeric values
  expect_equal(
    setdiff(names(new_roadmap[["conf_data"]]), names(old_roadmap[["conf_data"]])),
    "inctot_NA"
  )
  
  # expect auxiliary schema elements reflect new indicator variable
  expect_true(
    "inctot_NA" %in% new_roadmap[["schema"]][["synth_vars"]]
  )
  
  expect_true(
    "inctot_NA" %in% names(new_roadmap[["schema"]][["no_variation"]])
  )
  
})

