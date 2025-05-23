acs_schema <- schema(conf_data = acs_conf, start_data = acs_start)
acs_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start)

# Constructor tests ----------------------------------------------------------

test_that("Basic schema object properties", {
  
  # expect correct s3 class
  expect_s3_class(acs_schema, "schema")
  
  default_schema <- acs_schema[["col_schema"]]
  
  # expect one col_schema entry per confidential data column
  expect_equal(length(names(acs_conf)), length(names(default_schema)))
  
  # expect all default dtypes either dbl or fct
  expect_true(
    all(purrr::map_lgl(.x = default_schema, 
                       .f = \(x) { x[["dtype"]] %in% c("fct", "dbl") })))
  
  # expect levels = NULL whenever dtype != fct
  expect_true(
    all(purrr::map_lgl(
      .x = default_schema, 
      .f = \(x) { (x[["dtype"]] == "fct") | is.null(x[["levels"]]) } )))
  
  # expect non-zero na_prop values for missing-eligible columns
  expect_true(
    all(purrr::map_lgl(
      .x = names(default_schema),
      .f = \(x) { (
        (default_schema[[x]][["na_prop"]] > 0) & (x %in% c("inctot", "empstat")) | 
          (default_schema[[x]][["na_prop"]] == 0) & !(x %in% c("inctot", "empstat")) 
      ) }
    ))
  )
  
  # expect correct synth_vars
  expect_identical(
    acs_schema[["synth_vars"]],
    c("hcovany", "empstat", "classwkr", "age", 
      "famsize", "transit_time", "inctot") 
  )
  
  # expect correct no_variation properties
  expect_true(all(!acs_schema[["no_variation"]]))
  
})

test_that("schema encodes no_variation variables properly", {
  
  test_schema <- schema(conf_data = acs_conf %>% 
                          dplyr::mutate(const = 1),
                        start_data = acs_start)
  
  # expect dummy variable to be flagged as having no variation
  expect_true(test_schema[["no_variation"]][["const"]])
  
})

test_that("col_schema encodes tibble dtypes", {
  
  # when manually casting columns to new tibble dtypes...
  test_schema <- schema(
    conf_data = acs_conf %>% 
      dplyr::mutate(age = as.integer(age),
                    transit_time = as.logical(transit_time)),
    start_data = acs_start
  )[["col_schema"]]
  
  # ...expect that the new tibble dyptes are reflected in col_schema
  expect_equal(test_schema[["age"]][["dtype"]], "int")
  expect_equal(test_schema[["transit_time"]][["dtype"]], "lgl")
  
})

test_that("col_schema argument parsing", {
  
  # expect errors for invalid argument types
  expect_error(
    schema(conf_data = "not a data.frame", start_data = acs_start)
  )
  
  expect_error(
    schema(conf_data = acs_conf, start_data = "not a data.frame")
  )
  
  expect_error(
    schema(conf_data = acs_conf, 
           start_data = acs_start, 
           col_schema = c("not", "a", "list"))
  )
  
  expect_error(
    schema(conf_data = acs_conf, 
           start_data = acs_start, 
           enforce = "not a logical")
  )
  
  expect_error(
    schema(conf_data = acs_conf, 
           start_data = acs_start, 
           coerce_to_factors = "not a logical")
  )
  
  expect_error(
    schema(conf_data = acs_conf, 
           start_data = acs_start, 
           coerce_to_doubles = "not a logical")
  )
  
  expect_error(
    schema(conf_data = acs_conf, 
           start_data = acs_start, 
           na_factor_to_level = "not a logical")
  )
  
  expect_error(
    schema(conf_data = acs_conf, 
           start_data = acs_start, 
           na_numeric_to_ind = "not a logical")
  )
  
})

# Validator tests ------------------------------------------------------------

test_that("validate_schema expected errors", {
  
  # error if input is not a roadmap
  expect_error(validate_schema("not_a_roadmap"))
  
  # error if columns from col_schema not in conf_data
  expect_error(
    validate_schema(
      acs_roadmap %>%
        update_schema(col_schema = list("not_a_column" = list("dtype" = "dbl")))
    )
  )
  
  # error if unsupported dtype provided
  expect_error(
    validate_schema(
      acs_roadmap %>%
        update_schema(col_schema = list("gq" = list("dtype" = "notatype")))
    )
  )
  
  # error if unsupported col_schema fields provided
  expect_error(
    validate_schema(
      acs_roadmap %>%
        update_schema(col_schema = list("gq" = list("notafield" = "dbl")))
    )
  )
  
})


test_that("validate_schema expected warning messages", {
  
  # message if data contains variable with no variation
  expect_message(
    validate_schema(
      roadmap(conf_data = acs_conf %>% dplyr::mutate(novar = 1),
              start_data = acs_start)
    )
  )
  
  # message if data contains factor with empty levels 
  expect_message(
    validate_schema(
      roadmap(
        conf_data = acs_conf %>% dplyr::mutate(
          hcovany = factor(hcovany, 
                           levels = c("No health insurance coverage", 
                                      "With health insurance coverage", 
                                      "Other"))),
        start_data = acs_start
      )
    )
  )
  
})


# Tidy API tests -------------------------------------------------------------

test_that("add_schema functionality", {
  
  # add a new schema to an existing roadmap
  old_roadmap <- acs_roadmap
  new_schema <- schema(conf_data = acs_conf, start_data = acs_start,
                       enforce = FALSE)
  new_roadmap <- old_roadmap %>%
    add_schema(new_schema)
  
  # expect new object is a roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new object reflects new schema
  expect_identical(new_roadmap[["schema"]], new_schema)
  
})

test_that("update_schema functionality", {
  
  old_roadmap <- acs_roadmap
  new_roadmap <- old_roadmap %>% 
    update_schema(col_schema = list("wgt" = list("dtype" = "int")),
                  enforce = FALSE)
  
  # expect new object is a roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new object reflects new schema
  expect_equal(
    new_roadmap[["schema"]][["col_schema"]][["wgt"]][["dtype"]], "int"
  )
  expect_equal(new_roadmap[["schema"]][["enforce"]], FALSE)
  
  # expect new object retains existing col_schema elements
  # (in this case, all but the last element, the 12th column "wgt")
  expect_identical(
    old_roadmap[["schema"]][["col_schema"]][1:11], 
    new_roadmap[["schema"]][["col_schema"]][1:11]
  )
  
  
})

test_that("reset_schema functionality", {
  
  old_roadmap <- roadmap(
    conf_data = acs_conf, 
    start_data = acs_start,
    schema = schema(conf_data = acs_conf, 
                    start_data = acs_start,
                    col_schema = list("wgt" = list("dtype" = "int")), 
                    enforce = FALSE))
  
  new_roadmap <- reset_schema(old_roadmap)
  
  # expect new object is a roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new schema is the same as the default one
  expect_identical(
    new_roadmap[["schema"]],
    schema(conf_data = acs_conf, start_data = acs_start)
  )
  
})


test_that("print.schema functionality", {
  
  test_schema <- schema(conf_data = acs_conf, start_data = acs_start)
  
  expect_output(print(
    test_schema), 
    "Schema: 12 columns"
  )
  
  expect_output(
    print(test_schema),
    paste(c(rep("fct", 7), rep("dbl", 5)), collapse=" ")
  )
  
})