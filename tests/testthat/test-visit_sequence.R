acs_schema_nw <- schema(conf_data = acs_conf_nw, start_data = acs_start_nw)

test_that("visit_sequence constructor defaults", {
  
  # create default visit_sequence
  vs <- visit_sequence(acs_schema_nw)
  
  # expect right class
  expect_s3_class(vs, "visit_sequence")
  
  # default visit_sequence matches variables not in start_data
  expect_identical(
    vs[["default_sequence"]],
    c("hcovany", "empstat", "classwkr", "age", "famsize", 
      "transit_time", "inctot")
  )
  expect_null(vs[["built_sequence"]])
  expect_true(all(vs[["visit_method"]] == "default"))
  
})

test_that("visit_sequence constructor weight enquo", {
  
  # create default visit_sequence with weight_var
  vs <- visit_sequence(acs_schema_nw, weight_var = famsize)
  
  # expect weight variable passed as quosure
  expect_equal(
    rlang::as_name(rlang::quo_get_expr(vs[["weight_var"]])),
    "famsize"
  )
  
})

test_that("validate_visit_sequence errors", {
  
  # error if synthesize_weight = TRUE and weight_var in start_data
  expect_error(
    roadmap(conf_data = acs_conf_nw, start_data = acs_start) |>
      update_visit_sequence(weight_var = wgt) |>
      validate_visit_sequence()
  )
  
  # error if visit_sequence does not contain all synth_vars
  expect_error(
    roadmap(conf_data = acs_conf_nw, start_data = acs_start_nw) |>
      add_visit_sequence(
        visit_sequence = visit_sequence(
          schema = schema(conf_data = acs_conf |> dplyr::select(wgt),
                          start_data = acs_start_nw)
        )
      ) |>
      validate_visit_sequence()
  )
  
})

test_that("visit_sequence drops weight_var", {
  
  schema <- schema(
    conf_data = acs_conf, 
    start_data = acs_start
  )
  
  visit_sequence <- visit_sequence(
    schema = schema,
    weight_var = wgt, 
    synthesize_weight = FALSE
  )
  
  expect_false("wgt" %in% visit_sequence$visit_sequence)
  
})

test_that("visit_sequence respects NA variable ordering", {
  
  roadmap <- roadmap(
    conf_data = acs_conf_nw, 
    start_data = acs_start_nw
  ) |> 
    enforce_schema() |>
    add_sequence_manual(inctot, inctot_NA)
  
  expect_error(
    validate_visit_sequence(roadmap)
  )
  
})

test_that("print.visit_sequence", {
  
  vs <- visit_sequence(acs_schema_nw)
  
  expected_output <- paste0("default:", vs$visit_sequence)
  
  for (output in expected_output) {
    
    expect_output(print(vs), output)
    
  }
  
  
})

