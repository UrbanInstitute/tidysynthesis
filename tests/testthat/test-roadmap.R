data <- dplyr::select(mtcars, mpg, cyl, disp)
start_data <- dplyr::select(data, cyl)

roadmap <- roadmap(conf_data = data, start_data = start_data)

test_that("roadmap() input errors work ", {
  
  # conf_data must be a data frame
  expect_error(
    roadmap(conf_data = data, start_data = 1), 
    regexp = "`start_data` must be a data.frame",
    fixed = TRUE
  )
  
  # start_data must be a data frame
  expect_error(
    roadmap(conf_data = 1, start_data = start_data), 
    regexp = "`conf_data` must be a data.frame",
    fixed = TRUE
  )
  
  # start_method must be a start_method object
  expect_error(
    roadmap(conf_data = data, start_data = start_data, start_method = 1), 
    regexp = "`start_method` must be a start_method object",
    fixed = TRUE
  )
  
  # schema must be a schema object
  expect_error(
    roadmap(conf_data = data, start_data = start_data, schema = 1), 
    regexp = "`schema` must be a schema object",
    fixed = TRUE
  )

  # visit_sequence must be a visit_sequence object
  expect_error(
    roadmap(conf_data = data, start_data = start_data, visit_sequence = 1), 
    regexp = "`visit_sequence` must be a visit_sequence object",
    fixed = TRUE
  )
  
  # replicates must be a replicates object
  expect_error(
    roadmap(conf_data = data, start_data = start_data, replicates = 1), 
    regexp = "`replicates` must be a replcates object",
    fixed = TRUE
  )
  
  # constraints must be a constraints object
  expect_error(
    roadmap(conf_data = data, start_data = start_data, constraints = 1), 
    regexp = "`constraints` must be a constraints object",
    fixed = TRUE
  )
  
})

test_that("roadmap() outputs are correct with defaults ", {
  
  # start_method
  expect_s3_class(roadmap[["start_method"]], "start_method")
  expect_equal(roadmap[["start_method"]], start_method())
  expect_identical(roadmap[["start_method"]][["start_func"]], .identity_start)
  
  # schema
  expect_s3_class(roadmap[["schema"]], "schema")
  
  # visit_sequence
  expect_s3_class(roadmap[["visit_sequence"]], "visit_sequence")
  
  # replicates
  expect_s3_class(roadmap[["replicates"]], "replicates")
  
  # constraints
  expect_s3_class(roadmap[["constraints"]], "constraints")
  
})

test_that("roadmap() outputs are correct with customization ", {
  
  schema <- schema(conf_data = data, start_data = start_data, coerce_to_doubles = TRUE)
  
  custom_sampler <- function(data) {
    
    dplyr::slice_sample(data, n = 1000, replace = TRUE)
    
  }
  
  constraints_df_num <- 
    tibble::tribble(
      ~var, ~min, ~max, ~conditions,
      "mpg", 0, Inf, "TRUE",
      "mpg", -Inf, 15, "cyl == 6",
      "mpg", -Inf, 12, "cyl == 8",
      "disp", 0, 150, "TRUE"
    ) 
  
  replicates <- replicates(
    start_data_replicates = 2,
    model_sample_replicates = 2,
    end_to_end_replicates = 2
  )
  
  roadmap_full <- roadmap(
    conf_data = data, 
    start_data = start_data, 
    start_method = start_method(start_func = custom_sampler),
    schema = schema,
    visit_sequence = visit_sequence(schema = schema, synthesize_weight = TRUE),
    replicates = replicates,
    constraints = constraints(schema = schema, constraints_df_num = constraints_df_num)
  )
  
  expect_s3_class(roadmap_full, "roadmap")
  
  # conf_data
  expect_identical(roadmap_full[["conf_data"]], data)
  
  # start_data
  expect_identical(roadmap_full[["start_data"]], start_data)
  
  # start_method
  expect_s3_class(roadmap_full[["start_method"]], "start_method")
  expect_equal(roadmap_full[["start_method"]][["start_func"]], custom_sampler)
  
  # schema
  expect_s3_class(roadmap_full[["schema"]], "schema")
  expect_equal(roadmap_full[["schema"]][["coerce_to_doubles"]], TRUE)
  
  # visit_sequence
  expect_s3_class(roadmap_full[["visit_sequence"]], "visit_sequence")
  expect_equal(roadmap_full[["visit_sequence"]][["synthesize_weight"]], TRUE)
  
  # replicates
  expect_s3_class(roadmap_full[["replicates"]], "replicates")
  expect_equal(roadmap_full[["replicates"]][["total_replicates"]], 8)
  
  # constraints
  expect_s3_class(roadmap_full[["constraints"]], "constraints")
  expect_equal(
    roadmap_full[["constraints"]][["constraints_num"]][["mpg"]],
    dplyr::filter(constraints_df_num, var == "mpg")
  )
  
})

test_that("roadmap() correctly handles variables without variation ", {
  
  conf_data_ident <- tibble::tibble(
    chr_var = c("a", "b", "c"),    
    num_var = c(1, 1, 1),
    fctr_var1 = factor(c("a", "a", "a")),
    fctr_var2 = factor(c("d", "e", "f"))
  )
  
  start_data_ident <- tibble::tibble(
    chr_var = c("a", "b", "c"),
    fctr_var2 = factor(c("d", "e", "f"))
  )
  
  # this should return a message about identity variables
  expect_message(
    validate_roadmap(
      roadmap(conf_data = conf_data_ident, start_data = start_data_ident)
    )
  )
  
  roadmap_ident <- roadmap(conf_data = conf_data_ident, start_data = start_data_ident)
  
  # error integer
  expect_equal(unname(roadmap_ident[["schema"]][["no_variation"]]), c(TRUE, TRUE))
  expect_equal(names(roadmap_ident[["schema"]][["no_variation"]]), c("num_var", "fctr_var1"))
  
})

test_that("roadmap() throws an error for a factor with ordered levels", {
  # create a dataset to roadmap
  data_factor <- data.frame(
    col1 = factor(c("a", "b", "a", "b", "a"), ordered = TRUE, levels = c("a", "b")), 
    col2 = c(-10, 10, -7, 7, -8)
  )
  
  # expect error when creating roadmap involving ordered factor
  expect_error(
    validate_roadmap(
      roadmap(
        conf_data = data_factor, 
        start_data = dplyr::select(data_factor, col2) # select the non-ordered-factor column
      )
    ), 
    regexp = "`col_schema` included unsupported dtype(s) ord",
    fixed = TRUE
  )
  
  expect_no_error(
    roadmap(
      conf_data = data_factor, 
      start_data = dplyr::select(data_factor, col1) # select the ordered-factor column
    )
  )

})

test_that("roadmap() returns a message for factor variables with empty levels ", {
  
  conf_data_empty <- tibble::tibble(
    chr_var = c("a", "b", "c"),    
    fctr_var1 = factor(c("a", "a", "b"), levels = c("a", "b", "c")),
    fctr_var2 = factor(c("a", "a", "b"), levels = c("a", "b", "c"))
  )
  
  start_data_empty <- tibble::tibble(
    chr_var = c("a", "b", "c")
  )

  expect_message(
    validate_roadmap(
      roadmap(conf_data = conf_data_empty, start_data = start_data_empty) 
    )
  )
  
})

test_that("roadmap() throws an error for ordinal variables ", {
  
  expect_error(
    validate_roadmap(
      roadmap(
        conf_data = example_na,
        start_data = dplyr::select(example_na, age)
      )
    ), 
    regexp = "`col_schema` included unsupported dtype(s) ord",
    fixed = TRUE
  )
  
})

test_that("print.roadmap", {
  
  expect_output(
    print(roadmap(conf_data = acs_conf_nw, start_data = acs_start_nw)),
    "conf_data: 1500 observations, 11 variables"
  )
  
  expect_output(
    print(roadmap(conf_data = acs_conf_nw, start_data = acs_start_nw)),
    "start_data: 500 observations, 4 variables"
  )
  
})

