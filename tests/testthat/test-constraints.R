# create a roadmap
data <- dplyr::select(mtcars, mpg, cyl, disp, carb, vs, am, gear) |>
  dplyr::mutate(vs = factor(vs), 
                am = factor(am),
                gear = factor(gear))

start_data <- dplyr::select(data, cyl) |>
  dplyr::slice_sample(n = 30)

schema <- schema(conf_data = data, start_data = start_data)

# create a correctly formatted constraints tibble
constraints_df_num <- tibble::tribble(
  ~var, ~min, ~max, ~conditions,
  "mpg", 0, Inf, "TRUE",
  "mpg", -Inf, 15, "cyl == 6",
  "mpg", -Inf, 12, "cyl == 8",
  "disp", 0, 150, "TRUE"
)

constraints_df_cat <- tibble::tribble(
  ~var, ~allowed, ~forbidden, ~conditions,
  "vs", "1", NA, "TRUE",
  "gear", NA, "5", "TRUE",
  "gear", NA, "3", "cyl == 8"
)

# manually create the correct constraints list for tests
constraints_list_num <- list(
  mpg = tibble::tibble(var = c("mpg", "mpg", "mpg"), 
                       min = c(0, -Inf, -Inf), 
                       max = c(Inf, 15, 12), 
                       conditions = c("TRUE", "cyl == 6", "cyl == 8")),
  disp = tibble::tibble(var = "disp", 
                        min = 0, 
                        max = 150, 
                        conditions = "TRUE"),
  carb = tibble::tibble(var = "carb", 
                        min = -Inf, 
                        max = Inf, 
                        conditions = "TRUE")
)

constraints_list_cat <- list(
  vs = tibble::tibble(var = c("vs"), 
                      allowed = c("1"), 
                      forbidden = as.character(c(NA)),
                      conditions = c("TRUE")),
  am = tibble::tibble(var = c("am"), 
                      allowed = as.character(c(NA)), 
                      forbidden = as.character(c(NA)),
                      conditions = c("TRUE")),
  gear = tibble::tibble(var = c("gear", "gear"), 
                        allowed = as.character(c(NA, NA)), 
                        forbidden = c("5", "3"),
                        conditions = c("TRUE", "cyl == 8"))
)

test_that("constraints() input errors work ", {

  # schema should be a schema
  expect_error(
    constraints(schema = 1),
    regexp = "`schema` must be a schema object",
    fixed = TRUE
  )

  # numeric constraints should be a data.frame
  expect_error(
    constraints(
      schema = schema, 
      constraints_df_num = 1
    ),
    regexp = "`constraints_df_num` must be a data.frame if supplied",
    fixed = TRUE
  )
  # numeric constraints should be a properly formatted data.frame
  expect_error(
    constraints(
      schema = schema, 
      constraints_df_num = constraints_df_num |> 
        dplyr::select(
          dplyr::all_of(
            c("var", "min", "max")
          )
        )
    ),
    regexp = "constraints_df_num missing required column(s): conditions",
    fixed = TRUE
  )
  
  # categorical constraints should be a data.frame
  expect_error(
    constraints(schema = schema, constraints_df_cat = 1),
    regexp = "`constraints_df_cat` must be a data.frame if supplied",
    fixed = TRUE
  )
  
  # numeric constraints should be a properly formatted data.frame
  expect_error(
    constraints(
      schema = schema, 
      constraints_df_cat = constraints_df_cat |> 
        dplyr::select(
          dplyr::all_of(
            c("var", "allowed", "forbidden")
          )
        )
    ),
    regexp = "constraints_df_cat missing required column(s): conditions",
    fixed = TRUE
  )
  
  # max_z_num should be a non-negative integer
  expect_error(
    constraints(
      schema = schema, 
      constraints_df_num = constraints_df_num, 
      max_z_num = -1
    ),
    regexp = "`max_z_num` values must be non-negative",
    fixed = TRUE
  )
  
  expect_error(
    constraints(
      schema = schema, 
      constraints_df_num = constraints_df_num, 
      max_z_num = 1.5
    ),
    regexp = "`max_z_num` values must be integers",
    fixed = TRUE
  )
  
  # max_z_cat should be a non-negative integer
  expect_error(
    constraints(
      schema = schema, 
      constraints_df_cat = constraints_df_cat, 
      max_z_num = -1
    ),
    regexp = "`max_z_num` values must be non-negative",
    fixed = TRUE
  )
  
  expect_error(
    constraints(
      schema = schema, 
      constraints_df_cat = constraints_df_cat, 
      max_z_num = 1.5
    ),
    regexp = "`max_z_num` values must be integers",
    fixed = TRUE
  )

})

test_that("constraints() outputs are correct with defaults ", {
  
  constraints_defaults <- constraints(schema = schema)
  
  expect_s3_class(constraints_defaults, "constraints")
  
  expect_equal(
    constraints_defaults[["constraints_num"]], 
    list("mpg" = NULL, "disp" = NULL, "carb" = NULL)
  )
  
  expect_equal(
    constraints_defaults[["constraints_cat"]], 
    list("vs" = NULL, "am" = NULL, "gear" = NULL)
  )
  
  expect_equal(constraints_defaults[["max_z_num"]], 
               list("mpg" = 0, "disp" = 0, "carb" = 0))
  
  expect_equal(constraints_defaults[["max_z_cat"]], 
               list("vs" = 0, "am" = 0, "gear" = 0))
  
})

test_that("constraints() outputs are correct with customs ", {
  
  constraints_custom <- constraints(
    schema = schema, 
    constraints_df_num = constraints_df_num, 
    constraints_df_cat = constraints_df_cat,
    max_z_num = 1,
    max_z_cat = 2
  )
  
  expect_equal(constraints_custom[["constraints_num"]], 
               constraints_list_num)
  
  expect_equal(constraints_custom[["constraints_cat"]], 
               constraints_list_cat)
  
  expect_equal(constraints_custom[["max_z_num"]], 
               list("mpg" = 1, "disp" = 1, "carb" = 1))
  
  expect_equal(constraints_custom[["max_z_cat"]], 
               list("vs" = 2, "am" = 2, "gear" = 2))
  
})

test_that("constraints() outputs are correct with customs with p length max_z_num", {
  
  constraints_custom_p <- constraints(
    schema = schema, 
    constraints_df_num = constraints_df_num, 
    max_z_num = list(mpg = 1, cyl = 2, disp = 3)
  )
  
  expect_identical(
    constraints_custom_p[["max_z_num"]], 
    list("mpg" = 1, "cyl" = 2, "disp" = 3)
  )
  
  constraints_custom_p2 <- constraints(
    schema = schema, 
    constraints_df_cat = constraints_df_cat, 
    max_z_cat = list(vs = 1, gear = 2)
  )
  
  expect_identical(
    constraints_custom_p2[["max_z_cat"]], 
    list("vs" = 1, "gear" = 2)
  )
  
})

test_that("synthesize() works with constraints() ", {
  
  constraints_custom_p <- constraints(
    schema = schema, 
    constraints_df_num = constraints_df_num, 
    max_z_num = list(mpg = 1, cyl = 2, disp = 3)
  )
  
  roadmap <- roadmap(
    conf_data = data |> dplyr::select(-c(vs, am, gear)),
    start_data = start_data, 
    constraints = constraints_custom_p
  ) |>
    add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")
  
  dt_mod <- parsnip::decision_tree() |>
    parsnip::set_engine(engine = "rpart") |>
    parsnip::set_mode(mode = "regression")
  
  
  synth_spec <- synth_spec(
    default_regression_model = dt_mod, 
    default_regression_sampler = sample_rpart
  )
  
  expect_warning(
    presynth1 <- presynth(roadmap = roadmap, synth_spec = synth_spec)
  )
  
  set.seed(20201030)
  synth1 <- synthesize(presynth = presynth1)
  
  expect_is(synth1, "postsynth")
  expect_equal(dim(synth1$synthetic_data), c(30, 4))
  
  expect_is(synth1$ldiversity, "data.frame")
  expect_equal(dim(synth1$ldiversity), c(30, 3))
  
})
  
test_that("reset_constraints", {
  
  constraints_custom_p <- constraints(
    schema = schema, 
    constraints_df_num = constraints_df_num, 
    max_z_num = list(mpg = 1, cyl = 2, disp = 3)
  )
  
  old_roadmap <- roadmap(
    conf_data = data,
    start_data = start_data, 
    constraints = constraints_custom_p
  )
  
  new_roadmap <- reset_constraints(old_roadmap)
  
  expect_equal(
    new_roadmap$constraints, 
    constraints(schema = schema)
  )
  
})

test_that("update_constraints invalid kwargs", {
  
  expect_error(
    update_constraints(
      roadmap(
        conf_data = data,
        start_data = start_data, 
      ), 
      invalid_kwarg = constraints_df_cat
    ),
    regexp = "Invalid update_constraints argument: invalid_kwarg",
    fixed = TRUE
  )
  
})

test_that("print.constraints", {
  
  constraints_defaults <- constraints(schema = schema)
  
  for (eo in c("mpg: 0", "disp: 0", "carb: 0")) {
    
    expect_output(print(constraints_defaults, eo))
    
  }
  
  for (eo in c("vs: 0", "am: 0", "gear: 0")) {
    
    expect_output(print(constraints_defaults, eo))
    
  }
  
  constraints_custom <- constraints(
    schema = schema, 
    constraints_df_num = constraints_df_num, 
    max_z_num = 1,
    constraints_df_cat = constraints_df_cat,
    max_z_cat = 2
  )
  
  for (eo in c("mpg: 3", "disp: 1", "carb: 1")) {
    
    expect_output(print(constraints_custom, eo))
    
  }
  
  for (eo in c("vs: 1", "am: 0", "gear: 2")) {
    
    expect_output(print(constraints_custom, eo))
    
  }
  
})

test_that("validate_constraints variable name checking", {
  
  # expect invalid variable names to raise error
  expect_error(
    {
      rmap <- roadmap(conf_data = data, start_data = start_data)
      rmap$constraints$constraints_num <- list("invalid" = constraints_df_num)
      validate_constraints(rmap)
    },
    regexp = "Numeric constraint variable(s) not set to be synthesized: invalid",
    fixed = TRUE
  )
  
  expect_error(
    {
      rmap <- roadmap(conf_data = data, start_data = start_data)
      rmap$constraints$constraints_cat <- list("invalid" = constraints_df_cat)
      validate_constraints(rmap)
    },
    regexp = "Categorical constraint variable(s) not set to be synthesized: invalid",
    fixed = TRUE
  )
  
})

test_that("validate_constraints pre-computing constraint failures", {
  
  constraints_df_num_invalid <- tibble::tribble(
    ~var, ~min, ~max, ~conditions,
    "mpg", 0, Inf, "TRUE",
    "mpg", -Inf, 15, "notavariable == 1"
  )
  
  rmap1 <- roadmap(
    conf_data = data,
    start_data = start_data, 
    constraints = constraints(
      schema = schema, 
      constraints_df_num = constraints_df_num_invalid, 
      max_z_num = list(mpg = 1)
    )
  )
  
  expect_error(
    validate_constraints(rmap1)
  )
  
  constraints_df_cat_invalid <- tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "gear", NA, "3", "notavariable == 1"
  )
  
  rmap2 <- roadmap(
    conf_data = data,
    start_data = start_data, 
    constraints = constraints(
      schema = schema, 
      constraints_df_cat = constraints_df_cat_invalid, 
      max_z_cat = list(gear = 1)
    )
  )
  
  expect_error(
    validate_constraints(rmap2)
  )
  
})



test_that("validate_constraints pre-computing constraint future failures", {
  
  constraints_df_num_invalid <- tibble::tribble(
    ~var, ~min, ~max, ~conditions,
    "mpg", 0, Inf, "TRUE",
    "mpg", -Inf, 15, "disp >= 0"
  )
  
  rmap1 <- roadmap(
    conf_data = data,
    start_data = start_data, 
    constraints = constraints(
      schema = schema, 
      constraints_df_num = constraints_df_num_invalid, 
      max_z_num = list(mpg = 1)
    )
  )
  
  expect_error(
    validate_constraints(rmap1)
  )
  
  constraints_df_cat_invalid <- tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "vs", NA, "1", "gear == '4'"
  )
  
  rmap2 <- roadmap(
    conf_data = data,
    start_data = start_data, 
    constraints = constraints(
      schema = schema, 
      constraints_df_cat = constraints_df_cat_invalid, 
      max_z_cat = list(gear = 1)
    )
  )
  
  expect_error(
    validate_constraints(rmap2)
  )
  
})

test_that("validate_constraints invalid allowed levels", {
  
  constraints_df_cat_invalid <- tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "vs", "100", NA, "TRUE"
  )
  
  rmap1 <- roadmap(
    conf_data = data,
    start_data = start_data, 
    constraints = constraints(
      schema = schema, 
      constraints_df_cat = constraints_df_cat_invalid, 
      max_z_cat = list(gear = 1)
    )
  )
  
  expect_error(
    validate_constraints(rmap1)
  )

})

test_that("validate_constraints invalid forbidden levels", {
  
  constraints_df_cat_invalid <- tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "vs", NA, "100", "TRUE"
  )
  
  rmap1 <- roadmap(
    conf_data = data,
    start_data = start_data, 
    constraints = constraints(
      schema = schema, 
      constraints_df_cat = constraints_df_cat_invalid, 
      max_z_cat = list(gear = 1)
    )
  )
  
  expect_warning(
    validate_constraints(rmap1)
  )
  
})
