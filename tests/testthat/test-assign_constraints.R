test_that("assign_constraints_num returns expected values", {
  
  conf_data <- mtcars[1:3, c("cyl", "disp", "mpg", "wt")]
  start_data <- mtcars[1:3, "cyl", drop = FALSE]
  
  roadmap <- roadmap(conf_data = conf_data, start_data = start_data) |>
    add_sequence_manual(disp, mpg, wt)
  
  schema <- schema(conf_data = conf_data, start_data = start_data)
  
  constraints_by_hand_num <- 
    dplyr::bind_cols(
      mtcars[1:3, ],
      .assigned_min = c(0, 0, 0),
      .assigned_max = c(15, 12, Inf)
    )
  
  # constraints by function
  constraints_df_num <- tibble::tribble(
    ~var, ~min, ~max, ~conditions,
    "mpg", 0, Inf, "TRUE",
    "mpg", -Inf, 15, "cyl == 6",
    "mpg", -Inf, 12, "wt >= 2.8"
  ) 
  
  constraints_by_function_num <- assign_constraints_num(
    synth_data = mtcars[1:3, ], 
    constraints = constraints(
      schema = schema,
      constraints_df_num = constraints_df_num
    )$constraints_num[["mpg"]]
  )
  
  expect_equal(constraints_by_function_num, constraints_by_hand_num)
  
})


test_that("assign_constraints_cat returns expected values", {
  
  schema <- schema(
    conf_data = dplyr::select(mtcars, mpg, cyl, disp, carb, vs, am, gear) |>
      dplyr::mutate(vs = factor(vs), 
                    am = factor(am),
                    gear = factor(gear)),
    start_data = dplyr::select(mtcars, cyl)
  )
  
  constraints_df_cat <-  tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "vs", "1", NA, "TRUE",
    "gear", NA, "5", "TRUE",
    "gear", "3", NA, "cyl == 8"
  )
  
  constraints_by_function_cat <- assign_constraints_cat(
    synth_data = dplyr::select(mtcars, cyl, vs, gear),
    constraints = constraints(
      schema = schema,
      constraints_df_cat = constraints_df_cat
    )$constraints_cat[["gear"]]
  )
  
  expect_true(
    all(
      # directly applying the constraint condition IDs...
      dplyr::mutate(
        mtcars, 
        .condition_id = dplyr::if_else(
          cyl == 8, 2, 1
        ), 
        .keep = "none"
      ) == (
        # ...should be the same as using assign_constraints_cat
        constraints_by_function_cat$synth_data$.condition_id
      )
    )
  )
  
  expect_true(
    all(
      # directly applying the constraint modes...
      dplyr::mutate(
        mtcars, 
        .constraint_mode = dplyr::if_else(
          cyl == 8, "inclusion", "exclusion"
        ), 
        .keep = "none"
      ) == (
        # ...should be the same as using assign_constraints_cat
        constraints_by_function_cat$synth_data |>
          dplyr::inner_join(constraints_by_function_cat$condition_defs,
                            by = ".condition_id") |>
          dplyr::pull(".condition_mode")
      )
    )
  )
  
})

