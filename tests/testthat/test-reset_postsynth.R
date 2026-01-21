start_data <- dplyr::select(mtcars, cyl, vs, am)

constraints_df_num <- tibble::tribble(
  ~var, ~min, ~max, ~conditions,
  "drat", 0, Inf, "TRUE",
  "qsec", 0, Inf, "TRUE",
  "carb", 0, Inf, "TRUE"
)


roadmap1 <- roadmap(conf_data = mtcars,
                    start_data = start_data) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg") |>
  update_constraints(constraints_df_num = constraints_df_num,
                     max_z_num = 0)


dt_mod <- parsnip::decision_tree() |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")

synth_spec1 <- synth_spec(
  default_regression_model = dt_mod,
  custom_models = list(
    list(
      vars = c("carb", "mpg", "gear"),
      model = dt_mod
    ),
    list(
      vars = c("hp"),
      model = dt_mod
    )
  ),
  default_regression_sampler = sample_rpart
)

suppressWarnings(
  {
    working_presynth <- presynth(roadmap = roadmap1,
                                 synth_spec = synth_spec1)
    failing_presynth <- presynth(roadmap = roadmap1,
                                 synth_spec = synth_spec1) 
  }
)

suppressMessages(
  { 
    full_result <- synthesize(working_presynth)
    full_result_keep <- synthesize(working_presynth, 
                                   keep_workflows = TRUE)
  }
)

# intentionally modify the confidential data to induce an error in `carb`
failing_presynth$roadmap$conf_data$carb[1] <- "not_a_float"

suppressWarnings({
  
  ex_ps <- synthesize(failing_presynth, keep_workflows=TRUE, keep_partial=TRUE)
  new_synth_vars <- names(ex_ps$roles[ex_ps$roles == "unsynthesized"])
  
})


test_that("postsynth_to_roadmap basic functionality", {
  
  new_rmap <- postsynth_to_roadmap(ex_ps)
  
  # expect starting data to contain incomplete synthesis
  expect_true(ncol(roadmap1$start_data) == 3)
  expect_true(ncol(new_rmap$start_data) == 8)
  
  # expect new visit sequence to contain all unsynthesized variables 
  # in the same order with the same method
  new_synth_vs_ix <- match(new_synth_vars, 
                           roadmap1$visit_sequence$visit_sequence)
  
  expect_true(all(new_rmap$visit_sequence$visit_sequence == new_synth_vars))
  expect_true(all(new_rmap$visit_sequence$visit_method == 
                    roadmap1$visit_sequence$visit_method[new_synth_vs_ix]))
  
  # expect constraints to be correctly subsetted
  new_constraints_num <- new_rmap$constraints$inputs$input_constraints_df_num
  expect_true(nrow(new_constraints_num) == 2)
  expect_true(all(new_constraints_num$var %in% new_synth_vars))
  
})


test_that("postsynth_to_synth_spec basic functionality", {
  
  new_ss <- postsynth_to_synth_spec(ex_ps)
  
  # expect to retain only one element from custom_models with visit_sequence vars
  expect_true(length(new_ss$custom_models) == 1)
  
  # expect each remaining custom_var is in the implied visit sequence
  expect_true(all(
    new_ss$custom_models[[1]][["vars"]] %in% new_synth_vars
  ))
  
})

test_that("synthesize() with postsynth input", {
  
  # reset original confidential data in failing partial postsynth
  ex_ps2 <- ex_ps
  ex_ps2$roadmap$conf_data <- mtcars 
  
  # expect to be able to rerun the synthesis from the failing variable
  expect_no_error(
    expect_warning(
      fixed_postsynth <- synthesize(ex_ps2)
    )
  )
  
  # expect synthesis to match underlying confidential data schema
  expect_true(ncol(fixed_postsynth$synthetic_data) == ncol(mtcars))
  
})

test_that("synthesize() will bypass completed postsynth", {
  
  # expect warning about already completed synthesis
  expect_warning(synthesize(full_result),
                 regexp = "Synthesis already completed")
  
})

test_that(".filter_constraint_var basic functionality", {
  
  expect_null(.filter_constraint_var(constraints_df_num, 
                                     c("mpg")))
  
  expect_null(.filter_constraint_var(NULL, c("drat", "qsec")))
  
  expect_true(
    nrow(.filter_constraint_var(constraints_df_num, 
                                c("drat", "qsec"))) == 2
  )
  
})

test_that(".remove_custom_var basic functionality", {
  
  custom1 <- list(list("vars" = c("a"), "model" = "1"))
  custom2 <- list(list("vars" = c("a", "b"), "model" = "2"))
  
  expect_identical(.remove_custom_var(custom1, "b"), custom1)
  expect_null(.remove_custom_var(custom1, "a"))
  
  expect_identical(.remove_custom_var(custom2, "a")[[1]][["vars"]], c("b"))
  
})

test_that("postsynth_to_* behavior for completed synthesis", {
  
  expect_warning(
    postsynth_to_roadmap(full_result_keep),
    regexp = "Synthesis already completed"
  )
  
  expect_warning(
    postsynth_to_synth_spec(full_result_keep),
    regexp = "Synthesis already completed"
  )
  
})


