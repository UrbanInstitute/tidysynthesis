mtcars_start <- dplyr::select(mtcars, cyl, vs, am, gear, carb)

base_rmap <- roadmap(conf_data = mtcars,
                     start_data = mtcars_start) |> 
    add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

# synth_specs 
dt_mod <- parsnip::decision_tree() |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")

base_ss <- synth_spec(default_regression_model = dt_mod,
                      default_regression_sampler = sample_rpart)

test_that("start_data_replicates basic functionality", {
  
  # ensure that adding new start_data_replicates correctly modifies
  # the output dataframe size
  new_rmap <- base_rmap |>
    update_start_method(
      start_func = start_resample, 
      support = "observed",
      n = 20
    ) |> 
    update_replicates(
      start_data_replicates = 3
    )
  
  expect_warning({
    new_post <- synthesize(presynth(roadmap = new_rmap, synth_spec = base_ss))
  })
  
  # new output data should be 20 (start_method output size) * 3 (replicates)
  expect_true(nrow(new_post$synthetic_data) == 20 * 3)
  
})

test_that("model_sample_replicates basic functionality", {
  
  # generate model-sample replicates
  new_rmap <- base_rmap |>
    update_replicates(
      model_sample_replicates = 2
    )
  
  expect_warning({
    new_post <- synthesize(presynth(roadmap = new_rmap, synth_spec = base_ss))
  })
  
  # expect to produce two postsynths, one per model-sample replicate
  expect_true(length(new_post) == 2)
  expect_true(
    all(purrr::map_lgl(new_post, \(x) { nrow(x$synthetic_data) == 32}))
  )
  
})

test_that("end_to_end_replicates basic functionality", {
  
  # generate end-to-end replicates
  new_rmap <- base_rmap |>
    update_start_method(
      start_func = start_resample, 
      support = "observed",
      n = 20
    ) |> 
    update_replicates(
      end_to_end_replicates = 3
    )
  
  expect_warning({
    new_post <- synthesize(presynth(roadmap = new_rmap, synth_spec = base_ss))
  })
  
  # expect to produce two postsynths, one per end-to-end replicate
  expect_true(length(new_post) == 3)
  
  # expect each end-to-end replicate has the correct start data size
  expect_true(
    all(purrr::map_lgl(new_post, \(x) { nrow(x$synthetic_data) == 20}))
  )
  
})

test_that("replicates combined functionality", {
  
  # generate replicates with every setting > 1
  new_rmap <- base_rmap |>
    update_start_method(
      start_func = start_resample, 
      support = "observed",
      n = 20
    ) |> 
    update_replicates(
      start_data_replicates = 2,
      model_sample_replicates = 3,
      end_to_end_replicates = 5
    )
  
  expect_warning({
    new_post <- synthesize(presynth(roadmap = new_rmap, synth_spec = base_ss))
  })
  
  # expect correct number of end-to-end replicates
  expect_true(length(new_post) == 5)
  
  # for each end-to-end replicate...
  expect_true(
    all(
      # expect the correct number of model-sample replicates
      purrr::map_lgl(.x = new_post, .f = \(x) { length(x) == 3 })
    )
  )
  
  expect_true(
    all(
      # for each end-to-end replicate...
      purrr::map_lgl(
        .x = new_post, 
        .f = \(x) { 
          all(
            # and for each model-sample replicate...
            purrr::map_lgl(
              .x = x,
              .f = \(y) { 
                # expect the correct number of start data replicates
                nrow(y$synthetic_data) == 20 * 2
              }
            )
          )
        }
      )
    )
  )
  
  
})