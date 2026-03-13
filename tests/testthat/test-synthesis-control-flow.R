start_data <- dplyr::select(mtcars, cyl, vs, am)

roadmap1 <- roadmap(conf_data = mtcars,
                   start_data = start_data) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")


dt_mod <- parsnip::decision_tree() |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")

synth_spec <- synth_spec(default_regression_model = dt_mod,
                         default_regression_sampler = sample_rpart)

suppressWarnings(
  {
    working_presynth <- presynth(roadmap = roadmap1,
                                 synth_spec = synth_spec)
    failing_presynth <- presynth(roadmap = roadmap1,
                                 synth_spec = synth_spec) 
  }
)

suppressMessages(
  { full_result <- synthesize(working_presynth) }
)

# intentionally modify the confidential data to induce an error in `qsec`
failing_presynth$roadmap$conf_data$qsec[1] <- "not_a_float"


test_that("Basic functionality for saving partially synthetic data", {
    
    # if we try to synthesize the failing_presynth, we raise expected errors
    expect_error({ synthesize(failing_presynth) }, 
                 regexp = "`qsec` must have class") 
    
    # if we try to synthesize with keep_partial, we raise a warning that 
    # contains the error described above.
    expect_warning(
      { partial_result <- synthesize(failing_presynth, keep_partial = TRUE) },
      regexp = "Error encountered in variable 'qsec'.*`qsec` must have class"
    )
    
    # expect the failing variable to be the last in the visit_sequence (by
    # construction in the roadmap)
    expect_equal(tail(roadmap1$visit_sequence$visit_sequence, n=1), "qsec")
    
    # because the last variable fails, expect the complete result to be one
    # variable shorter than the partial result
    expect_equal(ncol(full_result$synthetic_data), 11)
    expect_equal(ncol(partial_result$synthetic_data), 10)
    
    # expect roles to reflect the incomplete synthesis
    expect_true(all(full_result$roles[3:11] == "synthesized"))
    expect_equal(partial_result$roles[["qsec"]], "unsynthesized")
    expect_true(all(partial_result$roles[3:10] == "synthesized"))
    

})


test_that("Basic functionality for saving workflows", {
  
  expect_no_error({ full_result_cached <- synthesize(working_presynth,
                                                     keep_workflows = TRUE) })
  
  # expect keep_workflows components saved in full result, not by default
  expect_true(is.null(full_result$roadmap))
  expect_true(is.null(full_result$synth_spec))
  expect_true(is.null(full_result$workflows))
  expect_false(is.null(full_result_cached$roadmap))
  expect_false(is.null(full_result_cached$synth_spec))
  expect_false(is.null(full_result_cached$workflows))
  
})


