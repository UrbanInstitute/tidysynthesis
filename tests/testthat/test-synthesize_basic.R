# set up
start_data <- dplyr::select(mtcars, cyl, vs, am, gear, carb)

roadmap <- roadmap(conf_data = mtcars,
                   start_data = start_data) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")


dt_mod <- parsnip::decision_tree() |>
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")

synth_spec <- synth_spec(default_regression_model = dt_mod,
                         default_regression_sampler = sample_rpart)


# synthesis 1 -------------------------------------------------------------


test_that("Basic synthesis", {
  
  set.seed(20201030)
  
  expect_warning(
    presynth1 <- presynth(roadmap = roadmap,
                          synth_spec = synth_spec)
    
  )
  expect_no_warning(
    synth1 <- synthesize(presynth = presynth1)
  )
  
})