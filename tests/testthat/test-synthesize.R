# set up
start_data <- dplyr::select(mtcars, cyl, vs, am, gear, carb)

# roadmaps 
roadmap <- roadmap(conf_data = mtcars,
                   start_data = start_data) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

roadmap4 <- roadmap(
  conf_data = mtcars,
  start_data = dplyr::select(mtcars, cyl, vs, am, gear, carb) %>%
    dplyr::sample_n(size = 50, replace = TRUE)
) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

# synth_specs 
dt_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine(engine = "rpart") %>%
  parsnip::set_mode(mode = "regression")

synth_spec <- synth_spec(default_regression_model = dt_mod,
                         default_regression_sampler = sample_rpart)


# tests -------------------------------------------------------------------

test_that("synthesize() returns data.frame of proper size", {
  
  expect_warning(
    presynth1 <- presynth(roadmap = roadmap,
                          synth_spec = synth_spec)
  )
  
  synth1 <- synthesize(presynth1)
  
  expect_is(synth1, "postsynth")
  expect_equal(dim(synth1$synthetic_data), c(32, 11))

  expect_is(synth1$ldiversity, "data.frame")
  expect_equal(dim(synth1$ldiversity), c(32, 6))
  expect_equal(names(synth1$ldiversity), 
               roadmap$visit_sequence$visit_sequence)
  
  
})

test_that("synthesize() works with extractors", {
  
  expect_warning(
    presynth2 <- presynth(
      roadmap = roadmap,
      synth_spec = synth_spec %>%
        update_synth_spec(
          default_extractor = parsnip::extract_fit_engine
        )
    )
  )
  
  synth2 <- synthesize(presynth2)
  
  for (n in presynth2$roadmap$visit_sequence$visit_sequence) {
    
    expect_true("rpart" %in% class(synth2$extractions[[n]]))
    
  }
  
})

test_that("synthesize() returns correct number of replicates", {
  
  expect_warning(
    presynth3 <- presynth(
      roadmap = roadmap %>%
        update_replicates(model_sample_replicates = 2),
      synth_spec = synth_spec
    )
  )
  
  synth3 <- synthesize(presynth3)
  
  expect_equal(length(synth3), 2)
  expect_equal(dim(synth3[[1]]$synthetic_data), dim(synth3[[2]]$synthetic_data))
  
})


test_that("synthesize() properly handles start_data", {
  
  expect_warning(
    presynth4 <- presynth(roadmap = roadmap4,
                          synth_spec = synth_spec)
  )

  synth4 <- synthesize(presynth4)
  expect_equal(nrow(synth4$synthetic_data), 50)
  
})
