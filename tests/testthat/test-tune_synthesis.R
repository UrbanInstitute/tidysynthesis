# set up
conf_data <- dplyr::select(mtcars, cyl, vs, am, gear, carb, mpg)
start_data <- dplyr::select(mtcars, cyl, vs, am, gear, carb)

roadmap <- roadmap(
  conf_data = conf_data,
  start_data = start_data) |> 
  add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg")

dt_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine(engine = "rpart") %>%
  parsnip::set_mode(mode = "regression")

lm_mod <- lm_mod <- parsnip::linear_reg() %>% 
  parsnip::set_engine("lm")

# synth specs
synth_spec1 <- synth_spec(default_regression_model = dt_mod,
                          default_regression_sampler = sample_rpart)

synth_spec2 <- synth_spec(default_regression_model = lm_mod,
                          default_regression_sampler = sample_lm)

# presynths
suppressWarnings({
  
    presynth1 <- presynth(roadmap = roadmap,
                          synth_spec = synth_spec1)
    
    presynth2 <- presynth(roadmap = roadmap,
                          synth_spec = synth_spec2)
    
})


postproc_f_null <- function(synth_id, synth_name, postsynth) {
  return(
    list(a = 1)
  )
}

postproc_f <- function(synth_id, synth_name, postsynth) {
  return(
    list(
      "file_name" = paste0("synths/", synth_name, ".R"),
      "synth_time" = postsynth$total_synthesis_time,
      "mean_mpg" = postsynth$synthetic_data$mpg %>% mean
    )
  )
}

test_that("Naive tune_synthesis works as expected", {
  
  #using default seed raises warning
  expect_warning(
    tune_synthesis(
      presynths = list(presynth1, presynth2),
      postprocessing_func = postproc_f_null
    )
  )

  ms <- tune_synthesis(
    presynths = list(presynth1, presynth2),
    postprocessing_func = postproc_f_null,
    seed = 12345
  )
  
  expect_identical(ms$metadata,
                   tibble::tibble(id = c(1L, 2L), 
                                  name = c("synth_0001", "synth_0002")))
  
  expect_identical(ms$results, 
                   list(list(a = 1), list(a = 1)))
  

  
})

test_that("tune_synthesis with names", {
  
  # tune synthesis with named list
  ms <- tune_synthesis(
    presynths = list("ps1" = presynth1, "ps2" = presynth2),
    postprocessing_func = postproc_f_null,
    seed = 12345
  )
  
  # named list names get transferred to output metadata
  expect_identical(ms$metadata, 
                   tibble::tibble(id = c(1L, 2L), 
                                  name = c("ps1", "ps2")))
  
})


test_that("tune_synthesis simplify_post works as expected", {
  
  ms <- tune_synthesis(
    presynths = list(presynth1, presynth2),
    postprocessing_func = postproc_f_null,
    simplify_post = TRUE,
    seed = 12345
  )
  
  expect_identical(ms$metadata,
                   tibble::tibble(id = c(1L, 2L), 
                                  name = c("synth_0001", "synth_0002")))
  
  expect_identical(ms$results,
                   tibble::tibble(id = c(1L, 2L), 
                                  name = c("synth_0001", "synth_0002"),
                                  a = c(1., 1.)))
  
})

test_that("Replicable seeds work as expected", {
  
  ms1 <- tune_synthesis(
    presynths = list(presynth1, presynth2),
    postprocessing_func = postproc_f,
    simplify_post = TRUE,
    seed = 12345
  )
  
  ms2 <- tune_synthesis(
    presynths = list(presynth1, presynth2),
    postprocessing_func = postproc_f,
    simplify_post = TRUE,
    seed = 12345
  )
  
  expect_identical(ms1$results %>% dplyr::select(mean_mpg), 
                   ms2$results %>% dplyr::select(mean_mpg))
  
})

test_that("Invalid inputs throw errors as expected", {

  expect_error(
    tune_synthesis(list("not_a_presynth"), 
                   postproc_f_null),
    regexp = "`presynths` elements must be presynth objects",
    fixed = TRUE
  )
  
  expect_error(
    tune_synthesis(list(presynth1), 
                   function(z) { }),
    regexp = "`postprocessing_func` must have required arguments: synth_id, synth_name, \n    and postsynth",
    fixed = TRUE
  )
  
  expect_error(
    tune_synthesis(list(presynth1), 
                   postprof_f_null, 
                   metadata_func = function(z) {}),
    regexp = "object 'postprof_f_null' not found",
    fixed = TRUE
  )
    
})

test_that("tune_synthesis metadata function", {
  
  mf <- function(presynth) {
    
    return(
      list(
        "model_engine" = presynth$synth_spec$default_regression_model$engine
      )
    )
    
  }
  
  ms1 <- tune_synthesis(
    presynths = list(presynth1, presynth2),
    postprocessing_func = postproc_f,
    metadata_func = mf,
    simplify_post = TRUE,
    seed = 12345
  )
  
  expected_result <- tibble::tibble(
    "id" = c(1L, 2L), 
    "name" = c("synth_0001", "synth_0002"),
    "model_engine" = c("rpart", "lm")
  )
  
  expect_identical(
    ms1$metadata, expected_result
  )
  
})
