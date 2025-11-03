default_reps <- replicates()

test_reps <- replicates(
  start_data_replicates = 1, 
  model_sample_replicates = 2, 
  end_to_end_replicates = 3
)

# Constructor tests ----------------------------------------------------------

test_that("default replicates object behavior", {
  
  # expect correct class definition 
  expect_s3_class(default_reps, "replicates") 
  
  # expect correct default values
  expect_equal(default_reps[["start_data_replicates"]], 1)
  expect_equal(default_reps[["model_sample_replicates"]], 1)
  expect_equal(default_reps[["end_to_end_replicates"]], 1)
  expect_equal(default_reps[["total_replicates"]], 1)
  
})

test_that("replicate total accounting", {
  
  # expect total replicates calculates product correctly
  expect_equal(test_reps[["total_replicates"]], 6)
  
})

test_that("replicates input type checking", {
  
  # expect error for incorrect types
  expect_error(
    replicates(start_data_replicates = FALSE), 
    regexp = "Argument `start_data_replicates` is not numeric.",
    fixed = TRUE
  )
  expect_error(
    replicates(start_data_replicates = 1.5), 
    regexp = "Argument `start_data_replicates` must be integer-valued.",
    fixed = TRUE
  )
  expect_error(
    replicates(start_data_replicates = -1), 
    regexp = "Argument `start_data_replicates` must be >= 1.",
    fixed = TRUE
  )
  expect_error(
    replicates(start_data_replicates = c(1, 2, 3)), 
    regexp = "Argument `start_data_replicates` must be length 1.",
    fixed = TRUE
  )
  
  expect_error(
    replicates(model_sample_replicates = FALSE), 
    regexp = "Argument `model_sample_replicates` is not numeric.",
    fixed = TRUE
  )
  expect_error(
    replicates(model_sample_replicates = 1.5), 
    regexp = "Argument `model_sample_replicates` must be integer-valued.",
    fixed = TRUE
  )
  expect_error(
    replicates(model_sample_replicates = -1), 
    regexp = "Argument `model_sample_replicates` must be >= 1.",
    fixed = TRUE
  )
  expect_error(
    replicates(model_sample_replicates = c(1, 2, 3)), 
    regexp = "Argument `model_sample_replicates` must be length 1.",
    fixed = TRUE
  )
  
})

test_that("replicates print function", {
  
  print_output <- utils::capture.output(print(test_reps))
  
  expect_identical(
    print_output, 
    c("Replicates", 
      "", 
      "Start Data Replicates:  1 ", 
      "Model Sample Replicates:  2 ", 
      "End-to-End Replicates:  3 ",
      "",
      "Total Replicates:  6 ")
  )
  
})

# Tidy API tests ------------------------------------------------------------

test_that("add_replicates functionality", {
  
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start)
  new_roadmap <- old_roadmap %>% add_replicates(test_reps)
  
  # expect new object is roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new_roadmap has right replicates object
  expect_identical(new_roadmap[["replicates"]], test_reps)
  
})

test_that("update_replicates functionality", {
  
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start)
  new_roadmap <- old_roadmap %>%
    update_replicates(model_sample_replicates = 5,
                      end_to_end_replicates = 7)
  
  # expect new object is roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new_roadmap has updated replicates values
  expect_equal(new_roadmap[["replicates"]][["model_sample_replicates"]], 5)
  expect_equal(new_roadmap[["replicates"]][["end_to_end_replicates"]], 7)
  
  # expect total_replicates updates
  expect_equal(new_roadmap[["replicates"]][["total_replicates"]], 35)
  
})

test_that("reset_replicates functionality", {
  
  old_roadmap <- roadmap(conf_data = acs_conf, start_data = acs_start,
                         replicates = test_reps)
  
  new_roadmap <- old_roadmap %>% reset_replicates()
  
  # expect new object is roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new object has default replicate settings
  expect_identical(new_roadmap[["replicates"]], default_reps)
  
  
})

test_that("validate_replicates() catches manual overrides", {
  
  # Create basic roadmap
  data <- dplyr::select(mtcars, cyl, mpg, disp, hp, gear)
  
  test_reps <- replicates(start_data_replicates = 1, 
                          model_sample_replicates = 2, 
                          end_to_end_replicates = 3)
  
  roadmap <- roadmap(conf_data = data,
                     start_data = dplyr::select(data, cyl, gear)) |> 
    add_sequence_numeric(everything(), method = "correlation", cor_var = "mpg") |>
    add_replicates(test_reps)
  
  # Manually override start data replicates
  roadmap$replicates$start_data_replicates <- 2
  
  dt_mod <- parsnip::decision_tree() %>%
    parsnip::set_engine("rpart") %>%
    parsnip::set_mode("regression")
  
  synth_spec <- synth_spec(default_regression_model = dt_mod,
                           default_regression_sampler = sample_rpart)
  
  # Validator inside presynth() should catch error due to manual overriding
  expect_error(
    expect_warning(
      presynth(roadmap = roadmap,
               synth_spec = synth_spec)
    )
  )
  
})
