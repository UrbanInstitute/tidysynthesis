test_that("postsynth() creates a basic presynth object", {
  
  # presynth
  postsynth <- postsynth(synthetic_data = NULL,
                         jth_preprocessing = NULL, 
                         total_synthesis_time = NULL, 
                         jth_synthesis_time = NULL,
                         extractions = NULL,
                         ldiversity = NULL)
  
  expect_true(is_postsynth(postsynth))
  expect_s3_class(postsynth, "postsynth")
  
})

test_that("print.postsynth", {
  
  ps <- postsynth(synthetic_data = acs_conf,
                  jth_preprocessing = NULL, 
                  total_synthesis_time = 120, 
                  jth_synthesis_time = NULL,
                  extractions = NULL,
                  ldiversity = NULL)
  
  expect_output(
    print(ps), "Synthetic Data: 1500 synthetic observations, 12 variables"
  )
  
  expect_output(
    print(ps), "Total Synthesis Time: 120 seconds"
  )
  
  
})
