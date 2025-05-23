data <- dplyr::select(mtcars, cyl, mpg, disp, vs, gear) %>%
  dplyr::mutate(
    vs = factor(vs, levels = c("0", "1", "2")),
    gear = factor(gear)
  ) 

start_data <- dplyr::select(data, cyl, mpg)

constraints_df_cat <-  tibble::tribble(
  ~var, ~allowed, ~forbidden, ~conditions,
  "gear", NA, "5", "TRUE",
  "gear", "3", NA, "cyl == 8"
)


roadmap1 <- roadmap(conf_data = data, 
                    start_data = start_data) %>%
  update_constraints(constraints_df_cat = constraints_df_cat,
                     max_z_cat = 2)

roadmap2 <- roadmap(conf_data = data, 
                    start_data = start_data) %>%
  update_constraints(constraints_df_cat = constraints_df_cat,
                     max_z_cat = 0)

rpart_mod_cat <- parsnip::decision_tree() %>% 
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("rpart")

rpart_mod_num <- parsnip::decision_tree() %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("rpart")

synth_spec <- synth_spec(
  default_regression_model = rpart_mod_num,
  default_classification_model = rpart_mod_cat,
  default_regression_sampler = sample_rpart,
  default_classification_sampler = sample_rpart
)

suppressWarnings({
  
  presynth1 <- presynth(
    roadmap = roadmap1,
    synth_spec = synth_spec
  )
  
  presynth2 <- presynth(
    roadmap = roadmap2,
    synth_spec = synth_spec
  )
  
  synth1 <- synthesize(presynth1)
  synth2 <- synthesize(presynth2)
  
})


test_that("synthesize() runs without error", {
  
  expect_true(is_postsynth(synth1))
  expect_equal(nrow(roadmap1$start_data), nrow(synth1$synthetic_data))
  expect_true(is_postsynth(synth2))
  expect_equal(nrow(roadmap2$start_data), nrow(synth2$synthetic_data))
  
})

test_that("synthesize() returns correct variable types ", {
  
  expect_true(is.factor(synth1$synthetic_data$vs))
  expect_true(is.factor(synth2$synthetic_data$vs))
  expect_true(is.factor(synth1$synthetic_data$gear))
  expect_true(is.factor(synth2$synthetic_data$gear))
  
})

test_that("synthesize() returns correct ldiversity ", {
  
  expect_equal(dim(synth1$ldiversity), c(32, 3))
  expect_equal(names(synth1$ldiversity), 
               roadmap1$visit_sequence$visit_sequence)
  
  expect_equal(dim(synth2$ldiversity), c(32, 3))
  expect_equal(names(synth2$ldiversity), 
               roadmap2$visit_sequence$visit_sequence)
  
})

test_that("constraints enforced properly", {
  
  for (s in list(synth1, synth2)) {
    
    expect_true(all(s$synthetic_data$gear != 5))
    expect_true(all(s$synthetic_data %>%
                      dplyr::filter(cyl == 8) %>%
                      dplyr::pull(gear) == "3"))
    
  }
  
})

