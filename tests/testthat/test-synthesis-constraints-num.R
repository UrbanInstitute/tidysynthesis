data <- dplyr::select(mtcars, cyl, mpg, disp, vs) |>
  dplyr::mutate(
    cyl_factor = factor(cyl),
    vs = factor(vs, levels = c("0", "1", "2"))
  ) |>
  dplyr::mutate(cyl = dplyr::if_else(cyl == 6, NA, cyl))

start_data <- dplyr::select(data, cyl)

constraints_df_num <- tibble::tribble(
  ~var, ~min, ~max, ~conditions,
  "mpg", 0, Inf, "TRUE",
  "mpg", -Inf, 15, "cyl == 6",
  "mpg", -Inf, 12, "cyl == 8",
  "mpg", 2, 14, "cyl_factor == '6'",
  "mpg", 3, 11, "cyl_factor == '8'",
  "disp", 0, Inf, "TRUE",
  "disp", 1, Inf, "is.na(cyl)"
) 


roadmap1 <- roadmap(conf_data = data, 
                    start_data = start_data) |>
  add_sequence_manual(cyl_factor, mpg, disp, vs) |>
  update_constraints(constraints_df_num = constraints_df_num,
                     max_z_num = 0)

roadmap2 <- roadmap(conf_data = data, 
                    start_data = start_data) |>
  add_sequence_manual(cyl_factor, mpg, disp, vs) |>
  update_constraints(constraints_df_num = constraints_df_num,
                     max_z_num = 3)

step1 <- function(x) {
  
  x |>
    recipes::step_center(recipes::all_numeric_predictors()) |>
    recipes::step_impute_mean(cyl)
  
}

# noise
noise1 <- noise(
  add_noise = TRUE,
  noise_func = add_noise_kde,
  exclusions = c(0, 100),
  n_ntiles = 4
)

# algos
rpart_mod_cat <- parsnip::decision_tree() |> 
  parsnip::set_mode("classification") |>
  parsnip::set_engine("rpart")

rpart_mod_num <- parsnip::decision_tree() |>
  parsnip::set_mode("regression") |>
  parsnip::set_engine("rpart")


synth_spec <- synth_spec(
  default_regression_model = rpart_mod_num,
  default_classification_model = rpart_mod_cat,
  default_regression_steps = step1, 
  default_classification_steps = step1,
  default_regression_sampler = sample_rpart,
  default_classification_sampler = sample_rpart,
  default_regression_noise = noise1,
  default_classification_noise = noise()
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
  
  expect_type(synth1$synthetic_data$mpg, "double")
  expect_type(synth1$synthetic_data$disp, "double")
  expect_type(synth2$synthetic_data$mpg, "double")
  expect_type(synth2$synthetic_data$disp, "double")
  
})

test_that("synthesize() returns correct ldiversity ", {
  
  expect_equal(dim(synth1$ldiversity), c(32, 4))
  expect_equal(names(synth1$ldiversity), 
               roadmap1$visit_sequence$visit_sequence)
  
  expect_equal(dim(synth2$ldiversity), c(32, 4))
  expect_equal(names(synth2$ldiversity), 
               roadmap2$visit_sequence$visit_sequence)
  
})

test_that("constraints enforced properly", {
  
  for (s in list(synth1, synth2)) {
    
    expect_true(all(s$synthetic_data$mpg > 0))
    expect_true(all(s$synthetic_data |>
                      dplyr::filter(cyl == 8) |>
                      dplyr::pull(mpg) < 12))
    expect_true(all(s$synthetic_data |>
                      dplyr::filter(cyl == 6) |>
                      dplyr::pull(mpg) < 15))
    
  }
  
})
