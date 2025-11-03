penguins_complete <- palmerpenguins::penguins |>
  tidyr::drop_na() |>
  dplyr::mutate(
    flipper_length_mm = as.numeric(flipper_length_mm),
    body_mass_g = as.numeric(body_mass_g)
    ) |>
  dplyr::select(-year)

set.seed(1)

starting_data <- penguins_complete |> 
  dplyr::select(sex) |>
  dplyr::slice_sample(n = nrow(penguins_complete), replace = TRUE)


roadmap <- roadmap(
  conf_data = penguins_complete,
  start_data = starting_data
  ) |>
  add_sequence_manual(
    species, #START WITH FACTOR VARIABLE; SHOULD NOT THROW ERROR
    bill_length_mm, bill_depth_mm, flipper_length_mm, 
    body_mass_g, island
  )


rpart_mod_cat <- parsnip::decision_tree() |> 
  parsnip::set_mode("classification") |>
  parsnip::set_engine("rpart")

rpart_mod_num <- parsnip::decision_tree() |>
  parsnip::set_mode("regression") |>
  parsnip::set_engine("rpart")

numeric_vars <- c("flipper_length_mm", 
                  "bill_length_mm", 
                  "bill_depth_mm", 
                  "body_mass_g")

noise1 <- noise(
  add_noise = TRUE,
  noise_func = add_noise_kde,
  noise_params = list(
    exclusions = 0,
    obs_per_ntile = 100
  )
)

noise_too_big <- noise(
  add_noise = TRUE,
  noise_func = add_noise_kde,
  noise_params = list(
    exclusions = 0,
    obs_per_ntile = nrow(starting_data) + 1 # SHOULD RESULT IN ERROR
  ) 
)

synth_spec <- synth_spec(
  default_classification_model = rpart_mod_cat,
  default_regression_model = rpart_mod_num,
  default_classification_sampler = sample_rpart,
  default_regression_sampler = sample_rpart
)

set.seed(321)

# test -----------------------------------------------------------------
test_that("Synthesis with factor starting variable", {
  
  expect_warning(
    presynth1 <- presynth(
      roadmap = roadmap,
      synth_spec = synth_spec
    )
  )
  
  expect_no_error(synth1 <- synthesize(presynth1))
  
})

test_that("failure when obs_per_ntile > start_data", {
  
  expect_warning(
    presynth_noise_too_big <- presynth(
      roadmap = roadmap,
      synth_spec = synth_spec |>
        update_synth_spec(
          default_regression_noise = noise_too_big
        )
    )
  )
  
  expect_error(synth2 <- synthesize(presynth_noise_too_big))
  
})
