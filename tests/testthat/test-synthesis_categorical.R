data <- dplyr::select(mtcars, cyl, mpg, disp, vs) %>%
  dplyr::mutate(vs = factor(vs, levels = c("0", "1", "2")))

set.seed(20231218)
start_data <- dplyr::select(data, cyl) %>%
  dplyr::slice_sample(n = 20)

# roadmap
roadmap <- roadmap(
  conf_data = data,
  start_data = start_data
) %>%
  add_sequence_manual(mpg, disp, vs)

# synth_spec
step1 <- function(x) {
  recipes::step_pca(x, recipes::all_predictors())
}

step2 <- function(x) {
  recipes::step_center(x, recipes::all_numeric(), -recipes::all_outcomes())
}

rpart_mod_cat <- parsnip::decision_tree() %>% 
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("rpart")

rpart_mod_num <- parsnip::decision_tree() %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("rpart")


synth_spec <- synth_spec(
  default_regression_model = rpart_mod_num,
  default_classification_model = rpart_mod_cat,
  custom_models = list(
    list(vars = c("vs"), 
         model = rpart_mod_cat)
  ),
  default_regression_steps = step1, 
  default_classification_steps = step1, 
  custom_steps = list(
    list(vars = c("vs"), 
         steps = step2)
  ),
  default_regression_sampler = sample_rpart,
  default_classification_sampler = sample_rpart,
  default_regression_noise = noise(
    add_noise = TRUE,
    noise_func = add_noise_kde,
    exclusions = c(0, 100),
    n_ntiles = 4
  ),
  default_classification_noise = noise(add_noise = FALSE)
)


# constraints
constraints_df_num <- 
  tibble::tribble(
    ~var, ~min, ~max, ~conditions,
    "mpg", 0, Inf, "TRUE",
    "mpg", -Inf, 15, "cyl == 6",
    "mpg", -Inf, 12, "cyl == 8",
    "disp", 0, Inf, "TRUE"
  ) 

constraints <- constraints(
  schema = roadmap$schema,
  constraints_df_num = constraints_df_num,
  max_z_num = 0
)

test_that("synthesize() runs without error", {
  
  expect_warning(
    presynth <- presynth(
      roadmap = roadmap %>%
        add_constraints(constraints),
      synth_spec = synth_spec
    )
  )
  
  synth <- synthesize(presynth)
  
  expect_true(is_postsynth(synth))
  expect_equal(nrow(roadmap$start_data), nrow(synth$synthetic_data))

  expect_type(synth$synthetic_data$mpg, "double")
  expect_type(synth$synthetic_data$disp, "double")
  expect_true(is.factor(synth$synthetic_data$vs))
  
  expect_equal(dim(synth$ldiversity), c(20, 3))
  expect_equal(names(synth$ldiversity), 
               presynth$roadmap$visit_sequence$visit_sequence)
  
})

test_that("identity method works with factor variable ", {
  
  data_no_var <- dplyr::select(mtcars, cyl, mpg, disp, vs) %>%
    dplyr::mutate(vs = "0") %>%
    dplyr::mutate(vs = as.factor(vs))
  
  roadmap_no_var <- roadmap(
    conf_data = data_no_var,
    start_data = start_data) %>%
    add_sequence_manual(mpg, disp, vs)
  
  synth_spec <- synth_spec(
    default_regression_model = rpart_mod_num,
    default_classification_model = rpart_mod_cat,
    custom_models = list(
      list(vars = c("vs"), 
           model = rpart_mod_cat)
    ),
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart
  )
  
  # presynth
  expect_warning(
    presynth_no_var <- presynth(
      roadmap = roadmap_no_var,
      synth_spec = synth_spec
    )
  )
  
  synth_no_var <- synthesize(presynth_no_var)
  
  expect_equal(synth_no_var$synthetic_data$vs, 
               factor(rep("0", nrow(start_data))))
  
})
