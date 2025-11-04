acs_conf <- acs_conf |>
  tidyr::drop_na() 

acs_start <- acs_start |>
  tidyr::drop_na() 

# roadmaps
mtcars_roadmap <- roadmap(
  conf_data = mtcars,
  start_data = dplyr::select(mtcars, mpg, cyl, disp)
)

roadmap <- roadmap(
  conf_data = acs_conf,
  start_data = acs_start
)



lm_mod <- parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::set_mode(mode = "regression")

# create a decision tree for the categorical variables
rpart_mod <- parsnip::decision_tree() |>
  parsnip::set_engine("rpart") |>
  parsnip::set_mode(mode = "classification")

test_that("sample_lm() doesn't work with classification models", {
  
  classification_rec <- recipes::recipe(classwkr ~ ., data = acs_conf)
  
  model_class <- workflows::workflow() |>
    workflows::add_model(spec = rpart_mod) |>
    workflows::add_recipe(recipe = classification_rec) |>
    parsnip::fit(data = acs_conf)
  
  expect_error(
    sample_lm(model = model_class, 
              new_data = acs_conf[1:3, ], 
              conf_data = acs_conf),
    regexp = "sample_lm only works with regression models",
    fixed = TRUE
  )
  
})

test_that("sample_lm() works with regression", {
  
  regression_rec <- recipes::recipe(inctot ~ ., data = acs_conf)
  
  model_reg <- workflows::workflow() |>
    workflows::add_model(spec = lm_mod) |>
    workflows::add_recipe(recipe = regression_rec) |>
    parsnip::fit(data = acs_conf)
  
  set.seed(1)
  sample1 <- sample_lm(model = model_reg, 
                       new_data = acs_conf[1:3, ], 
                       conf_data = acs_conf)
  
  set.seed(1)
  sample2 <- sample_lm(model = model_reg, 
                       new_data = acs_conf[1:3, ], 
                       conf_data = acs_conf)
  
  set.seed(2)
  sample3 <- sample_lm(model = model_reg, 
                       new_data = acs_conf[1:3, ], 
                       conf_data = acs_conf)
  
  # y_hat reproduces with set.seed
  expect_identical(sample1, sample2)
  expect_false(all(sample1 == sample3))
  
})

test_that("synthesize() with sample_lm() reproduces with set.seed()", {
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = lm_mod,
    default_regression_sampler = sample_lm,
    default_classification_model = rpart_mod,
    default_classification_sampler = sample_rpart
  )
  
  # presynth
  expect_warning(
    presynth <- presynth(
      roadmap = mtcars_roadmap,
      synth_spec = synth_spec
    )
  )
  
  set.seed(20201019)
  synth1 <- synthesize(presynth)
  
  set.seed(20201019)
  synth2 <- synthesize(presynth)
  
  expect_true(is_postsynth(synth1))
  expect_true(is_postsynth(synth2))
  expect_equal(synth1$synthetic_data, synth2[["synthetic_data"]])
  
})

test_that("sample_lm() works with noise and constraints", {
    
  # build a constraints objects
  constraints_df_num <- 
    tibble::tribble(~var, ~min, ~max, ~conditions,
                    "gear", 0, Inf, "TRUE",
                    "gear", 0, 4, "vs ==1"
    )
  
  constraints <- constraints(
    schema = mtcars_roadmap$schema,
    constraints_df_num = constraints_df_num,
    max_z_num = 0
  )
  
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = lm_mod,
    default_regression_sampler = sample_lm,
    default_classification_model = rpart_mod,
    default_classification_sampler = sample_rpart,
    default_regression_noise = noise(
      add_noise = TRUE,
      noise_func = add_noise_kde,
      exclusions = 0,
      n_ntiles = 2
    ),
    default_classification_noise = noise(add_noise = FALSE)
  )
  
  # presynth
  expect_warning(
    presynth <- presynth(
      roadmap = mtcars_roadmap,
      synth_spec = synth_spec
    )
  )
  
  synth <- synthesize(presynth)
  
  expect_true(is_postsynth(synth))
  
})

# create a few objects that will be used by the final tests
acs_rec <- recipes::recipe(inctot ~ ., data = acs_conf)

# acs_rec <- construct_recipes(roadmap = roadmap)

# create model workflow
model_wf <- workflows::workflow() |>
  workflows::add_model(lm_mod) |>
  workflows::add_recipe(acs_rec) #acs_rec[["inctot"]])

test_that("Test sample_lm() with no variation in outcome", {
  
  # set all values to 10 so ldiversity is 1 and the regression tree is a root 
  # tree
  roadmap[["conf_data"]]$inctot <- 10
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf |>
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  inctot_hat <- sample_lm(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_equal(inctot_hat, rep(10, times = 777))
  
})

test_that("Test sample_lm()", {
  
  # randomly set the values to 10 and 20
  # the model should be bad
  roadmap[["conf_data"]]$inctot <- c(rep(10, times = 389), rep(20, times = 388))
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf |>
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  set.seed(20230919)
  inctot_hat <- sample_lm(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(dplyr::between(inctot_hat, -10, 30)))
  
})

test_that("Test sample_lm() with perfect model", {
  
  # create data that will generate a perfectly predictive model
  roadmap[["conf_data"]]$inctot <- ifelse(
    roadmap[["conf_data"]]$hcovany == "With health insurance coverage", 
    20000, 
    10000
  )
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf |>
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  inctot_hat <- sample_lm(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(round(inctot_hat) %in% c(10000, 20000)))
  
})
