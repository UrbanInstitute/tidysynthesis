# data setup - dropping mismatched factor levels in start_data after drop_na
acs_conf <- acs_conf |>
  dplyr::select(-gq) |>
  tidyr::drop_na()

acs_start <- acs_start |>
  dplyr::select(-gq) |>
  tidyr::drop_na()

# roadmap
roadmap <- roadmap(
  conf_data = acs_conf,
  start_data = acs_start
)

rf_mod_regression <- parsnip::rand_forest(trees = 500, min_n = 1) |>
  parsnip::set_engine(engine = "ranger") |>
  parsnip::set_mode(mode = "regression") |>
  parsnip::set_args(quantreg = TRUE)

rf_mod_classification <- parsnip::rand_forest(trees = 500, min_n = 1) |>
  parsnip::set_engine(engine = "ranger") |>
  parsnip::set_mode(mode = "classification")

test_that("sample_ranger() works with regression", {
  
  regression_rec <- recipes::recipe(age ~ ., data = acs_conf)

  model_reg <- workflows::workflow() |>
    workflows::add_model(spec = rf_mod_regression) |>
    workflows::add_recipe(recipe = regression_rec) |>
    parsnip::fit(data = acs_conf)
  
  set.seed(1)
  sample1 <- sample_ranger(model = model_reg, 
                           new_data = acs_conf[1:3, ], 
                           conf_data = acs_conf)
  
  set.seed(1)
  sample2 <- sample_ranger(model = model_reg, 
                           new_data = acs_conf[1:3, ], 
                           conf_data = acs_conf)
  
  set.seed(2)
  sample3 <- sample_ranger(model = model_reg, 
                           new_data = acs_conf[1:3, ], 
                           conf_data = acs_conf)
  
  # y_hat reproduces with set.seed
  expect_identical(sample1, sample2)
  
})

test_that("sample_ranger() works with classification", {
  
  classification_rec <- recipes::recipe(hcovany ~ ., data = acs_conf)

  model_reg <- workflows::workflow() |>
    workflows::add_model(spec = rf_mod_classification) |>
    workflows::add_recipe(recipe = classification_rec) |>
    parsnip::fit(data = acs_conf)
  
  set.seed(1)
  sample1 <- sample_ranger(model = model_reg, 
                           new_data = acs_conf[1:3, ], 
                           conf_data = acs_conf)
  
  set.seed(1)
  sample2 <- sample_ranger(model = model_reg, 
                           new_data = acs_conf[1:3, ], 
                           conf_data = acs_conf)
  
  set.seed(2)
  sample3 <- sample_ranger(model = model_reg, 
                           new_data = acs_conf[1:3, ], 
                           conf_data = acs_conf)
  
  # y_hat reproduces with set.seed
  expect_identical(sample1, sample2)
  
})

test_that("synthesize() with sample_ranger() reproduces with set.seed()", {
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = rf_mod_regression,
    default_classification_model = rf_mod_classification,
    default_regression_sampler = sample_ranger,
    default_classification_sampler = sample_ranger
  )
  
  # presynth
  expect_warning(
    presynth <- presynth(
      roadmap = roadmap,
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
  
  # ensure the synthetic values are in the range of the data
  expect_true(all(dplyr::between(synth1$synthetic_data$age, 
                                 left = min(acs_conf$age), 
                                 right = max(acs_conf$age))))
  expect_true(all(dplyr::between(synth1$synthetic_data$famsize, 
                                 left = min(acs_conf$famsize), 
                                 right = max(acs_conf$famsize))))
  expect_true(all(dplyr::between(synth1$synthetic_data$transit_time, 
                                 left = min(acs_conf$transit_time), 
                                 right = max(acs_conf$transit_time))))
  expect_true(all(dplyr::between(synth1$synthetic_data$inctot, 
                                 left = min(acs_conf$inctot), 
                                 right = max(acs_conf$inctot))))
  expect_true(all(dplyr::between(synth1$synthetic_data$wgt, 
                                 left = min(acs_conf$wgt), 
                                 right = max(acs_conf$wgt))))
  
})

test_that("sample_ranger() works with noise and constraints", {
  
  # build a constraints object
  schema <- schema(conf_data = acs_conf, start_data = acs_start)
  
  constraints_df_num <- 
    tibble::tribble(~var, ~min, ~max, ~conditions,
                    "age", 0, Inf, "TRUE",
                    "age", 40, Inf, "famsize == 1"
    )
  
  constraints <- constraints(
    schema = schema,
    constraints_df_num = constraints_df_num,
    max_z_num = 0
  )
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = rf_mod_regression,
    default_classification_model = rf_mod_classification,
    default_regression_sampler = sample_ranger,
    default_classification_sampler = sample_ranger,
    default_regression_noise = noise(
      add_noise = TRUE,
      noise_func = add_noise_kde,
      exclusions = 0,
      n_ntiles = 2
    ),
    default_classification_noise = noise()
  )
  
  # presynth
  expect_warning(
    presynth <- presynth(
      roadmap = roadmap,
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
  workflows::add_model(spec = rf_mod_regression) |>
  workflows::add_recipe(recipe = acs_rec) #acs_rec[["inctot"]])

test_that("Test sample_ranger() with no variation in outcome", {
  
  # set all values to 10 the regression tree is a root tree
  roadmap[["conf_data"]]$inctot <- 10
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf |>
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  inctot_hat <- sample_ranger(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_equal(inctot_hat, rep(10, times = 777))
  
})

test_that("Test sample_ranger()", {
  
  # randomly set the values to 10 and 20
  # the model should be bad
  roadmap[["conf_data"]]$inctot <- c(rep(10, times = 389), rep(20, times = 388))
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf |>
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  # this sample_ranger() needs a seed because the tree isn't root and isn't perfect
  set.seed(20230919)
  inctot_hat <- sample_ranger(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(dplyr::between(inctot_hat, 10, 20)))
  
})

test_that("Test sample_ranger() with perfect model", {
  
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
  inctot_hat <- sample_ranger(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(dplyr::between(inctot_hat, 10000, 20000)))
  
})
