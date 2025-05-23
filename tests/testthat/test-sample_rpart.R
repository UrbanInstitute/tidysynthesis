# roadmap

mtcars_roadmap <- roadmap(
  conf_data = mtcars,
  start_data = dplyr::select(mtcars, mpg, cyl, disp)
)

roadmap <- roadmap(
  conf_data = acs_conf,
  start_data = acs_start
)

rpart_mod_reg <- parsnip::decision_tree() %>%
  parsnip::set_engine("rpart") %>%
  parsnip::set_mode(mode = "regression")

rpart_mod_class <- parsnip::decision_tree() %>%
  parsnip::set_engine("rpart") %>%
  parsnip::set_mode(mode = "classification")

test_that("sample_rpart() works with regression", {

  regression_rec <- recipes::recipe(inctot ~ ., data = acs_conf)
  
  model_reg <- workflows::workflow() %>%
    workflows::add_model(spec = rpart_mod_reg) %>%
    workflows::add_recipe(recipe = regression_rec) %>%
    parsnip::fit(data = acs_conf)
  
  set.seed(1)
  sample1 <- sample_rpart(model = model_reg, new_data = acs_conf[1:3, ], conf_data = acs_conf)
  
  set.seed(1)
  sample2 <- sample_rpart(model = model_reg, new_data = acs_conf[1:3, ], conf_data = acs_conf)
  
  set.seed(2)
  sample3 <- sample_rpart(model = model_reg, new_data = acs_conf[1:3, ], conf_data = acs_conf)

  # ldiversity is accurate and unchanging
  expect_equal(sample1[["ldiversity"]], c(62, 117, 62))
  expect_equal(sample2[["ldiversity"]], c(62, 117, 62))
  expect_equal(sample3[["ldiversity"]], c(62, 117, 62))
  
  # y_hat reproduces with set.seed
  expect_identical(sample1, sample2)
  
})

test_that("sample_rpart() works with classification", {

  classification_rec <- recipes::recipe(hcovany ~ ., data = acs_conf)
  
  model_reg <- workflows::workflow() %>%
    workflows::add_model(spec = rpart_mod_class) %>%
    workflows::add_recipe(recipe = classification_rec) %>%
    parsnip::fit(data = acs_conf)
  
  set.seed(1)
  sample1 <- sample_rpart(model = model_reg, 
                          new_data = acs_conf[1:10, ], 
                          conf_data = acs_conf)
  
  set.seed(1)
  sample2 <- sample_rpart(model = model_reg, 
                          new_data = acs_conf[1:10, ], 
                          conf_data = acs_conf)
  
  set.seed(5)
  sample3 <- sample_rpart(model = model_reg, 
                          new_data = acs_conf[1:10, ], 
                          conf_data = acs_conf)
  
  # ldiversity is accurate and unchanging
  expect_equal(sample1[["ldiversity"]], rep(2, 10))
  expect_equal(sample2[["ldiversity"]], rep(2, 10))
  expect_equal(sample3[["ldiversity"]], rep(2, 10))
  
  # y_hat reproduces with set.seed
  expect_identical(sample1, sample2)
  
  expect_false(all(sample1[["y_hat"]] == sample3[["y_hat"]]))
  
})

test_that("synthesize() with sample_rpart() reproduces with set.seed()", {
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = rpart_mod_reg,
    default_classification_model = rpart_mod_class,
    default_regression_sampler = sample_rpart,
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
  expect_true(rlang::is_vector(synth2[["ldiversity"]]))
  expect_equal(synth1$synthetic_data, synth2[["synthetic_data"]])
  expect_equal(synth1$ldiversity, synth2[["ldiversity"]])
  
})

test_that("sample_rpart() works with rpart::LAD", {
  
  # rpart model
  rpart_lad <- parsnip::decision_tree() %>% 
    parsnip::set_engine(engine = "rpart") %>%
    parsnip::set_mode(mode = "regression") %>%
    parsnip::set_args(method = rpart.LAD::LAD)
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = rpart_mod_reg,
    default_classification_model = rpart_mod_class,
    default_regression_sampler = sample_rpart,
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
  expect_true(rlang::is_vector(synth2[["ldiversity"]]))
  expect_equal(synth1[["synthetic_data"]], synth2[["synthetic_data"]])
  expect_equal(synth1[["ldiversity"]], synth2[["ldiversity"]])
  
})

test_that("sample_rpart() works with noise and constraints", {
  
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
    default_regression_model = rpart_mod_reg,
    default_classification_model = rpart_mod_class,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_rpart,
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
      roadmap = mtcars_roadmap %>%
        add_constraints(constraints),
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
model_wf <- workflows::workflow() %>%
  workflows::add_model(rpart_mod_reg) %>%
  workflows::add_recipe(acs_rec) #acs_rec[["inctot"]])

test_that("Test sample_rpart() with no variation in outcome", {
  
  # set all values to 10 so ldiversity is 1 and the regression tree is a root 
  # tree
  roadmap[["conf_data"]]$inctot <- 10
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf %>%
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  inctot_hat <- sample_rpart(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_equal(inctot_hat[["y_hat"]], rep(10, times = 1500))
  expect_equal(inctot_hat[["ldiversity"]], rep(1, times = 1500))
  
})

test_that("Test sample_rpart() with no variation in outcome and all equal 0", {
  
  # set all values to 0 so ldiversity calculator returns NA by default
  roadmap[["conf_data"]]$inctot <- 0
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf %>%
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  inctot_hat <- sample_rpart(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )

  expect_true(all(is.na(inctot_hat[["ldiversity"]])))
  
})

test_that("Test sample_rpart()", {
  
  # randomly set the values to 10 and 20
  # the model should be bad
  roadmap[["conf_data"]]$inctot <- rep(c(10, 20), times = 750)
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf %>%
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  # this sample_rpart() needs a seed because the tree isn't root and isn't perfect
  set.seed(20230919)
  inctot_hat <- sample_rpart(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(inctot_hat[["y_hat"]] %in% c(10, 20)))
  expect_equal(inctot_hat[["ldiversity"]], rep(2, times = 1500))

})

test_that("Test sample_rpart() with perfect model", {
  
  # create data that will generate a perfectly predictive model
  roadmap[["conf_data"]]$inctot <- ifelse(
    roadmap[["conf_data"]]$hcovany == "With health insurance coverage", 
    20000, 
    10000
  )
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf %>%
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  inctot_hat <- sample_rpart(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(inctot_hat[["y_hat"]] %in% c(10000, 20000)))
  expect_equal(inctot_hat[["ldiversity"]], rep(1, times = 1500))
  
})
