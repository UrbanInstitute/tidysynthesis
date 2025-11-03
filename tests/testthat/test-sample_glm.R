library(poissonreg)
library(stats)

acs_conf <- acs_conf |>
  tidyr::drop_na()

acs_start <- acs_start |>
  tidyr::drop_na()

# roadmap
roadmap <- roadmap(
  conf_data = acs_conf,
  start_data = acs_start
)

mtcars_roadmap <- roadmap(
  conf_data = dplyr::select(mtcars, mpg, cyl, disp, gear),
  start_data = dplyr::select(mtcars, mpg, cyl, disp)
)

poisson_mod <- parsnip::poisson_reg() |>
  parsnip::set_engine("glm") |>
  parsnip::set_mode(mode = "regression")

logistic_mod <- parsnip::logistic_reg() |>
  parsnip::set_engine("glm") |>
  parsnip::set_mode(mode = "classification")

rpart_mod <- parsnip::decision_tree() |>
  parsnip::set_engine("rpart") |>
  parsnip::set_mode(mode = "classification")

lm_mod <- parsnip::linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::set_mode(mode = "regression")

test_that("sample_glm() doesn't work with non-logistic classification models", {
  
  classification_rec <- recipes::recipe(classwkr ~ ., data = acs_conf)
  
  model_class <- workflows::workflow() |>
    workflows::add_model(spec = rpart_mod) |>
    workflows::add_recipe(recipe = classification_rec) |>
    parsnip::fit(data = acs_conf)
  
  expect_error(
    sample_glm(model = model_class, 
               new_data = acs_conf[1:3, ], 
               conf_data = acs_conf)
  )
  
})

test_that("sample_glm() doesn't work for non-poisson regression models", {
  
  regression_rec <- recipes::recipe(inctot ~ ., data = acs_conf)
  
  model_reg <- workflows::workflow() |>
    workflows::add_model(spec = lm_mod) |>
    workflows::add_recipe(recipe = regression_rec) |>
    parsnip::fit(data = acs_conf)
  
  expect_error(
    sample1 <- sample_glm(model = model_reg, 
                          new_data = acs_conf[1:3, ], 
                          conf_data = acs_conf)
  )
  
})

test_that("sample_glm() works with poisson regression", {
  
  regression_rec <- recipes::recipe(age ~ ., data = acs_conf)
  
  model_reg <- workflows::workflow() |>
    workflows::add_model(spec = poisson_mod) |>
    workflows::add_recipe(recipe = regression_rec) |>
    parsnip::fit(data = acs_conf)
  
  set.seed(1)
  sample1 <- sample_glm(model = model_reg, 
                        new_data = acs_conf[1:3, ], 
                        conf_data = acs_conf)
  
  set.seed(1)
  sample2 <- sample_glm(model = model_reg, 
                        new_data = acs_conf[1:3, ], 
                        conf_data = acs_conf)
  
  set.seed(2)
  sample3 <- sample_glm(model = model_reg, 
                        new_data = acs_conf[1:3, ], 
                        conf_data = acs_conf)
  
  # sample_glm from scratch
  set.seed(1)
  pred_i_raw <- stats::predict(model_reg$fit$fit$fit, 
                               newdata = acs_conf[1:3, ])
  pred_i <- exp(pred_i_raw)
  y_hat_scratch <- stats::rpois(n = nrow(acs_conf[1:3, ]),
                                lambda = pred_i)
  
  # y_hat reproduces with set.seed
  expect_identical(sample1, sample2)
  expect_false(all(sample1 == sample3))
  expect_identical(sample1, y_hat_scratch)
  
})

test_that("sample_glm() works with classification", {
  
  classification_rec <- recipes::recipe(hcovany ~ ., data = acs_conf)
  
  model_class <- workflows::workflow() |>
    workflows::add_model(spec = logistic_mod) |>
    workflows::add_recipe(recipe = classification_rec) |>
    parsnip::fit(data = acs_conf)
  
  set.seed(1)
  sample1 <- sample_glm(model = model_class, 
                        new_data = acs_conf[1:3, ], 
                        conf_data = acs_conf)
  
  set.seed(1)
  sample2 <- sample_glm(model = model_class, 
                        new_data = acs_conf[1:3, ], 
                        conf_data = acs_conf)
  
  set.seed(5)
  sample3 <- sample_glm(model = model_class, 
                        new_data = acs_conf[1:3, ], 
                        conf_data = acs_conf)
  
  set.seed(1)
  pred_i_raw <- stats::predict(model_class$fit$fit, 
                               new_data = acs_conf[1:3, ], 
                               type = "raw")
  pred_i <- exp(pred_i_raw) / (1 + exp(pred_i_raw))
  y_hat_scratch <- stats::rbinom(n = nrow(acs_conf[1:3, ]),
                                 size = 1, 
                                 prob = pred_i)
  y_hat_scratch <- ifelse(
    y_hat_scratch == 1, 
    yes = "With health insurance coverage", 
    no = "No health insurance coverage With health insurance coverage"
  )
  
  # y_hat reproduces with set.seed
  expect_identical(sample1, sample2)
  expect_identical(as.character(sample1), y_hat_scratch)
  expect_false(all(sample1 == sample3))
  
})

test_that("synthesize() with sample_glm() reproduces with set.seed()", {
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = poisson_mod,
    default_regression_sampler = sample_glm,
    default_classification_model = logistic_mod,
    default_classification_sampler = sample_glm
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

test_that("sample_glm() works with noise and constraints", {
  
  # build a constraints object
  constraints_df_num <- (
    tibble::tribble(~var, ~min, ~max, ~conditions,
                    "gear", 2, 6, "TRUE")
  )
  
  constraints <- constraints(
    schema = mtcars_roadmap$schema,
    constraints_df_num = constraints_df_num,
    max_z_num = 0
  )
  
  # noise
  noise <- noise(
    add_noise = TRUE,
    noise_func = add_noise_kde,
    exclusions = 0,
    n_ntiles = 2
  )
  
  # synth_spec
  synth_spec <- synth_spec(
    default_regression_model = poisson_mod,
    default_classification_model = logistic_mod,
    default_regression_sampler = sample_glm,
    default_classification_sampler = sample_glm
  )
  
  # presynth
  expect_warning(
    presynth <- presynth(
      roadmap = mtcars_roadmap |>
        add_constraints(constraints),
      synth_spec = synth_spec
    )
  )
  
  synth <- synthesize(presynth)
  expect_true(is_postsynth(synth))
  
})

# ---------------------------------------------------------------------------
# create a few objects that will be used by the final tests
acs_rec <- recipes::recipe(inctot ~ ., 
                           data = acs_conf)

# create model workflow
model_wf <- workflows::workflow() |>
  workflows::add_model(poisson_mod) |>
  workflows::add_recipe(acs_rec)

test_that("Test sample_glm() with no variation in outcome", {
  
  # set all values to 10
  roadmap[["conf_data"]]$inctot <- 10
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf |>
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  set.seed(10)
  inctot_hat <- sample_glm(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(inctot_hat %in% 1:24))
  
})

test_that("Test sample_glm()", {
  
  # randomly set the values to 10 and 20
  # the model should be bad
  roadmap[["conf_data"]]$inctot <- c(rep(10, times = 389), rep(20, times = 388))
  
  # fit the model with the edited confidential data
  fitted_model <- model_wf |>
    parsnip::fit(data = roadmap[["conf_data"]])
  
  # sample values
  # this sample_glm() needs a seed because the tree isn't root and isn't perfect
  set.seed(20230919)
  inctot_hat <- sample_glm(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(inctot_hat %in% 1:30))
  
})

test_that("Test sample_glm() with perfect model", {
  
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
  inctot_hat <- sample_glm(
    model = fitted_model, 
    new_data = dplyr::select(roadmap[["conf_data"]], -inctot), 
    conf_data = roadmap[["conf_data"]]
  )
  
  expect_true(all(inctot_hat %in% 8000:22000))
  
})

