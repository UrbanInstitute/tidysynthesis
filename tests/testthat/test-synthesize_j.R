rpart_mod <- parsnip::decision_tree() |> 
  parsnip::set_engine(engine = "rpart") |>
  parsnip::set_mode(mode = "regression")

default_noise <- noise()

test_that("synthesize_j() returns correct outputs ", {
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  jth_synth <- synthesize_j(conf_data = mtcars,
                            synth_data = dplyr::select(mtcars, cyl, disp, hp),
                            col_schema = list(dtype = "dbl", na_prop = 0),
                            recipe = mtcars_rec,
                            sampler = sample_rpart,
                            noise = default_noise,
                            tuner = NULL,
                            extractor = NULL,
                            constraints = NULL,
                            model = rpart_mod,
                            invert_transformations = TRUE)
  
  expect_equal(jth_synth$extraction, NA)
  expect_equal(dim(jth_synth$predictions), c(32, 1))
  expect_type(jth_synth$jth_synthesis_time, "double")
  expect_type(jth_synth$ldiversity, "integer")
  expect_equal(length(jth_synth$ldiversity), 32)
  
})

test_that("synthesize_j() throws error with no sampler specified ", {
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  expect_error(
    synthesize_j(conf_data = mtcars,
                 synth_data = dplyr::select(mtcars, cyl, disp, hp),
                 col_schema = list(dtype = "dbl", na_prop = 0),
                 recipe = mtcars_rec,
                 sampler = NULL,
                 noise = default_noise,
                 tuner = NULL,
                 extractor = NULL,
                 constraints = NULL,
                 model = rpart_mod,
                 invert_transformations = TRUE),
    regexp = "Missing sampler object in generate_predictions().",
    fixed = TRUE
  )
  
})

test_that("synthesize_j() returns the correct prediction ", {
  
  rpart_mod <- parsnip::decision_tree() |> 
    parsnip::set_engine(engine = "rpart") |>
    parsnip::set_mode(mode = "regression")
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  new_data <- tibble::tibble(
    cyl = 6,
    disp = 160,
    hp = 110
  )
  
  jth_synth <- synthesize_j(conf_data = dplyr::slice_head(mtcars, n = 1),
                            synth_data = new_data,
                            col_schema = list(dtype = "dbl", na_prop = 0),
                            recipe = mtcars_rec,
                            sampler = sample_rpart,
                            tuner = NULL,
                            extractor = "workflow",
                            noise = default_noise,
                            constraints = NULL,
                            model = rpart_mod,
                            invert_transformations = TRUE)
  
  expect_s3_class(jth_synth$extraction, "workflow")
  expect_equal(jth_synth$predictions$mpg, 21)
  expect_type(jth_synth$jth_synthesis_time, "double")
  expect_type(jth_synth$ldiversity, "integer")
  expect_equal(length(jth_synth$ldiversity), 1)
  
})

test_that("synthesize_j() returns the correct prediction with constraints ", {
  
  roadmap <- roadmap(
    conf_data = dplyr::slice_head(mtcars, n = 1),
    start_data = dplyr::select(dplyr::slice_head(mtcars, n = 1), 
                               cyl, disp, hp)
  ) |>
    add_sequence_manual(mpg)
  
  rpart_mod <- parsnip::decision_tree() |> 
    parsnip::set_engine(engine = "rpart") |>
    parsnip::set_mode(mode = "regression")
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  constraints_df_num <- tibble::tribble(
    ~var, ~min, ~max, ~conditions,
    "mpg", 0, 20, "TRUE",
  ) 
  
  # don't impose constraints
  constraints <- constraints(schema = roadmap[["schema"]], 
                             constraints_df_num = constraints_df_num,
                             max_z_num = list(0))
  
  new_data <- tibble::tibble(
    cyl = 6,
    disp = 160,
    hp = 110
  )
  
  jth_synth <- synthesize_j(conf_data = dplyr::slice_head(mtcars, n = 1),
                            synth_data = new_data,
                            col_schema = list(dtype = "dbl", na_prop = 0),
                            recipe = mtcars_rec,
                            sampler = sample_rpart,
                            tuner = NULL,
                            noise = default_noise,
                            extractor = workflows::extract_recipe,
                            constraints = list(
                              "constraints_df" = constraints$constraints_num[["mpg"]],
                              "max_z" = 0
                            ),
                            model = rpart_mod,
                            invert_transformations = TRUE)
  
  expect_s3_class(jth_synth$extraction, "recipe")
  expect_equal(jth_synth$predictions$mpg, 20)
  expect_type(jth_synth$jth_synthesis_time, "double")
  expect_type(jth_synth$ldiversity, "integer")
  expect_equal(length(jth_synth$ldiversity), 1)
  
})

test_that(".fit_j() returns a fitted workflow and extraction ", {
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  fit <- .fit_j(conf_data = mtcars,
                col_schema = list(dtype = "dbl", na_prop = 0),
                model = rpart_mod,
                recipe = mtcars_rec,
                tuner = NULL,
                extractor = "workflow")
  
  expect_s3_class(fit$fitted, "workflow")
  expect_true(workflows::is_trained_workflow(fit$fitted))
  expect_s3_class(fit$extraction, "workflow")
  expect_type(fit$jth_fit_time, "double")
  
  fit_null <- .fit_j(conf_data = mtcars,
                     col_schema = list(dtype = "dbl", na_prop = 0),
                     model = rpart_mod,
                     recipe = mtcars_rec,
                     tuner = NULL,
                     extractor = NULL)
  
  expect_equal(fit_null$extraction, NA)
  
})

test_that(".fit_j() fits missing numeric outcomes on complete rows only ", {
  
  conf <- mtcars
  conf$mpg[1:4] <- NA
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = conf)
  
  fit <- .fit_j(conf_data = conf,
                col_schema = list(dtype = "dbl", na_prop = 4 / 32),
                model = rpart_mod,
                recipe = mtcars_rec,
                tuner = NULL,
                extractor = NULL)
  
  expect_true(workflows::is_trained_workflow(fit$fitted))
  
})

test_that(".fit_j() and .sample_j() take the identity shortcut ", {
  
  identity_mod <- rpart_mod
  identity_mod$mode <- "identity"
  
  conf <- dplyr::slice_head(mtcars, n = 1)
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = conf)
  
  fit <- .fit_j(conf_data = conf,
                col_schema = list(dtype = "dbl", na_prop = 0),
                model = identity_mod,
                recipe = mtcars_rec,
                tuner = NULL,
                extractor = NULL)
  
  expect_equal(fit$fitted, "identity")
  expect_equal(fit$extraction, "identity")
  
  sampled <- .sample_j(fitted = fit$fitted,
                       conf_data = conf,
                       synth_data = dplyr::select(mtcars, cyl, disp, hp),
                       col_schema = list(dtype = "dbl", na_prop = 0),
                       recipe = mtcars_rec,
                       sampler = sample_rpart,
                       noise = default_noise,
                       constraints = NULL,
                       invert_transformations = TRUE)
  
  expect_equal(sampled$predictions$mpg, rep(conf$mpg, 32))
  expect_equal(sampled$ldiversity, 1)
  
})

test_that(".sample_j() samples from a fitted workflow with constraints ", {
  
  roadmap <- roadmap(
    conf_data = dplyr::slice_head(mtcars, n = 1),
    start_data = dplyr::select(dplyr::slice_head(mtcars, n = 1), 
                               cyl, disp, hp)
  ) |>
    add_sequence_manual(mpg)
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  constraints_df_num <- tibble::tribble(
    ~var, ~min, ~max, ~conditions,
    "mpg", 0, 20, "TRUE",
  ) 
  
  constraints <- constraints(schema = roadmap[["schema"]], 
                             constraints_df_num = constraints_df_num,
                             max_z_num = list(0))
  
  conf <- dplyr::slice_head(mtcars, n = 1)
  col_schema <- list(dtype = "dbl", na_prop = 0)
  
  fit <- .fit_j(conf_data = conf,
                col_schema = col_schema,
                model = rpart_mod,
                recipe = mtcars_rec,
                tuner = NULL,
                extractor = NULL)
  
  sampled <- .sample_j(
    fitted = fit$fitted,
    conf_data = conf,
    synth_data = tibble::tibble(cyl = 6, disp = 160, hp = 110),
    col_schema = col_schema,
    recipe = mtcars_rec,
    sampler = sample_rpart,
    noise = default_noise,
    constraints = list(
      "constraints_df" = constraints$constraints_num[["mpg"]],
      "max_z" = 0
    ),
    invert_transformations = TRUE
  )
  
  expect_equal(sampled$predictions$mpg, 20)
  expect_type(sampled$jth_sample_time, "double")
  expect_equal(length(sampled$ldiversity), 1)
  
})

test_that(".fit_j() tunes hyperparameters when a tuner is supplied ", {
  
  tune_mod <- parsnip::decision_tree(cost_complexity = tune::tune()) |> 
    parsnip::set_engine(engine = "rpart") |>
    parsnip::set_mode(mode = "regression")
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  tuner <- list(
    v = 3,
    grid = tibble::tibble(cost_complexity = c(0.001, 0.01)),
    metrics = yardstick::metric_set(yardstick::rmse)
  )
  
  set.seed(1)
  suppressWarnings(
    fit <- .fit_j(conf_data = mtcars,
                  col_schema = list(dtype = "dbl", na_prop = 0),
                  model = tune_mod,
                  recipe = mtcars_rec,
                  tuner = tuner,
                  extractor = NULL)
  )
  
  expect_true(workflows::is_trained_workflow(fit$fitted))
  
})
