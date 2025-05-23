data <- dplyr::select(mtcars, cyl, mpg, disp, hp)

# algos
rpart_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine(engine = "rpart") %>%
  parsnip::set_mode(mode = "regression")

new_data <- tibble::tibble(
  cyl = 6,
  hp = 110
)

test_that("garnish can handle no outcome variable transformations", {
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + hp, data = data)
  
  # create a workflow with the model and recipe
  model_wf <- workflows::workflow() %>%
    workflows::add_model(rpart_mod) %>%
    workflows::add_recipe(mtcars_rec)
  
  # estimate the specified model using the confidential data
  estimated_model <- model_wf %>%
    parsnip::fit(data = data)
  
  preds <- predict(estimated_model, 
                   new_data = recipes::bake(object = recipes::prep(mtcars_rec), new_data = new_data))
  
  preds_garnished <- garnish(predictions = preds, object = estimated_model)
  
  expect_equal(preds_garnished, preds)
})



test_that("garnish inverts a log transformation", {
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + hp, data = data) %>%
    recipes::step_log(recipes::all_outcomes(), id = "outcomes log", skip = TRUE) %>%
    recipes::step_center(-recipes::all_outcomes(), id = "predictors scale")
  
  # create a workflow with the model and recipe
  model_wf <- workflows::workflow() %>%
    workflows::add_model(rpart_mod) %>%
    workflows::add_recipe(mtcars_rec)
  
  # estimate the specified model using the confidential data
  estimated_model <- model_wf %>%
    parsnip::fit(data = data)
  
  preds <- predict(estimated_model, 
                   new_data = recipes::bake(object = recipes::prep(mtcars_rec), new_data = new_data))
  
  preds_garnished <- garnish(predictions = preds, object = estimated_model)
  
  expect_equal(preds_garnished, tibble::tibble(.pred = exp(preds$.pred)))
})

test_that("garnish inverts a Yeo-Johnson transformation", {
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + hp, data = data) %>%
    recipes::step_YeoJohnson(recipes::all_outcomes(), id = "outcomes yj", skip = TRUE) %>%
    recipes::step_center(-recipes::all_outcomes(), id = "predictors scale")
  
  # create a workflow with the model and recipe
  model_wf <- workflows::workflow() %>%
    workflows::add_model(rpart_mod) %>%
    workflows::add_recipe(mtcars_rec)
  
  # estimate the specified model using the confidential data
  estimated_model <- model_wf %>%
    parsnip::fit(data = data)
  
  preds <- predict(estimated_model, 
                   new_data = recipes::bake(object = recipes::prep(mtcars_rec), new_data = new_data))
  
  preds_garnished <- garnish(predictions = preds, object = estimated_model)
  
  lambda <- estimated_model[["pre"]][["mold"]][["blueprint"]][["recipe"]][["steps"]][[1]][["lambdas"]]
  
  expect_equal(
    preds_garnished, 
    tibble::tibble(.pred = unname((preds$.pred * lambda + 1) ^ (1 / lambda) - 1))
  )
})

test_that("garnish works with synthesize_j()  ", {
  
  rpart_mod <- parsnip::decision_tree() %>% 
    parsnip::set_engine("rpart") %>%
    parsnip::set_mode(mode = "regression")
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  mtcars_rec_log <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars) %>%
    recipes::step_log(recipes::all_outcomes(), id = "outcome log", skip = TRUE)
  
  new_data <- tibble::tibble(
    cyl = 6,
    disp = 160,
    hp = 110
  )
  
  set.seed(202109131)
  jth_synth <- synthesize_j(conf_data = dplyr::slice_head(mtcars, n = 1),
                            synth_data = new_data,
                            col_schema = list(dtype = "dbl", na_prop = 0),
                            model = rpart_mod,
                            recipe = mtcars_rec,
                            sampler = sample_rpart,
                            noise = noise(),
                            tuner = NULL, 
                            extractor = NULL,
                            constraints = NULL,
                            invert_transformations = TRUE)
  
  set.seed(202109132)
  jth_synth_log <- synthesize_j(conf_data = dplyr::slice_head(mtcars, n = 1),
                                synth_data = new_data,
                                col_schema = list(dtype = "dbl", na_prop = 0),
                                model = rpart_mod,
                                recipe = mtcars_rec,
                                sampler = sample_rpart,
                                noise = noise(),
                                tuner = NULL, 
                                extractor = NULL,
                                constraints = NULL,
                                invert_transformations = TRUE)
  
  expect_equal(jth_synth$predictions$mpg, 21)
  expect_equal(jth_synth_log$predictions$mpg, 21)
  
})


test_that("garnish works with many observations in synthesize_j() ", {
  
  rpart_mod <- parsnip::decision_tree() %>% 
    parsnip::set_engine("rpart") %>%
    parsnip::set_mode(mode = "regression")
  
  mtcars_rec <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars)
  
  mtcars_rec_bc <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars) %>%
    recipes::step_BoxCox(recipes::all_outcomes(), id = "outcome Box-Cox", skip = TRUE)
  
  mtcars_rec_log <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars) %>%
    recipes::step_log(recipes::all_outcomes(), id = "outcome log", skip = TRUE)
  
  mtcars_rec_yj <- recipes::recipe(mpg ~ cyl + disp + hp, data = mtcars) %>%
    recipes::step_YeoJohnson(recipes::all_outcomes(), id = "outcome Yeo-Johnson", skip = TRUE)
  
  new_data <- dplyr::select(mtcars, cyl, disp, hp)

  set.seed(202109133)
  jth_synth <- synthesize_j(conf_data = mtcars,
                            synth_data = new_data,
                            col_schema = list(dtype = "dbl", na_prop = 0),
                            model = rpart_mod,
                            recipe = mtcars_rec,
                            sampler = sample_rpart,
                            noise = noise(),
                            tuner = NULL, 
                            extractor = NULL,
                            constraints = NULL,
                            invert_transformations = TRUE)
  
  set.seed(202109134)
  jth_synth_bc <- synthesize_j(conf_data = mtcars,
                               synth_data = new_data,
                               col_schema = list(dtype = "dbl", na_prop = 0),
                               model = rpart_mod,
                               recipe = mtcars_rec,
                               sampler = sample_rpart,
                               noise = noise(),
                               tuner = NULL, 
                               extractor = NULL,
                               constraints = NULL,
                               invert_transformations = TRUE)
  
  set.seed(202109135)
  jth_synth_log <- synthesize_j(conf_data = mtcars,
                                synth_data = new_data,
                                col_schema = list(dtype = "dbl", na_prop = 0),
                                model = rpart_mod,
                                recipe = mtcars_rec,
                                sampler = sample_rpart,
                                noise = noise(),
                                tuner = NULL, 
                                extractor = NULL,
                                constraints = NULL,
                                invert_transformations = TRUE)
  
  set.seed(202109136)
  jth_synth_yj <- synthesize_j(conf_data = mtcars,
                               synth_data = new_data,
                               col_schema = list(dtype = "dbl", na_prop = 0),
                               model = rpart_mod,
                               recipe = mtcars_rec,
                               sampler = sample_rpart,
                               noise = noise(),
                               tuner = NULL, 
                               extractor = NULL,
                               constraints = NULL,
                               invert_transformations = TRUE)
  
  comparison_vector <- rep(c(TRUE), times = 32)
  
  expect_setequal((round(jth_synth$predictions$mpg, 6) %in% mtcars$mpg), comparison_vector)
  expect_setequal((round(jth_synth_bc$predictions$mpg, 6) %in% mtcars$mpg), comparison_vector)
  expect_setequal((round(jth_synth_log$predictions$mpg, 6) %in% mtcars$mpg), comparison_vector)
  expect_setequal((round(jth_synth_yj$predictions$mpg, 6) %in% mtcars$mpg), comparison_vector)
  
  
})
