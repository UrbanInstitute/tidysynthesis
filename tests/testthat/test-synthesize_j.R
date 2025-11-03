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
    jth_synth <- synthesize_j(conf_data = mtcars,
                              synth_data = dplyr::select(mtcars, cyl, disp, hp),
                              col_schema = list(dtype = "dbl", na_prop = 0),
                              recipe = mtcars_rec,
                              sampler = NULL,
                              noise = default_noise,
                              tuner = NULL,
                              extractor = NULL,
                              constraints = NULL,
                              model = rpart_mod,
                              invert_transformations = TRUE)
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
