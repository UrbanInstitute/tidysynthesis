df <- tibble::tibble(
  color = c("blue", "red"),
  cut = c("Ideal", "Premium"),
  carat = c(1, 2),
  price = c(2, 3),
  table = c(3, 4)
)

df_start <- dplyr::select(df, carat)

# roadmap
roadmap <- roadmap(conf_data = df, start_data = df_start)

# model objects
rpart_mod <- parsnip::decision_tree() %>%
  parsnip::set_engine(engine = "rpart") %>%
  parsnip::set_mode(mode = "regression")

rpart_mod_class <- parsnip::decision_tree() %>%
  parsnip::set_engine(engine = "rpart") %>%
  parsnip::set_mode(mode = "classification")

lm_mod <- parsnip::linear_reg() %>% 
  parsnip::set_engine("lm") %>%
  parsnip::set_mode(mode = "regression")

test_that("input errors work correctly", {
  
  # roadmap must be a roadmap
  expect_error(
    construct_models(
      roadmap = "apple", 
      default_regression_model = rpart_mod
    ),
    regexp = "`roadmap` must be a roadmap object",
    fixed = TRUE
  )
  
  # must specify models
  expect_error(
    construct_models(
      roadmap = roadmap
    ),
    regexp = "No model(s) specified",
    fixed = TRUE
  )
  
  # default model must be a model_spec object
  expect_error(
    construct_models(
      roadmap = roadmap, 
      default_regression_model = "banana"
    ),
    regexp = "Default regression model(s) has incorrect type",
    fixed = TRUE
  )
  
  expect_error(
    construct_models(
      roadmap = roadmap, 
      default_classification_model = "banana",
    ),
    regexp = "Default classification model(s) has incorrect type",
    fixed = TRUE
  )
  
  # custom model must be a list of model_spec objects
  expect_error(
    construct_models(
      roadmap = roadmap, 
      custom_model = "apple"
    ),
    regexp = "Custom model(s) list missing variable without model(s) specification: colorcutpricetable",
    fixed = TRUE
  )
  
  # custom model must be properly specified
  expect_error(
    construct_models(
      roadmap = roadmap,
      custom_models = list(
        list("vars" = c("not", "in", "visit_sequence"), 
             "model" = rpart_mod)
      )
    ),
    regexp = "Custom model(s) list has variables not in visit_sequence: not, in, visit_sequence",
    fixed = TRUE
  )
  
  # duplicate custom specifications
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      custom_samplers = list(
        list("vars" = c("color", "cut"), "sampler" = sample_ranger),
        list("vars" = c("price", "table", "color"), "sampler" = sample_ranger)
      )
    )
  )
  
  # including only regression or classification samplers when both are needed
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      default_regression_model = sample_rpart
    )
  )
  
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      default_classification_model = rpart_mod_class
    )
  )
  
})

test_that("directly test outputs", {
  
  models_default <- construct_models(
    roadmap = roadmap, 
    default_regression_model = lm_mod,
    default_classification_model = rpart_mod
  )
  
  expect_length(models_default, 4)
  expect_s3_class(models_default[[1]], "decision_tree")
  expect_s3_class(models_default[[2]], "decision_tree")
  expect_s3_class(models_default[[3]], "linear_reg")
  expect_s3_class(models_default[[4]], "linear_reg")
  
  
  models_hybrid <- construct_models(
    roadmap = roadmap, 
    default_regression_model = lm_mod,
    custom_models = list(list(vars = "price", model = rpart_mod)), 
    default_classification_model = rpart_mod
  )
  
  expect_length(models_hybrid, 4)
  expect_s3_class(models_hybrid[[1]], "decision_tree")
  expect_s3_class(models_hybrid[[2]], "decision_tree")
  expect_s3_class(models_hybrid[[3]], "decision_tree")
  expect_s3_class(models_hybrid[[4]], "linear_reg")
  
})

test_that("fully default and fully custom are identical", {
  
  models_default <- construct_models(
    roadmap = roadmap, 
    default_regression_model = lm_mod,
    default_classification_model = rpart_mod
  )
  
  models_hybrid <- construct_models(
    roadmap = roadmap, 
    default_regression_model = lm_mod,
    default_classification_model = rpart_mod,
    custom_models = list(
      list(vars = "price", model = lm_mod), 
      list(vars = "cut", model = rpart_mod)
    )
  )
  
  
  expect_equal(models_default, models_hybrid)

})

test_that("construct_models() correctly handles variables without variation ", {
  
  conf_data <- tibble::tibble(
    start = c(1, 1, 1),
    num_var1 = c(1, 1, 1),
    num_var2 = c(1, 2, 3),
    fctr_var1 = factor(c("a", "a", "a")),
    fctr_var2 = factor(c("a", "b", "c"))
  )

  start_data <- conf_data %>%
    dplyr::select(start)
  
  roadmap <- roadmap(conf_data = conf_data, start_data = start_data) |>
    add_sequence_manual(num_var1, num_var2, fctr_var1, fctr_var2)
  
  models <- construct_models(
    roadmap = roadmap, 
    default_regression_model = lm_mod,
    default_classification_model = rpart_mod
  )
  
  # this is the identity model
  identity <- list(
    args = NULL,
    eng_args = NULL,
    mode = "identity",
    use_specified_mode = TRUE,
    method = NULL,
    engine = "identity",
    user_specified_engine = TRUE
  )
  
  expect_equal(models[[1]], identity)
  expect_equal(models[[3]], identity)
  
})

test_that("construct_models() works identically with length 1 sequence", {
  
  rmap <- roadmap(
    conf_data = df %>% dplyr::select(carat, price),
    start_data = df_start
  )
  
  default1 <- construct_models(
    roadmap = rmap,
    default_regression_model = rpart_mod
  )
  
  custom1 <- construct_models(
    roadmap = rmap,
    custom_models = list(list("vars" = c("price"), "model" = rpart_mod))
  )
  
  expect_equal(default1, custom1)
  
})
