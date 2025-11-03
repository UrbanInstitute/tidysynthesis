df <- tibble::tibble(
  color = c("blue", "red"),
  cut = c("Ideal", "Premium"),
  carat = c(1, 2),
  price = c(2, 3),
  table = c(3, 4)
)

df_start <- dplyr::select(df, carat)
df_start_cat <- dplyr::select(df, carat, price, table)
df_start_num <- dplyr::select(df, color, cut)

# roadmap
roadmap <- roadmap(conf_data = df, start_data = df_start)

default_tuner <- list(
  v = 3,
  grid = 3,
  metrics = yardstick::metric_set(yardstick::rmse)
)

default_tuner_alt <- list(
  v = 5,
  grid = 5,
  metrics = yardstick::metric_set(yardstick::mae)
)

test_that("input errors work correctly", {
  
  expect_error(
    construct_tuners(
      roadmap = "notaroadmap", 
      default_regression_tuner = NULL,
      default_classification_tuner = NULL,
      custom_tuners = NULL
    )
  )
  
  expect_error(
    construct_tuners(
      roadmap = df, 
      default_regression_tuner = "notatuner", 
      custom_tuners = NULL)
  )
  
  expect_error(
    construct_tuners(
      roadmap = roadmap, 
      default_regression_tuner = default_tuner, 
      custom_tuners = list("table" = "table"))
  )
  
  # incorrect variable names
  expect_error(
    construct_tuners(
      roadmap = roadmap, 
      default_regression_tuner = default_tuner, 
      custom_tuners = list(list("vars" = c("color", "notavar"),
                                "tuner" = default_tuner_alt))
    )
  )
  
})

test_that("directly test outputs", {
  
  tuners_default <- construct_tuners(
    roadmap = roadmap, 
    default_regression_tuner = default_tuner,
    default_classification_tuner = default_tuner
  )
  
  expect_length(tuners_default, 4)
  expect_equal(class(tuners_default[[1]]), "list")
  expect_equal(class(tuners_default[[2]]), "list")
  expect_equal(class(tuners_default[[3]]), "list")
  expect_equal(class(tuners_default[[4]]), "list")
  
  
  tuners_hybrid <- construct_tuners(
    roadmap = roadmap, 
    default_regression_tuner = default_tuner,
    default_classification_tuner = default_tuner,
    custom_tuners = list(list(vars = "price", tuner = default_tuner_alt))
    
  )
  
  expect_length(tuners_hybrid, 4)
  expect_equal(class(tuners_hybrid[[1]]), "list")
  expect_equal(class(tuners_hybrid[[2]]), "list")
  expect_equal(class(tuners_hybrid[[3]]), "list")
  expect_equal(class(tuners_hybrid[[4]]), "list")
  
})

test_that("fully default and fully custom are identical", {
  
  default <- construct_tuners(
    roadmap = roadmap, 
    default_regression_tuner = default_tuner, 
    default_classification_tuner = default_tuner_alt
  )
  
  custom <- construct_tuners(
    roadmap = roadmap, 
    custom_tuners = list(
      list(vars = c("price", "table"),
           tuner = default_tuner),
      list(vars = c("color", "cut"),
           tuner = default_tuner_alt)
    )
  )

  expect_equal(default, custom)
  
})

test_that("construct_tuners() correctly handles variables without variation ", {
  
  conf_data <- tibble::tibble(
    start = c(1, 1, 1),
    num_var1 = c(1, 1, 1),
    num_var2 = c(1, 2, 3),
    fctr_var1 = factor(c("a", "a", "a")),
    fctr_var2 = factor(c("a", "b", "c"))
  )
  
  start_data <- conf_data |>
    dplyr::select(start)
  
  roadmap <- roadmap(
    conf_data = conf_data,
    start_data = start_data
  ) |>
    add_sequence_manual(num_var1, num_var2, fctr_var1, fctr_var2)
  
  tuners <- construct_tuners(
    roadmap = roadmap, 
    default_regression_tuner = default_tuner,
    default_classification_tuner = default_tuner
  )
  
  expect_equal(tuners[[1]], "identity")
  expect_equal(tuners[[3]], "identity")
  
})
