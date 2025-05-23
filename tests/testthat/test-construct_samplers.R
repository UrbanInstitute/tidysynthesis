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


test_that("input errors work correctly", {
  
  # type checking
  expect_error(construct_samplers(roadmap = "no"))
  
  # no samplers specified
  expect_error(
    construct_samplers(
      roadmap = roadmap
    )
  )
  
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      default_regression_sampler = "notasampler"
    )
  )
  
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      default_classification_sampler = "notasampler"
    )
  )
  
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      custom_samplers = "notsamplers"
    )
  )
  
  # inclusion checking for missing custom specifications
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      custom_samplers = list(
        list("vars" = c("color", "cut"), "sampler" = sample_ranger)
      )
    )
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
      default_regression_sampler = sample_rpart
    )
  )
  
  expect_error(
    construct_samplers(
      roadmap = roadmap,
      default_classification_sampler = sample_rpart
    )
  )
  
})

test_that("default samplers", {
  
  samplers_defaults <- construct_samplers(
    roadmap = roadmap,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_ranger
  )
  
  expect_equal(
    samplers_defaults,
    list(
      "color" = sample_ranger,
      "cut" = sample_ranger,
      "price" = sample_rpart,
      "table" = sample_rpart
    )
  )
  
})

test_that("fully default and fully custom are identical", {
  
  default <- construct_samplers(roadmap = roadmap, 
                                default_regression_sampler = sample_rpart, 
                                default_classification_sampler = sample_ranger,
                                custom_samplers = NULL)
  
  custom <- construct_samplers(
    roadmap = roadmap, 
    custom_samplers = list(
      list("vars" = c("color", "cut"), "sampler" = sample_ranger),
      list("vars" = c("price", "table"), "sampler" = sample_rpart)
    )
  )
  
  expect_equal(default, custom)
  
})




test_that("construct_samplers() correctly handles variables without variation ", {
  
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
  
  samplers <- construct_samplers(
    roadmap = roadmap,
    default_regression_sampler = sample_rpart,
    default_classification_sampler = sample_ranger
  )
  
  expect_equal(samplers[[1]], "identity")
  expect_equal(samplers[[3]], "identity")
  
})

test_that("construct_samplers() works identically with length 1 sequence", {
  
  rmap <- roadmap(
    conf_data = df %>% dplyr::select(carat, price),
    start_data = df_start
  )
  
  default1 <- construct_samplers(
    roadmap = rmap,
    default_regression_sampler = sample_rpart
  )
  
  custom1 <- construct_samplers(
    roadmap = rmap,
    custom_samplers = list(list("vars" = c("price"), "sampler" = sample_rpart))
  )
  
  expect_equal(default1, custom1)
  
})