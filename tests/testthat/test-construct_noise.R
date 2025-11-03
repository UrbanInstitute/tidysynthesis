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


test_that("Unspecified noise raises a warning, not an error", {
  
  expect_no_error(
    expect_warning(
      construct_noise(
        roadmap = roadmap, 
        default_regression_noise = NULL,
        default_classification_noise = NULL,
        custom_noise = NULL
      )
    )
  )
  
})

test_that("input errors work correctly", {
  
  # type checking
  expect_error(construct_noise(roadmap = "no"))
  
  expect_error(
    construct_noise(
      roadmap = roadmap,
      default_regression_noise = "notanoise"
    )
  )
  
  expect_error(
    construct_noise(
      roadmap = roadmap,
      default_classification_noise = "notanoise"
    )
  )
  
  expect_error(
    construct_noise(
      roadmap = roadmap,
      custom_noise = "notnoises"
    )
  )
  
})

test_that("default noise", {
  
  noise_defaults <- construct_noise(
    roadmap = roadmap,
    default_regression_noise = noise(),
    default_classification_noise = noise() # noise_cat
  )
  
  expect_equal(
    noise_defaults,
    list(
      "color" = noise(), # noise_cat
      "cut" = noise(), # noise_cat
      "price" = noise(),
      "table" = noise()
    )
  )
  
})

test_that("fully default and fully custom are identical", {
  
  default <- construct_noise(
    roadmap = roadmap, 
    default_regression_noise = noise(add_noise = TRUE,
                                     noise_func = add_noise_kde), 
    default_classification_noise = noise(add_noise = FALSE), 
    custom_noise = NULL
  )
  
  custom <- construct_noise(
    roadmap = roadmap, 
    custom_noise = list(
      list("vars" = c("color", "cut"), "noise" = noise()), # noise_cat
      list("vars" = c("price", "table"), 
           "noise" = noise(add_noise = TRUE,
                           noise_func = add_noise_kde))
    )
  )
  
  expect_equal(default, custom)
  
})




test_that("construct_noise() correctly handles variables without variation ", {
  
  conf_data <- tibble::tibble(
    start = c(1, 1, 1),
    num_var1 = c(1, 1, 1),
    num_var2 = c(1, 2, 3),
    fctr_var1 = factor(c("a", "a", "a")),
    fctr_var2 = factor(c("a", "b", "c"))
  )
  
  start_data <- conf_data |>
    dplyr::select(start)
  
  roadmap <- roadmap(conf_data = conf_data, start_data = start_data) |>
    add_sequence_manual(num_var1, num_var2, fctr_var1, fctr_var2)
  
  noise <- construct_noise(
    roadmap = roadmap,
    default_regression_noise = noise(),
    default_classification_noise = noise()
  )
  
  expect_equal(noise[[1]], "identity")
  expect_equal(noise[[3]], "identity")
  
})
