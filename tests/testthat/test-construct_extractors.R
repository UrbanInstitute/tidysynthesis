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
  
  # defaults 
  expect_no_error(
    expect_warning(
      construct_extractors(
        roadmap = roadmap, 
        default_extractor = NULL,
        custom_extractors = NULL
      )
    )
  )
  
  expect_error(
    construct_extractors(
      roadmap = df, 
      default_extractor = parsnip::extract_fit_engine, 
      custom_extractors = NULL
    ),
    regexp = "`roadmap` must be a roadmap object",
    fixed = TRUE
  )
  
  expect_error(
    construct_extractors(
      roadmap = roadmap, 
      default_extractor = parsnip::extract_fit_engine, 
      custom_extractors = list("table" = "table")
    ),
    regexp = "subscript out of bounds",
    fixed = TRUE
  )

})

test_that("directly test outputs", {
  
  extractors_default <- construct_extractors(
    roadmap = roadmap, 
    default_extractor = parsnip::extract_fit_engine
  )
  
  expect_length(extractors_default, 4)
  expect_equal(class(extractors_default[[1]]), "function")
  expect_equal(class(extractors_default[[2]]), "function")
  expect_equal(class(extractors_default[[3]]), "function")
  expect_equal(class(extractors_default[[4]]), "function")
  
  
  extractors_hybrid <- construct_extractors(
    roadmap = roadmap, 
    default_extractor = parsnip::extract_fit_engine,
    custom_extractors = list(list(vars = "price", extractor = parsnip::extract_parameter_dials))
  )
  
  expect_length(extractors_hybrid, 4)
  expect_equal(class(extractors_hybrid[[1]]), "function")
  expect_equal(class(extractors_hybrid[[2]]), "function")
  expect_equal(class(extractors_hybrid[[3]]), "function")
  expect_equal(class(extractors_hybrid[[4]]), "function")
  
})

test_that("fully default and fully custom are identical", {
  
  default <- construct_extractors(
    roadmap = roadmap, 
    default_extractor = parsnip::extract_fit_engine
  )
  
  custom <- construct_extractors(
    roadmap = roadmap, 
    custom_extractors = list(
      list(vars = c("price", "table"),
           extractor = parsnip::extract_fit_engine),
      list(vars = c("color", "cut"),
           extractor = parsnip::extract_fit_engine)
    )
  )

  expect_equal(default, custom)
  
})
