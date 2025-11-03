df <- tibble::tibble(
  color = c("blue", "blue"),
  cut = c("Ideal", "Premium"),
  carat = c(1, 2),
  price = c(2, 3),
  table = c(3, 3)
)

df_start <- dplyr::select(df, color)

# roadmap
roadmap <- roadmap(conf_data = df, start_data = df_start)


step1 <- function(x) {
  x %>%
    recipes::step_center(recipes::all_predictors(), id = "center")
}

step2 <- function(x) {
  x %>%
    recipes::step_scale(recipes::all_predictors(), id = "scale")
}

# this is the identity recipe
identity <- list(
  var_info = tibble::tibble(
    variable = NA,
    type = "identity",
    role = "outcome",
    source = ""
  ),
  term_info = NULL,
  steps = NULL,
  template= NULL,
  levels = NULL,
  retained = NA,
  requirements = NULL
)

test_that("input errors work correctly", {
   
  # roadmap must be a roadmap
  expect_error(
    construct_recipes(
      roadmap = "apple", 
      default_regression_steps = step1
    ),
    regexp = "`roadmap` must be a roadmap object",
    fixed = TRUE
  )
  
  # recipes must be a function
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      default_regression_steps = "banana"
    ),
    regexp = "Default regression step(s) has incorrect type",
    fixed = TRUE
  )
  
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      default_classification_steps = "banana"
    ),
    regexp = "Default classification step(s) has incorrect type",
    fixed = TRUE
  )
  
  # custom recipes must be a list of functions
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      default_regression_steps = step1,
      custom_steps = "apple"
    ),
    regexp = "subscript out of bounds",
    fixed = TRUE
  )
  
  # custom recipes must be correctly specified
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      default_regression_steps = step1,
      default_classification_steps = step1,
      custom_steps = list(list("vars" = c("price", "wrong_var"), 
                               "steps" = step2))
    ),
    regexp = "Custom step(s) list has variables not in visit_sequence: wrong_var",
    fixed = TRUE
  )
  
  # custom recipes should not have duplicate vars
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      default_regression_steps = step1,
      default_classification_steps = step1,
      custom_steps = list(list("vars" = c("price"), 
                               "steps" = step1),
                          list("vars" = c("price"), 
                               "steps" = step2))
    ),
    regexp = "Custom step(s) list has repeated variable names: price",
    fixed = TRUE
  )
  
})

test_that("directly test outputs", {
  
  recipes_default <- construct_recipes(
    roadmap = roadmap, 
    default_regression_steps = step1,
    default_classification_steps = step1
  )
  
  expect_length(recipes_default, 4)
  expect_s3_class(recipes_default[[1]], "recipe")
  expect_s3_class(recipes_default[[2]], "recipe")
  expect_s3_class(recipes_default[[3]], "recipe")
  expect_equal(class(recipes_default[[4]]), "list")
  
  
  recipes_hybrid <- construct_recipes(
    roadmap = roadmap, 
    default_regression_steps = step1,
    default_classification_steps = step1,
    custom_steps = list(list("vars" = "price", "steps" = step2))
  )
  
  expect_length(recipes_hybrid, 4)
  expect_s3_class(recipes_hybrid[[1]], "recipe")
  expect_s3_class(recipes_hybrid[[2]], "recipe")
  expect_s3_class(recipes_hybrid[[3]], "recipe")
  expect_equal(class(recipes_hybrid[[4]]), "list")
  
})

test_that("construct_recipes returns expected values", {
  
  # test a simple recipe
  local_identity <- identity
  local_identity[["var_info"]][["variable"]] <- "table"
  
  recipes_by_hand <- list(
    "cut" = recipes::recipe(data = head(df), formula = "cut ~ color"),
    "carat" = recipes::recipe(data = head(df), formula = "carat ~ color + cut"),
    "price" = recipes::recipe(data = head(df), formula = "price ~ color + cut + carat"),
    "table" = local_identity
  )
  
  recipes_by_function <- construct_recipes(roadmap = roadmap)
  
  expect_equal(recipes_by_hand, recipes_by_function)
  
})

test_that("fully default and hybrid are identical", {
  
  recipes_default <- construct_recipes(
    roadmap = roadmap, 
    default_regression_steps = step1,
    default_classification_steps = step2
  )
  
  recipes_hybrid <- construct_recipes(
    roadmap = roadmap, 
    default_regression_steps = step1,
    default_classification_steps = step2,
    custom_steps = list(
      list(vars = "price", steps = step1), 
      list(vars = "cut", steps = step2)
    )
  )
  
  expect_equal(recipes_default, recipes_hybrid)
  
})

test_that("try to break the custom recipes", {
  
  # wrong order
  wrong_order <- list(
    "price" = step1,
    "carat" = step1
  )
  
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      custom_steps = wrong_order
    )
  )
  
  # too few variables
  too_few_vars <- custom <- list(
    "price" = step1
  )
  
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      custom_steps = too_few_vars
    ),
    regexp = "Can't pluck from a function at level 1.",
    fixed = FALSE
  )
  
  # incorrect variables
  incorrect_vars <- custom <- list(
    "price" = step1,
    "TABLE" = step1
  )
  
  expect_error(
    construct_recipes(
      roadmap = roadmap, 
      custom_steps = incorrect_vars
    ),
    regexp = "Can't pluck from a function at level 1.",
    fixed = FALSE
  )
  
})

test_that("construct_recipes() correctly handles variables without variation ", {
  
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
  
  step1 <- function(x) recipes::step_pca(x, recipes::all_predictors())
  
  recipes <- construct_recipes(
    roadmap = roadmap, 
    default_regression_steps = step1,
    default_classification_steps = step1
  )
  
  expect_equal(recipes[[1]]$var_info$type, "identity")
  expect_equal(recipes[[3]]$var_info$type, "identity")
  
})


test_that("construct_recipes() can handle large visit sequences ", {
  
  nvar <- 600
  
  data <- purrr::map(.x = 1:nvar, ~tibble::tibble(x = rnorm(n = 1000))) %>%
    purrr::reduce(cbind)
  
  names(data) <- paste0("var", 1:nvar)
  
  data <- tibble::as_tibble(data)
  
  data <- dplyr::bind_cols(
    start = factor(sample(c("a", "b"), size = 1000, replace = TRUE)),
    data
  )
  
  start_data <- dplyr::select(data, start)
  
  roadmap <- roadmap(conf_data = data, start_data = start_data)
  
  recipes <- construct_recipes(roadmap)
  
  # No "Error: C stack usage  7955320 is too close to the limit"
  expect_equal(length(recipes), nvar)
  expect_equal(names(recipes), paste0("var", 1:nvar))
  
})


step1 <- function(x) {
  x %>%
    recipes::step_center(recipes::all_predictors(), id = "center")
}

test_that("construct_recipes() works with exactly one variable ", {
  
  start_data <- dplyr::select(cars, speed)
  
  roadmap <- roadmap(conf_data = cars, start_data = start_data)
  
  # ensure default behavior runs without error
  recipes <- construct_recipes(roadmap)
  expect_equal(names(recipes), "dist")
  
  # ensure default and custom specifications match
  recipe_default <- construct_recipes(
    roadmap = roadmap,
    default_regression_steps = step1
  )
  recipe_custom <- construct_recipes(
    roadmap = roadmap,
    custom_steps = list(list("vars" = c("dist"), "steps" = step1))
  )
  expect_equal(recipe_default, recipe_custom)

  # expect no error when providing an unused default object
  no_class_recipes <- construct_recipes(
    roadmap = roadmap,
    default_classification_steps = step1
  )
  expect_equal(no_class_recipes, recipes)
  
})
