data <- tibble::tibble(y = rlnorm(n = 1000, meanlog = 0, sdlog = 1),
                       x = rnorm(n = 1000))

test_that("invert.step_BoxCox returns untransformed values ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_BoxCox(recipes::all_outcomes()) |>
    recipes::prep()

  y_reversed <- invert(object = adj$steps[[1]], 
                       predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  
  expect_equal(data$y, y_reversed$.pred)
  
})

test_that("invert.step_BoxCox fails with incorrect lambdas length ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_BoxCox(recipes::all_outcomes()) |>
    recipes::prep()
  
  adj$steps[[1]]$lambdas <- c(1, 2)
  
  expect_error(
    invert(object = adj$steps[[1]], 
           predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  )
  
})

test_that("invert.step_BoxCox small lambda ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_BoxCox(recipes::all_outcomes()) |>
    recipes::prep()
  
  adj$steps[[1]]$lambdas <- c(y = .00001)
  
  y_reversed <- invert(object = adj$steps[[1]], 
                       predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  
  expect_true(
    all(
      exp(tibble::tibble(.pred = adj[["template"]][["y"]]))$.pred == y_reversed$.pred
    )
  )
  
})

test_that("invert.step_BoxCox NA ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_BoxCox(recipes::all_outcomes()) |>
    recipes::prep()
  
  adj$steps[[1]]$lambdas <- c(y = NA)
  
  y_reversed <- invert(object = adj$steps[[1]], 
                       predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  
  expect_true(
    all(
      tibble::tibble(.pred = adj[["template"]][["y"]])$.pred == y_reversed$.pred
    )
  )
  
})

test_that("invert.step_log returns untransformed values ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_log(recipes::all_outcomes()) |>
    recipes::prep()
  
  y_reversed <- invert(object = adj$steps[[1]], 
                       predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  
  expect_equal(data$y, y_reversed$.pred)
  
})

test_that("invert.step_YeoJohnson returns untransformed values ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_YeoJohnson(recipes::all_outcomes()) |>
    recipes::prep()
  
  y_reversed <- invert(object = adj$steps[[1]], 
                       predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  
  expect_equal(data$y, y_reversed$.pred)
  
})

test_that("invert.step_YeoJohnson fails with incorrect lambdas length ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_YeoJohnson(recipes::all_outcomes()) |>
    recipes::prep()
  
  adj$steps[[1]]$lambdas <- c(1, 2)
  
  expect_error(
    invert(object = adj$steps[[1]], 
           predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  )
  
})

test_that("invert.step_YeoJohnson NA ", {
  
  adj <- recipes::recipe(y ~ x, data = data) |>
    recipes::step_YeoJohnson(recipes::all_outcomes()) |>
    recipes::prep()
  
  adj$steps[[1]]$lambdas <- c(y = NA)
  
  y_reversed <- invert(object = adj$steps[[1]], 
                       predictions = tibble::tibble(.pred = adj[["template"]][["y"]]))
  
  expect_true(
    all(
      tibble::tibble(.pred = adj[["template"]][["y"]])$.pred == y_reversed$.pred
    )
  )
  
})