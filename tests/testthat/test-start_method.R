acs_roadmap <- roadmap(conf_data = acs_conf_nw, start_data = acs_start_nw)

test_that("start_method construction", {
  
  # create default start_method
  default_sm <- start_method()
  
  expect_s3_class(default_sm, "start_method")
  
  # expect default method is identity
  expect_equal(class(default_sm[["start_func"]]), "function")
  expect_identical(
    deparse(default_sm[["start_func"]]), deparse(.identity_start)
  )
  
  # expect no kwargs by default
  expect_equal(class(default_sm[["kwargs"]]), "list")
  expect_equal(length(default_sm[["kwargs"]]), 0)
  
  # create non-trivial start_method
  new_sm <- start_method(start_func = start_resample, 
                         n = 12345)
  
  expect_s3_class(new_sm, "start_method")
  expect_equal(class(new_sm[["start_func"]]), "function")
  
  # expect function and arguments passed properly
  expect_identical(
    deparse(new_sm[["start_func"]]), deparse(start_resample)
  )
  
  expect_identical(new_sm[["kwargs"]], list(n = 12345))
  
})

test_that("validate_start_method", {
  
  # expect error if wrong start_method type
  expect_error( {
      acs_roadmap <- roadmap(conf_data = acs_conf_nw, 
                            start_data = acs_start_nw)
      acs_roadmap[["start_method"]] <- "wrong"
      validate_start_method(acs_roadmap)
    },
    regexp = "`start_method` must be a start_method object",
    fixed = TRUE
  )
  
  # expect error if wrong keyword arguments provided
  expect_error(
    {
      roadmap(conf_data = acs_conf_nw, 
              start_data = acs_start_nw) %>%
        update_start_method(not_an_arg = 123) %>%
        validate_start_method()
    },
    regexp = "Keyword arguments not aligned with provided start_method function",
    fixed = TRUE
  )
  
})

# Tidy API calls -------------------------------------------------------------

test_that("add_start_method functionality", {
  
  old_roadmap <- acs_roadmap 
  
  # add start_method with default options
  new_roadmap <- acs_roadmap %>%
    add_start_method(start_method(start_func = start_resample))
  
  # expect new object is roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new start_method is a `start_method`
  expect_s3_class(new_roadmap[["start_method"]], "start_method")
  
})

test_that("update_start_method functionality", {
  
  # create old_roadmap with start_resample
  old_roadmap <- roadmap(conf_data = acs_conf_nw, 
                         start_data = acs_start_nw)
  
  new_roadmap <- old_roadmap %>%
    update_start_method(start_func = start_resample, 
                        n = 123)
  
  # expect new object is roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new start_method is a `start_method`
  expect_s3_class(new_roadmap[["start_method"]], "start_method")
  
  # expect new start_method is the right function
  expect_true(
    "function" %in% class(new_roadmap[["start_method"]][["start_func"]]))
  expect_identical(
    deparse(new_roadmap[["start_method"]][["start_func"]]), 
    deparse(start_resample)
  )
  
  # expect argument is updated
  expect_identical(new_roadmap[["start_method"]][["kwargs"]], 
                   list(n = 123))
  
})

test_that("remove_start_method functionality", {
  
  old_roadmap <- roadmap(conf_data = acs_conf_nw, 
                         start_data = acs_start_nw,
                         start_method = start_method(
                           start_func = start_resample, 
                           n = 12345))
  
  new_roadmap <- old_roadmap %>%
    remove_start_method()
  
  # expect new object is roadmap
  expect_s3_class(new_roadmap, "roadmap")
  
  # expect new start_method is a `start_method`
  expect_s3_class(new_roadmap[["start_method"]], "start_method")
  
  # expect default method is restored
  expect_equal(class(new_roadmap[["start_method"]][["start_func"]]), "function")
  expect_identical(
    deparse(new_roadmap[["start_method"]][["start_func"]]), 
    deparse(.identity_start)
  )
  
})

test_that("exec_start_method", {
  
  rmap <- roadmap(
    conf_data = acs_conf,
    start_data = acs_start %>%
      dplyr::select(county, gq),
    start_method = start_method(
      start_func = start_resample, 
      n = 100,
      support = "observed"
    )
  ) 
  
  expect_message(
    new_start_data <- exec_start_method(rmap)
  )
  
  expect_equal(dim(new_start_data), c(100, 2))
  
})


test_that("exec_start_method within synthesis", {
  
  start_data <- dplyr::select(mtcars, cyl, vs, am, gear, carb)
  
  roadmap <- roadmap(
    conf_data = mtcars,
    start_data = start_data,
    start_method = start_method(
      start_func = start_resample, 
      n = 20,
      support = "observed"
    )
  ) %>% 
    add_sequence_numeric(dplyr::everything(), 
                         method = "correlation", 
                         cor_var = "mpg")
  
  dt_mod <- parsnip::decision_tree() %>%
    parsnip::set_engine(engine = "rpart") %>%
    parsnip::set_mode(mode = "regression")
  
  synth_spec <- synth_spec(default_regression_model = dt_mod,
                           default_regression_sampler = sample_rpart)
  
  expect_warning(
    presynth <- presynth(roadmap = roadmap, synth_spec = synth_spec)
  )
  
  ps <- synthesize(presynth)
  
  expect_true(nrow(roadmap$start_data) != 20)
  expect_true(nrow(ps$synthetic_data) == 20)
  
})

test_that("print.start_method", {
  
  sm1 <- start_method(start_func = start_resample, 
                      n = 100, 
                      inv_noise_scale = 3)
  
  expect_output(print(sm1), "Start Method: User-Specified")
  expect_output(print(sm1), "n: 100")
  expect_output(print(sm1), "inv_noise_scale: 3")
  
  sm2 <- start_method()
  expect_output(print(sm2), "Start Method: Identity")
  
})
