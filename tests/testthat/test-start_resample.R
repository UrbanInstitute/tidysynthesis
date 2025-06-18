test_that("start_resample basic functionality", {
  
  # specify an example resampling with no warnings
  start_result <- start_resample(
    acs_start_nw %>% dplyr::select(county, gq), 
    n = 100, 
    inv_noise_scale = 1.0,
    support = "all")
  
  # expect correct output dimensions
  expect_equal(nrow(start_result), 100)
  expect_equal(ncol(start_result), 2)
  
  # expect correct column names and types
  expect_identical(names(start_result), c("county", "gq"))
  expect_true(all(purrr::map_chr(start_result, pillar::type_sum) == "fct"))

  # expect level sizes preserved
  expect_equal(
    purrr::map_int(start_result, \(x) { length(levels(x)) }), 
    c("county" = 4, "gq" = 3)
  )
  
})


test_that("start_resample privacy warnings", {
  
  # raise message using default settings for public n
  expect_message(
    start_resample(acs_start_nw, 
                   n = NULL,
                   inv_noise_scale = 1.,
                   support = "all")
  )
  
  # raise message using default settings for support
  expect_message(
    start_resample(acs_start_nw, 
                   n = 100,
                   inv_noise_scale = 1.,
                   support = "observed")
  )
  
})

test_that("start_resample error handling", {
  
  # error when using no noise for all support
  expect_error(
    start_resample(acs_start_nw, 
                   n = 100,
                   inv_noise_scale = 0,
                   support = "all")
  )
  
  expect_error(
    start_resample(acs_start_nw, 
                   n = 100,
                   inv_noise_scale = NULL,
                   support = "all")
  )
  
})

test_that("start_resample support options", {
  
  # add empty level
  start_w_empty_levels <- acs_start_nw %>%
    dplyr::mutate(county = factor(county, levels = c("Other", 
                                                     "Douglas",
                                                     "Lancaster", 
                                                     "Sarpy",
                                                     "Empty"))) %>%
    dplyr::select(county, gq)
  
  # observed support should raise a message
  expect_message(
    result_obs <- start_resample(start_w_empty_levels, 
                                 n = 10000,
                                 inv_noise_scale = 1.,
                                 support = "observed")
  )
  
  # observed support should yield no values in the empty setting
  expect_equal(sum(result_obs[["county"]] == "Empty"), 0)

  # complete support should yield no messages
  expect_no_message({
    
    set.seed(20241212)
    result_all <- start_resample(start_w_empty_levels, 
                                 n = 10000,
                                 inv_noise_scale = .001,
                                 support = "all")
  })
  
  # for this RNG seed, expect some empty levels synthesized
  expect_true(sum(result_all[["county"]] == "Empty") > 0)
  
})

test_that("start_resample inv_noise_scale", {
  
  set.seed(20241212)
  
  # calculate resampled counts at two different inv_noise_scales with 
  # 100 replicates 
  s1 <- purrr::map_int(1:100, \(x) {
    start_resample(acs_start_nw %>% dplyr::select(county, gq), 
                   n = 1000, 
                   inv_noise_scale = 1.0,
                   support = "all") %>% 
      dplyr::mutate(ct = as.integer(county == "Douglas")) %>% 
      dplyr::pull("ct") %>% 
      sum()
    })
  
  s2 <- purrr::map_int(1:100, \(x) {
    start_resample(acs_start_nw %>% dplyr::select(county, gq), 
                   n = 1000, 
                   inv_noise_scale = 0.1,
                   support = "all") %>% 
      dplyr::mutate(ct = as.integer(county == "Douglas")) %>% 
      dplyr::pull("ct") %>% 
      sum()
  })
  
  # expect that smaller inv_noise_scale values increases variance
  expect_true(var(s1) < var(s2))
  
})

test_that("start_resample observed support", {
  
  set.seed(20241212)
  
  # generate random uniform samples from observed support
  samples <- purrr::map(1:100, \(x) {
    suppressMessages({
      start_resample(acs_start_nw %>% dplyr::select(county, gq), 
                     n = 1000, 
                     inv_noise_scale = 0)
    })
  }) %>% 
    dplyr::bind_rows() %>%
    dplyr::group_by(county, gq) %>% 
    dplyr::tally()
  
  expect_true(nrow(samples) == 10)
  
  # expect only observed support 
  expected_value <- 1000 * 100 / nrow(samples)
  
  # expect approximately uniform samples
  sample_err <- samples %>%
    dplyr::mutate(err = (n - expected_value) / expected_value, 
                  .keep = "none") %>%
    dplyr::pull(err)
  
  expect_true(max(abs(sample_err)) <= .05)
  
})



test_that("start_resample uniform observed support", {
  
  set.seed(20241212)
  
  # generate random uniform samples from observed support
  samples <- purrr::map(1:100, \(x) {
    suppressMessages({
      start_resample(acs_start_nw %>% dplyr::select(county, gq), 
                     n = 1000, 
                     inv_noise_scale = 0)
    })
  }) %>% 
    dplyr::bind_rows() %>%
    dplyr::group_by(county, gq) %>% 
    dplyr::tally()
  
  # expect only observed support 
  expect_true(nrow(samples) == 10)
  
  expected_value <- 1000 * 100 / nrow(samples)
  
  # expect approximately uniform samples
  sample_err <- samples %>%
    dplyr::transmute(err = (n - expected_value) / expected_value) %>%
    dplyr::pull(err)
  
  expect_true(max(abs(sample_err)) <= .05)
  
})


test_that("start_resample uniform from observed support", {
  
  set.seed(20241212)
  
  # generate random uniform samples from observed support
  expect_message(
    rs1 <- start_resample(
      acs_start_nw %>% dplyr::select(county, gq), 
      n = 1000, 
      support = "observed",
      inv_noise_scale = NULL
    )
  )
  
  expect_true(all(dim(rs1) == c(1000, 2)))
  
})

test_that("start_resample from full support with numeric casting", {
  
  rs2 <- start_resample(
    acs_start_nw %>% 
      dplyr::select(county, gq) %>%
      dplyr::mutate(gq = as.integer(gq)), 
    n = 1000, 
    support = "all",
    inv_noise_scale = 2.
  )
  
  expect_true(all(dim(rs2) == c(1000, 2)))
  
})
