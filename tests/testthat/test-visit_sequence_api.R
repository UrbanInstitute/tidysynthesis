# add an empty factor level to test add_sequence_factor()
levels <- c("No health insurance coverage", "With health insurance coverage", "empty")

# create start roadmaps
acs_roadmap_nw <- roadmap(
  conf_data = acs_conf_nw %>% 
    tidyr::replace_na(list(inctot = 0)) %>%
    dplyr::mutate(hcovany = factor(hcovany, levels = levels)), 
  start_data = acs_start_nw
)
acs_roadmap_w <- roadmap(
  conf_data = acs_conf %>% 
    tidyr::replace_na(list(inctot = 0)) %>%
    dplyr::mutate(hcovany = factor(hcovany, levels = levels)), 
  start_data = acs_start_nw
)
acs_roadmap_w2 <- roadmap(
  conf_data = acs_conf %>% 
    tidyr::replace_na(list(inctot = 0)) %>%
    dplyr::mutate(hcovany = factor(hcovany, levels = levels)), 
  start_data = acs_start
)

acs_roadmap_na <- roadmap(
  conf_data = acs_conf %>% 
    dplyr::select(dplyr::where(is.numeric)),
  start_data = acs_conf %>% 
    dplyr::select(age, famsize, wgt)
)

test_that("visit_sequence manual order", {
  
  # add manual variables
  nr <- acs_roadmap_nw %>%
    add_sequence_manual(hcovany, age, classwkr)
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  # expect manual components properly specified
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("hcovany", "age", "classwkr")
  )
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:3] == "manual"))
  
  # expect remaining default components intact
  expect_identical(
    nr[["visit_sequence"]][["visit_sequence"]][4:7],
    setdiff(setdiff(names(acs_conf_nw), 
                    names(acs_start_nw)), 
            c("hcovany", "age", "classwkr"))
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][4:7] == "default"))
  
})

test_that("visit_sequence manual order with weight ", {
  
  # create roadmap with weight synthesis
  nr <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = TRUE) %>%
    add_sequence_manual(hcovany, age, classwkr)
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  # expect manual components properly specified
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("hcovany", "age", "classwkr")
  )
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:3] == "manual"))
  
  # expect remaining default components intact
  expect_identical(
    nr[["visit_sequence"]][["visit_sequence"]][4:7],
    setdiff(setdiff(names(acs_conf_nw), 
                    names(acs_start_nw)), 
            c("hcovany", "age", "classwkr"))
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][4:8] == "default"))
  
  # create roadmap without weight synthesis
  nr2 <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = FALSE) %>%
    add_sequence_manual(hcovany, age, classwkr)
  
  expect_s3_class(nr2[["visit_sequence"]], "visit_sequence")
  
  # expect manual components properly specified
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("hcovany", "age", "classwkr")
  )
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:3] == "manual"))
  
  # expect remaining default components intact
  expect_identical(
    nr[["visit_sequence"]][["visit_sequence"]][4:7],
    setdiff(setdiff(names(acs_conf), 
                    names(acs_start_nw)), 
            c("hcovany", "age", "classwkr", "wgt"))
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][4:7] == "default"))
  
})

# correlation -------------------------------------------------------------
test_that("visit_sequence correlation order", {
  
  nr <- acs_roadmap_nw %>%
    add_sequence_numeric(
      dplyr::where(is.numeric), 
      method = "correlation", 
      cor_var = "age"
    )
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  expect_identical(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("age", "famsize", "inctot", "transit_time")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "correlation"))
  
  expect_error(
    acs_roadmap_nw %>%
      add_sequence_numeric(
        dplyr::where(is.numeric), 
        method = "correlation", 
        cor_var = "not_in_conf_data"
      ),
    regexp = "`cor_var` isn't in conf_data",
    fixed = TRUE
  )
  
})

test_that("visit_sequence correlation order with weight_var", {
  
  nr <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt) %>%
    add_sequence_numeric(
      dplyr::where(is.numeric), 
      method = "correlation", 
      cor_var = "famsize"
    )
  
  expect_s3_class(acs_roadmap_w[["visit_sequence"]], "visit_sequence")
  
  expect_identical(
    nr[["visit_sequence"]][["visit_sequence"]][1:5], 
    c("famsize", "age", "inctot", "transit_time", "wgt")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:5] == "correlation"))
  
  nr2 <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(
      dplyr::where(is.numeric), 
      method = "correlation", 
      cor_var = "famsize"
    )
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  expect_identical(
    nr[["visit_sequence"]][["visit_sequence"]][1:5], 
    c("famsize", "age", "inctot", "transit_time", "wgt")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:5] == "correlation"))
  
  expect_error(
    acs_roadmap_w %>%
      update_visit_sequence(weight_var = not_in_vs) %>%
      add_sequence_numeric(
        dplyr::where(is.numeric), 
        method = "correlation", 
        cor_var = "famsize"
      ),
    regexp = "`weight_var` isn't in conf_data",
    fixed = TRUE
  )
  
})

# proportion --------------------------------------------------------------
test_that("visit_sequence proportion order", {
  
  nr <- acs_roadmap_nw %>%
    add_sequence_numeric(where(is.numeric), method = "proportion")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("famsize", "age", "inctot", "transit_time")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "proportion"))
  
  expect_error(
    acs_roadmap_nw %>%
      add_sequence_numeric(where(is.numeric), 
                           method = "proportion",
                           cor_var = "famsize"),
    regexp = "`cor_var` is unnecessary if method is not 'correlation'",
    fixed = TRUE
  )
  
})

test_that("visit_sequence proportion order with a weight variable", {
  
  nr <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt) %>%
    add_sequence_numeric(where(is.numeric), method = "proportion")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:5], 
    c("famsize", "wgt", "age", "inctot", "transit_time")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:5] == "proportion"))
  
  nr2 <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(where(is.numeric), method = "proportion")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  expect_equal(
    nr2[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("famsize", "age",  "inctot", "transit_time")
  )
  
  expect_true(all(
    nr2[["visit_sequence"]][["visit_method"]][1:4] == "proportion"))
  
})

# weighted total ----------------------------------------------------------
test_that("visit_sequence weighted total", {
  
  nr <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "weighted total")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("inctot", "age", "transit_time", "famsize")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "weighted total"))
  
  nr2 <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = TRUE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "weighted total")
  
  expect_s3_class(nr2[["visit_sequence"]], "visit_sequence")
  expect_equal(
    nr2[["visit_sequence"]][["visit_sequence"]][1:5], 
    c("inctot", "age", "transit_time", "famsize", "wgt")
  )
  
  expect_true(all(
    nr2[["visit_sequence"]][["visit_method"]][1:5] == "weighted total"))
  
  expect_error(
    acs_roadmap_w %>%
      add_sequence_numeric(
        dplyr::where(is.numeric), 
        method = "weighted total"
      ),
    regexp = "One of the weighted methods is specified but weight_var is NULL",
    fixed = TRUE
  )
  
})

test_that("visit_sequence weighted total with weight in start_data", {
  
  nr <- acs_roadmap_w2 %>%
    update_visit_sequence(weight_var = wgt, 
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "weighted total")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("inctot", "age", "transit_time", "famsize")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "weighted total"))
  
})

# absolute weighted total -------------------------------------------------
test_that("visit_sequence absolute weighted total", {
  
  nr <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt, 
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "absolute weighted total")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("inctot", "age", "transit_time", "famsize")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "absolute weighted total"))
  
  nr2 <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt, 
                          synthesize_weight = TRUE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "absolute weighted total")
  
  expect_s3_class(nr2[["visit_sequence"]], "visit_sequence")
  
  expect_equal(
    nr2[["visit_sequence"]][["visit_sequence"]][1:5], 
    c("inctot", "age", "transit_time", "famsize", "wgt")
  )
  
  expect_true(all(
    nr2[["visit_sequence"]][["visit_method"]][1:5] == "absolute weighted total"))
  
})

test_that("visit_sequence absolute weighted total with weight in start_data", {
  
  nr <- acs_roadmap_w2 %>%
    update_visit_sequence(weight_var = wgt, 
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "absolute weighted total")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("inctot", "age", "transit_time", "famsize")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "absolute weighted total"))
  
})

# weighted absolute total -------------------------------------------------
test_that("visit_sequence weighted absolute total", {
  
  nr <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt, 
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "weighted absolute total")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c( "inctot", "age", "transit_time", "famsize")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "weighted absolute total"))
  
  nr2 <- acs_roadmap_w %>%
    update_visit_sequence(weight_var = wgt, 
                          synthesize_weight = TRUE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "weighted absolute total")
  
  expect_s3_class(nr2[["visit_sequence"]], "visit_sequence")
  
  expect_equal(
    nr2[["visit_sequence"]][["visit_sequence"]][1:5], 
    c("inctot", "age", "transit_time", "famsize", "wgt")
  )
  
  expect_true(all(
    nr2[["visit_sequence"]][["visit_method"]][1:5] == "weighted absolute total"))
  
})

test_that("visit_sequence weighted absolute total with weight in start data", {
  
  nr <- acs_roadmap_w2 %>%
    update_visit_sequence(weight_var = wgt, 
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric), 
                         method = "weighted absolute total")
  
  expect_s3_class(nr[["visit_sequence"]], "visit_sequence")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:4], 
    c("inctot", "age", "transit_time", "famsize")
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:4] == "weighted absolute total"))
  
})

# sequence_factor (gini) --------------------------------------------------
test_that("visit_sequence with factor sequence (gini)", {
  
  nr <- acs_roadmap_nw %>%
    add_sequence_factor(dplyr::where(is.factor), method = "gini")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("empstat", "hcovany", "classwkr") 
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:3] == "gini"))
  
})

test_that("visit_sequence with factors and dropping weight (gini)", {
  
  nr <- acs_roadmap_nw %>%
    update_visit_sequence(synthesize_weight = FALSE, 
                          weight_var = wgt) %>%
    add_sequence_factor(dplyr::where(is.factor), method = "gini")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("empstat", "hcovany", "classwkr") 
  )
  
  expect_false("wgt" %in% nr[["visit_sequence"]][["visit_sequence"]])
  
})

test_that("visit_sequence with factors and keeping weight (gini)", {
  
  nr <- acs_roadmap_w %>%
    update_visit_sequence(synthesize_weight = FALSE, 
                          weight_var = wgt) %>%
    add_sequence_factor(dplyr::where(is.factor), method = "gini")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("empstat", "hcovany", "classwkr") 
  )
  
  expect_false("wgt" %in% nr[["visit_sequence"]][["visit_sequence"]])
  
})

# sequence_factor (entropy) ------------------------------------------------
test_that("visit_sequence with factor sequence (entropy)", {
  
  nr <- acs_roadmap_nw %>%
    add_sequence_factor(dplyr::where(is.factor), method = "entropy")
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("empstat", "hcovany", "classwkr") 
  )
  
  expect_true(all(
    nr[["visit_sequence"]][["visit_method"]][1:3] == "entropy"))
  
})

test_that("visit_sequence with factors and dropping weight", {
  
  nr <- acs_roadmap_nw %>%
    update_visit_sequence(synthesize_weight = FALSE, 
                          weight_var = wgt) %>%
    add_sequence_factor(dplyr::where(is.factor))
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("empstat", "hcovany", "classwkr") 
  )
  
  expect_false("wgt" %in% nr[["visit_sequence"]][["visit_sequence"]])
  
})

test_that("visit_sequence with factors and keeping weight (entropy)", {
  
  nr <- acs_roadmap_w %>%
    update_visit_sequence(synthesize_weight = FALSE, 
                          weight_var = wgt) %>%
    add_sequence_factor(dplyr::where(is.factor))
  
  expect_equal(
    nr[["visit_sequence"]][["visit_sequence"]][1:3], 
    c("empstat", "hcovany", "classwkr") 
  )
  
  expect_false("wgt" %in% nr[["visit_sequence"]][["visit_sequence"]])
  
})

test_that("visit_sequence weighted method with weight_var = NULL throws error", {
  expect_error(
    acs_roadmap_w %>%
      add_sequence_numeric(everything(), method = "weighted absolute total"),
    regexp = "Only default and manual methods are supported for numeric \n      data with NA unless na.rm = TRUE",
    fixed = TRUE
  )
})


test_that("error when synthesizing weight when the weight is in the start_data", {
  
  expect_error(
    roadmap(conf_data = acs_conf_nw, start_data = acs_start) %>%
      update_visit_sequence(weight_var = wgt) %>%
      add_sequence_numeric(dplyr::where(is.numeric), 
                           method = "absolute weighted total"),
    regexp = "Only default and manual methods are supported for numeric \n      data with NA unless na.rm = TRUE",
    fixed = TRUE
  )
  
})

# miscellaneous API tests

test_that("cannot rebuild built sequence", {
  
  nr <- acs_roadmap_nw %>%
    add_sequence_numeric(where(is.numeric), method = "proportion")
  
  expect_error(
    update_visit_sequence(nr, weight_var = wgt),
    regexp = "Cannot update_visit_sequence(roadmap, ...) if sequence already built. \n         Please call reset_visit_sequence(roadmap) first.",
    fixed = TRUE
  )
    
})

test_that("reset_built_sequence", {
  
  old_rmap <- acs_roadmap_nw %>%
    add_sequence_numeric(where(is.numeric), method = "proportion")
  
  new_rmap <- reset_visit_sequence(old_rmap)
  
  expect_equal(
    names(new_rmap$visit_sequence$visit_sequence),
    names(new_rmap$schema$synth_vars)
  )
  
})


# missing data tests

test_that("add_sequence_numeric() correlation with NA", {
  
  # expect failure without na.rm == TRUE
  expect_error(
    acs_roadmap_na %>%
      add_sequence_numeric(dplyr::where(is.numeric),
                           method = "correlation",
                           cor_var = "age"),
    regexp = "Only default and manual methods are supported for numeric \n      data with NA unless na.rm = TRUE",
    fixed = TRUE
  )
  
  # use na.rm == TRUE and confirm results
  new_rmap <- acs_roadmap_na %>%
    add_sequence_numeric(dplyr::where(is.numeric),
                         method = "correlation",
                         cor_var = "age",
                         na.rm = TRUE)
  
  expect_equal(
    new_rmap$visit_sequence$visit_method, rep("correlation", 2)
  )
  
  expect_equal(
    new_rmap$visit_sequence$visit_sequence, c("transit_time", "inctot")
  )
  
})

test_that("add_sequence_numeric() proportion with NA", {
  
  # expect failure without na.rm == TRUE
  expect_error(
    acs_roadmap_na %>%
      add_sequence_numeric(dplyr::where(is.numeric),
                           method = "proportion"),
    regexp = "Only default and manual methods are supported for numeric \n      data with NA unless na.rm = TRUE",
    fixed = TRUE
  )
  
  # use na.rm == TRUE and confirm results
  new_rmap <- acs_roadmap_na %>%
    add_sequence_numeric(dplyr::where(is.numeric),
                         method = "proportion",
                         na.rm = TRUE)
  
  expect_equal(
    new_rmap$visit_sequence$visit_method, rep("proportion", 2)
  )
  
  expect_equal(
    new_rmap$visit_sequence$visit_sequence, c("inctot", "transit_time")
  )
  
})

test_that("add_sequence_numeric() weighted total with NA", {
  
  # expect failure without na.rm == TRUE
  expect_error(
    acs_roadmap_na %>%
      update_visit_sequence(weight_var = wgt,
                            synthesize_weight = FALSE) %>%
      add_sequence_numeric(dplyr::where(is.numeric),
                           method = "weighted total"),
    regexp = "Only default and manual methods are supported for numeric \n      data with NA unless na.rm = TRUE",
    fixed = TRUE
  )
  
  # use na.rm == TRUE and confirm results
  new_rmap <- acs_roadmap_na %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric),
                         method = "weighted total",
                         na.rm = TRUE)
  
  expect_equal(
    new_rmap$visit_sequence$visit_method, rep("weighted total", 2)
  )
  
  expect_equal(
    new_rmap$visit_sequence$visit_sequence, c("inctot", "transit_time")
  )
  
})


test_that("add_sequence_numeric() absolute weighted total with NA", {
  
  # expect failure without na.rm == TRUE
  expect_error(
    acs_roadmap_na %>%
      update_visit_sequence(weight_var = wgt,
                            synthesize_weight = FALSE) %>%
      add_sequence_numeric(dplyr::where(is.numeric),
                           method = "absolute weighted total"),
    regexp = "Only default and manual methods are supported for numeric \n      data with NA unless na.rm = TRUE",
    fixed = TRUE
  )
  
  # use na.rm == TRUE and confirm results
  new_rmap <- acs_roadmap_na %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric),
                         method = "absolute weighted total",
                         na.rm = TRUE)
  
  expect_equal(
    new_rmap$visit_sequence$visit_method, rep("absolute weighted total", 2)
  )
  
  expect_equal(
    new_rmap$visit_sequence$visit_sequence, c("inctot", "transit_time")
  )
  
})

test_that("add_sequence_numeric() weighted absolute total with NA", {
  
  # expect failure without na.rm == TRUE
  expect_error(
    acs_roadmap_na %>%
      update_visit_sequence(weight_var = wgt,
                            synthesize_weight = FALSE) %>%
      add_sequence_numeric(dplyr::where(is.numeric),
                           method = "weighted absolute total"),
    regexp = "Only default and manual methods are supported for numeric \n      data with NA unless na.rm = TRUE",
    fixed = TRUE
  )
  
  # use na.rm == TRUE and confirm results
  new_rmap <- acs_roadmap_na %>%
    update_visit_sequence(weight_var = wgt,
                          synthesize_weight = FALSE) %>%
    add_sequence_numeric(dplyr::where(is.numeric),
                         method = "weighted absolute total",
                         na.rm = TRUE)
  
  expect_equal(
    new_rmap$visit_sequence$visit_method, rep("weighted absolute total", 2)
  )
  
  expect_equal(
    new_rmap$visit_sequence$visit_sequence, c("inctot", "transit_time")
  )
  
})



