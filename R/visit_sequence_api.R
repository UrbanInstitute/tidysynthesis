#' 
#' Add or reset a `visit_sequence` object within an existing `roadmap`.
#'
#' @param roadmap A `roadmap` object
#' @param visit_sequence A `visit_sequence` object. 
#' @param ... Optional additional parameters.
#' 
#' @return A new `roadmap` object.
#'
#' @name visit_sequence_api
#'
NULL
#> NULL


#'
#' @rdname visit_sequence_api
#' @export 
#' 
add_visit_sequence <- function(roadmap, visit_sequence) {
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`visit_sequence` must be a visit_sequence object" = { 
      is_visit_sequence(visit_sequence)
    }
  ) 
  
  roadmap[["visit_sequence"]] <- visit_sequence
  
  # update col_schema ordering to reflect visit_sequence
  roadmap[["schema"]][["col_schema"]] <- 
    roadmap[["schema"]][["col_schema"]][
      c(names(roadmap[["start_data"]]), visit_sequence[["visit_sequence"]])]
  
  return(roadmap)
  
}

#'
#' @rdname visit_sequence_api
#' @export
#' 
update_visit_sequence <- function(roadmap, ...) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  # only allow updates to sequences that haven't started builds
  if (!is.null(roadmap[["visit_sequence"]][["built_sequence"]])) {
    
    stop("Cannot update_visit_sequence(roadmap, ...) if sequence already built. 
         Please call reset_visit_sequence(roadmap) first.")
    
  }
  
  # update visit_sequence parameters
  kwargs <- rlang::quos(...) 
  if ("weight_var" %in% names(kwargs)) {
    
    roadmap[["visit_sequence"]][["weight_var"]] <- kwargs[["weight_var"]]
    
  } 
  
  if ("synthesize_weight" %in% names(kwargs)) {
    
    synthesize_weight <- rlang::eval_tidy(kwargs[["synthesize_weight"]])
    
    roadmap[["visit_sequence"]][["synthesize_weight"]] <- synthesize_weight
    
    if (!synthesize_weight) {
      
      default_sequence <- roadmap[["schema"]][["synth_vars"]][
        roadmap[["schema"]][["synth_vars"]] != rlang::as_label(
          roadmap[["visit_sequence"]][["weight_var"]]
        )
      ]
      
      new_vs <- list(
        default_sequence = default_sequence,
        built_sequence = NULL,
        visit_sequence = default_sequence,
        visit_method = rep("default", length(default_sequence)),
        synthesize_weight = synthesize_weight,
        weight_var = kwargs[["weight_var"]]
      )
      
      new_vs <- structure(new_vs, class = "visit_sequence")
      
      roadmap[["visit_sequence"]] <- new_vs
      
    }
    
  }
  
  return(roadmap)
  
}

#'
#' @rdname visit_sequence_api
#' @export 
#' 
reset_visit_sequence <- function(roadmap) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  roadmap[["visit_sequence"]] <- visit_sequence(schema = roadmap[["schema"]])
  
  # update col_schema ordering to reflect visit_sequence
  roadmap[["schema"]][["col_schema"]] <- 
    roadmap[["schema"]][["col_schema"]][
      c(names(roadmap[["start_data"]]), 
        roadmap[["visit_sequence"]][["visit_sequence"]])]
  
  return(roadmap)
  
}


#' 
#' Add to visit sequence using a manual method
#'
#' @param roadmap A `roadmap` object.
#' @param ... <tidy-select> One or more unquoted expressions separated by 
#' commas. Variable names can be used as if they were positions in the data 
#' frame, so expressions like x:y can be used to select a range of variables.
#'
#' @return An updated `roadmap` object.
#' 
#' @export
#' 
add_sequence_manual <- function(roadmap, ...) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  # unpack visit_sequence
  visit_sequence <- roadmap[["visit_sequence"]]
  
  # use tidyselect to select and reorder desired variables
  manual_order <- roadmap[["conf_data"]] %>%
    dplyr::select(...) %>%
    names()
  
  # add sequence method to existing vector of sequence methods
  start_index <- length(visit_sequence[["built_sequence"]]) + 1
  end_index <- length(visit_sequence[["built_sequence"]]) + length(manual_order)
  
  visit_sequence[["visit_method"]][start_index:end_index] <- "manual"
  
  # add manual sequence to existing built sequence
  visit_sequence[["built_sequence"]] <- c(visit_sequence[["built_sequence"]], manual_order)
  
  # add new built sequence to existing visit sequence
  visit_sequence[["visit_sequence"]] <- c(
    visit_sequence[["built_sequence"]],
    setdiff(visit_sequence[["default_sequence"]], visit_sequence[["built_sequence"]])
  )
  
  return(add_visit_sequence(roadmap, visit_sequence))
  
}

#'
#' Add to visit sequence for numeric variables
#'
#' @param roadmap A `roadmap` object
#' @param ... <tidy-select> One or more unquoted expressions separated by 
#' commas. Variable names can be used as if they were positions in the data 
#' frame, so expressions like x:y can be used to select a range of variables.
#' @param method A quoted name for the method used to sort the visit_sequence
#' @param cor_var A numeric variable for the correlation method
#' @param na.rm Boolean that if TRUE, removes `NA` values from computations
#' @param cor_use A string correlation data method passed to `stats::cor` if using. 
#' If `na.rm == TRUE` then defaults to `complete.obs`. See `?stats::cor` for
#' more options.
#'
#' @return An updated visit_sequence
#' 
#' @export
#' 
add_sequence_numeric <- function(
    roadmap, 
    ..., 
    method = c("correlation", "proportion", "weighted total", 
               "absolute weighted total", "weighted absolute total"), 
    cor_var = NULL,
    na.rm = FALSE,
    cor_use = "everything"
) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  visit_sequence <- roadmap[["visit_sequence"]]
  
  # get weight variable
  weight_var <- visit_sequence[["weight_var"]]
  
  # subset data used for calculations ---------------------------------------
  conf_data <- roadmap[["conf_data"]]
  
  # calc_data can include start_data and weight_var for calculations
  calc_data <- conf_data %>%
    dplyr::select(..., dplyr::any_of(rlang::quo_name(weight_var)))
  
  # edit_data only include variables that can be changed in the visit_sequence
  edit_data <- dplyr::select(
    calc_data, 
    -dplyr::any_of(names(roadmap[["start_data"]]))
  )
  
  # drop the weight from edit variables if the weight is synthesized
  if (!visit_sequence[["synthesize_weight"]]) {
    
    edit_data <- dplyr::select(
      edit_data, 
      -dplyr::any_of(rlang::quo_name(weight_var))
    )
    
  }
  
  # check inputs ------------------------------------------------------------
  
  # check methods
  method <- match.arg(arg = method)
  
  # throw error if cor_var is included when method is not correlation
  if (method != "correlation" && !is.null(cor_var)) {
    
    stop("`cor_var` is unnecessary if method is not 'correlation'")
    
  }
  
  # throw an error if target data contain NA and unsupported method is specified
  
  contains_na <- roadmap[["conf_data"]] %>%
    dplyr::select(..., dplyr::any_of(rlang::quo_name(weight_var))) %>%
    purrr::map_lgl(.f = \(x) any(is.na(x))) %>%
    any()
  
  if (contains_na & 
      !method %in% c("manual", "default") & 
      !na.rm) {
    
    stop("Only default and manual methods are supported for numeric 
      data with NA unless na.rm = TRUE")
    
  }
  
  # throw error if weight_var is missing
  if (rlang::quo_name(weight_var) != "NULL" &&
      !rlang::quo_name(weight_var) %in% names(conf_data)
  ) {
    
    stop("`weight_var` isn't in conf_data")
    
  }
  
  if (method %in% c("weighted total",
                    "absolute weighted total",
                    "weighted absolute total") &&
      rlang::quo_name(weight_var) == "NULL") {
    
    stop("One of the weighted methods is specified but weight_var is NULL")
    
  }
  
  # add sequence method to existing vector of sequence methods
  start_index <- length(visit_sequence[["built_sequence"]]) + 1
  end_index <- length(visit_sequence[["built_sequence"]]) + ncol(edit_data)
  
  visit_sequence[["visit_method"]][start_index:end_index] <- method
  
  # apply numeric method
  if (method == "correlation") {
    
    # throw error if cor_var is missing
    if (!cor_var %in% names(conf_data)) stop("`cor_var` isn't in conf_data")
    
    # if no cor_use specified...
    if(na.rm & cor_use == "everything") {
      
      # use complete observations
      cor_use <- "complete.obs"
      
    }
    
    cor_order <- calc_data %>%
      stats::cor(use = cor_use) %>%
      tibble::as_tibble(rownames = "var") %>%
      dplyr::select("var", cors = dplyr::any_of(cor_var)) %>%
      dplyr::mutate(cors = abs(.data$cors)) %>%
      dplyr::arrange(dplyr::desc(.data$cors)) %>%
      dplyr::pull("var")
    
    # add correlation sequence sequence to existing built sequence
    visit_sequence[["built_sequence"]] <- 
      c(visit_sequence[["built_sequence"]], cor_order)
    
  } else if (method == "proportion") {
    
    prop_order <- calc_data %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::everything(), 
          .fns = ~ as.numeric(.x != 0)
        )
      ) %>%
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::everything(), 
          .fns = ~ mean(.x, na.rm = na.rm)
        )
      ) %>%
      tidyr::gather(key = "variable", value = "prop") %>%
      dplyr::arrange(dplyr::desc(.data$prop)) %>%
      dplyr::pull("variable")
    
    # add prop sequence to existing built sequence
    visit_sequence[["built_sequence"]] <- 
      c(visit_sequence[["built_sequence"]], prop_order)
    
  } else if (method == "weighted total") {
    
    weighted_total_order <- calc_data %>%
      dplyr::summarize(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~sum(.x * !!weight_var, na.rm = na.rm)
        )
      ) %>%
      tidyr::gather(key = "variable", value = "weighted_sum") %>%
      dplyr::arrange(dplyr::desc(.data$weighted_sum)) %>%
      dplyr::pull("variable")
    
    # add weighted total sequence sequence to existing built sequence
    visit_sequence[["built_sequence"]] <- 
      c(visit_sequence[["built_sequence"]], weighted_total_order)
    
  } else if (method == "absolute weighted total") {
    
    weighted_total_order <- calc_data %>%
      dplyr::summarize(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ abs(sum(.x * !!weight_var, na.rm = na.rm))
        )
      ) %>%
      tidyr::gather(key = "variable", value = "weighted_sum") %>%
      dplyr::arrange(dplyr::desc(.data$weighted_sum)) %>%
      dplyr::pull("variable")
    
    # add absolute weighted total sequence sequence to existing built sequence
    visit_sequence[["built_sequence"]] <- 
      c(visit_sequence[["built_sequence"]], weighted_total_order)
    
  } else if (method == "weighted absolute total") {
    
    weighted_total_order <- calc_data %>%
      dplyr::summarize(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ sum(abs(.x) * !!weight_var, na.rm = na.rm)
        )
      ) %>%
      tidyr::gather(key = "variable", value = "weighted_sum") %>%
      dplyr::arrange(dplyr::desc(.data$weighted_sum)) %>%
      dplyr::pull("variable")
    
    # add absolute weighted total sequence sequence to existing built sequence
    visit_sequence[["built_sequence"]] <- 
      c(visit_sequence[["built_sequence"]], weighted_total_order)
    
  }
  
  # remove start variables (unless the start variable is weight and weight is synthesized)
  drop_vars <- names(roadmap[["start_data"]])
  drop_vars <- drop_vars[drop_vars != rlang::quo_name(weight_var)]
  visit_sequence[["built_sequence"]] <- 
    visit_sequence[["built_sequence"]][!visit_sequence[["built_sequence"]] %in% drop_vars]
  
  # remove weight if weight isn't synthesized
  if (!visit_sequence[["synthesize_weight"]] & rlang::quo_name(weight_var) != "NULL") {
    
    visit_sequence[["built_sequence"]] <- 
      visit_sequence[["built_sequence"]][visit_sequence[["built_sequence"]] != rlang::quo_name(weight_var)]
    
  }
  
  # add new built sequence to existing visit sequence
  visit_sequence[["visit_sequence"]] <- c(
    visit_sequence[["built_sequence"]],
    setdiff(visit_sequence[["default_sequence"]], visit_sequence[["built_sequence"]])
  )
  
  return(add_visit_sequence(roadmap, visit_sequence))
  
}

#' 
#' Add to visit sequence for factor variables
#'
#' @param roadmap A `roadmap` object
#' @param ... <tidy-select> One or more unquoted expressions separated by 
#' commas. Variable names can be used as if they were positions in the data 
#' frame, so expressions like x:y can be used to select a range of variables.
#' @param method A quoted name for the method used to sort the visit_sequence
#'
#' @return An updated visit_sequence
#' 
#' @export 
#' 
add_sequence_factor <- function(
    roadmap, 
    ..., 
    method = c("entropy")
) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  visit_sequence <- roadmap[["visit_sequence"]]
  
  # check method
  method <- match.arg(arg = method)
  
  # get weight variable
  weight_var <- visit_sequence[["weight_var"]]
  
  # calc_data can include start_data and weight_var for calculations
  calc_data <- roadmap[["conf_data"]] %>%
    dplyr::select(dplyr::any_of(roadmap[["schema"]][["synth_vars"]]),
                  dplyr::any_of(rlang::quo_name(weight_var))) %>%
    dplyr::select(..., dplyr::any_of(rlang::quo_name(weight_var)))
  
  # edit_data only include variables that can be changed in the visit_sequence
  edit_data <- dplyr::select(
    calc_data, 
    -dplyr::any_of(names(roadmap[["schema"]][["start_data"]]))
  )
  
  # drop the weight from edit variables if the weight is synthesized
  if (!visit_sequence[["synthesize_weight"]]) {
    
    edit_data <- dplyr::select(
      calc_data, 
      -dplyr::any_of(rlang::quo_name(weight_var))
    )
    
  }
  
  # add sequence method to existing vector of sequence methods
  start_index <- length(visit_sequence[["built_sequence"]]) + 1
  end_index <- length(visit_sequence[["built_sequence"]]) + ncol(edit_data)
  
  visit_sequence[["visit_method"]][start_index:end_index] <- method
  
  if (method == "entropy") {
    
    calc_entropy <- function(x) {
      
      p_i <- unname(prop.table(table(x)))
      
      -sum(p_i * log(p_i, base = 2))
      
    }
    
    entropy <- purrr::map_dbl(edit_data, .f = calc_entropy)
    
    entropy <- sort(entropy)
    
    entropy_order <- names(entropy)
    
    # add prop sequence to existing built sequence
    visit_sequence[["built_sequence"]] <- 
      c(visit_sequence[["built_sequence"]], entropy_order)
    
  }
  
  # remove start variables (unless the start variable is weight and weight is synthesized)
  drop_vars <- names(roadmap[["start_data"]])
  visit_sequence[["built_sequence"]] <- 
    visit_sequence[["built_sequence"]][!visit_sequence[["built_sequence"]] %in% drop_vars]
  
  # add new built sequence to existing visit sequence
  visit_sequence[["visit_sequence"]] <- c(
    visit_sequence[["built_sequence"]],
    setdiff(visit_sequence[["default_sequence"]], visit_sequence[["built_sequence"]])
  )
  
  return(add_visit_sequence(roadmap, visit_sequence))
  
}
