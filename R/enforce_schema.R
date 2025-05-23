
.sum_to_cast_categorical <- function(col, typesum, levels = NULL) {
  
  if (typesum == "lgl") {
    
    return(vctrs::vec_cast(col, base::logical()))
    
  } else if (typesum == "chr") {
    
    return(vctrs::vec_cast(col, base::character()))
    
  } else if (typesum == "fct") {
    
    if (is.null(levels)) {
      
      return(base::factor(col)) 
      
    } else {
      
      return(base::factor(col, levels = levels))
      
    }
    
  } else {
    
    stop("Cannot enforce_schema() with unsupported categorical type: ", typesum)
    
  }
  
}

.sum_to_cast_numeric <- function(col, typesum) {
  
  if (typesum == "dbl") {
    
    return(vctrs::vec_cast(col, base::double()))
    
  } else if (typesum == "int") {
    
    return(vctrs::vec_cast(col, base::integer()))
    
  } else {
    
    stop("Cannot enforce_schema() with unsupported numeric type: ", typesum)
    
  }
  
}

#'
#' Enforce a `roadmap`'s `schema` on its existing data
#' 
#' @param roadmap A `roadmap` object
#' 
#' @return A `roadmap` object with modified `conf_data`, `start_data`, and `schema`
#' information. 
#'
#' @export
#' 
enforce_schema <- function(roadmap) {
  
  # create copies of the data for modification
  conf_data <- roadmap[["conf_data"]]
  start_data <- roadmap[["start_data"]]
  col_schema <- roadmap[["schema"]][["col_schema"]]
  dtypes <- purrr::map_chr(
    .x = roadmap[["schema"]][["col_schema"]], .f = \(x) { x[["dtype"]] })
  
  # first, user-specified type casting overrides
  numeric_cols <- col_schema[dtypes %in% c("int", "dbl")]
  
  if (roadmap[["schema"]][["coerce_to_doubles"]]) {
    # if coerce_to_doubles, set all numeric columns to doubles
    
    for (nc in names(numeric_cols)) {
      
      conf_data <- conf_data %>% 
        dplyr::mutate(dplyr::across(dplyr::all_of(c(nc)), 
                                    as.double))
      
      if (nc %in% names(start_data)) {
        start_data <- start_data %>% 
          dplyr::mutate(dplyr::across(dplyr::all_of(c(nc)), 
                                      as.double))
      }
      
      col_schema[[nc]][["dtype"]] <- "dbl"
      
    }
    
  } else {
    
    # else, cast according to user-specified types
    for (nc in names(numeric_cols)) {
      
      conf_data <- conf_data %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(c(nc)),
            \(x) { .sum_to_cast_numeric(x, col_schema[[nc]][["dtype"]])  }
          )
        )
      
      if (nc %in% names(start_data)) {
        
        start_data <- start_data %>%
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(c(nc)),
              \(x) { .sum_to_cast_numeric(x, col_schema[[nc]][["dtype"]]) }
            )
          )
        
      }
    }
    
  }
  
  factor_cols <- col_schema[dtypes %in% c("chr", "lgl", "fct")]
  
  if (roadmap[["schema"]][["coerce_to_factors"]]) {
    # if coerce_to_factors, set all categorical columns to factors
    
    for (fc in names(factor_cols)) {
      
      # if specified factors provided, apply them
      if (!is.null(col_schema[[fc]][["levels"]])) {
        
        # apply factor levels to conf_data
        conf_data <- conf_data %>% 
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(c(fc)), 
              ~ factor(.x, levels = col_schema[[fc]][["levels"]])
            )
          )
        
        # apply to start_data if column exists in it
        if (fc %in% names(start_data)) {
          start_data <- start_data %>% 
            dplyr::mutate(
              dplyr::across(
                dplyr::all_of(c(fc)), 
                ~ factor(.x, levels = col_schema[[fc]][["levels"]])
              )
            )
        }
        
        # update col_schema dtype in all scenarios
        col_schema[[fc]][["dtype"]] <- "fct"
        
      } else {
        
        # apply factor levels to conf_data
        conf_data <- conf_data %>% 
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(c(fc)), 
              factor
            )
          )
        
        conf_levels <- levels(dplyr::pull(conf_data, fc))
        
        # apply to start_data if column exists in it
        if (fc %in% names(start_data)) {
          start_data <- start_data %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::all_of(c(fc)), 
                ~ factor(.x, levels = conf_levels)
              )
            )
        }
        # update col_schema
        col_schema[[fc]][["dtype"]] <- "fct"
        col_schema[[fc]][["levels"]] <- conf_levels
      }
      
    }
    
  } else {
    
    # else, cast according to user-specified types
    for (fc in names(factor_cols)) {
      
      conf_data <- conf_data %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(c(fc)),
            \(x) {
              .sum_to_cast_categorical(
                x, 
                typesum = col_schema[[fc]][["dtype"]],
                levels = col_schema[[fc]][["levels"]])
            }
          )
        )
      
      if (fc %in% names(start_data)) {
        
        start_data <- start_data %>%
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(c(fc)),
              \(x) {
                .sum_to_cast_categorical(
                  x, 
                  typesum = col_schema[[fc]][["dtype"]],
                  levels = col_schema[[fc]][["levels"]])
              }
            )
          )
        
      }
    }
    
  }
  
  # next, missing data handling 
  # first, if using custom_na values, enforce them
  conf_data <- enforce_custom_na(conf_data, col_schema)
  start_data <- enforce_custom_na(start_data, col_schema)
  
  # recalculate NA percentages and update col_schema
  new_na_props <- purrr::map(.x = conf_data,
                             .f = \(x) { mean(is.na(x)) } )
  
  for (col in names(col_schema)) {
    
    col_schema[[col]][["na_prop"]] <- new_na_props[[col]]
    
  }
  
  # if flagged, add indicator variables for missingness in numeric variables
  if (roadmap[["schema"]][["na_numeric_to_ind"]]) {
    
    # expand numeric variables with missing data to include _NA variables that 
    # reflect the pattern of missingness in the variables
    conf_data <- expand_na(data = conf_data, 
                           types = c("int", "dbl"),
                           skip_vars = names(start_data))
    
  }
  
  # if flagged, convert missing factor values into a new factor level
  if (roadmap[["schema"]][["na_factor_to_level"]]) {
    
    conf_data <- convert_na_to_level(data = conf_data)
    
  }
  
  # finally, update roadmap and schema
  roadmap[["conf_data"]] <- conf_data
  roadmap[["start_data"]] <- start_data
  
  # update synth_vars with names of new NA variables
  synth_vars <- setdiff(names(conf_data), names(start_data))
  
  # insert NA synth_vars into visit_sequence
  vs <- roadmap$visit_sequence$visit_sequence
  vm <- roadmap$visit_sequence$visit_method
  
  # first, get variable names with and without NA values and their indices
  na_vars <- synth_vars[endsWith(synth_vars, "_NA")]
  orig_na_vars <- purrr::map_chr(
    .x = na_vars, 
    .f = \(x) { stringr::str_replace(x, '_NA', '') } 
  )
  orig_vms <- purrr::map_chr(
    .x = orig_na_vars,
    .f = \(x) { vm[[which(!is.na(match(vs, x)))]] }
  )
  
  # for each NA variable
  for (i in seq_along(na_vars)) {
    
    # find index at which to insert indicator
    insert_ix <- which(!is.na(match(vs, orig_na_vars[[i]])))
    
    # if indicator at beginning...
    if (insert_ix == 1) {
      
      vs <- c(na_vars[[i]], vs)
      vm <- c(orig_vms[[i]], vm)
    
    # else if indicator at the end...
    } else if (insert_ix == length(vs)) {
      
      vs <- c(vs[1:insert_ix - 1], na_vars[[i]], vs[insert_ix])
      vm <- c(vm[1:insert_ix - 1], orig_vms[[i]], vm[insert_ix])
    
    # else indicator in the middle...
    } else {
      
      vs <- c(vs[1:insert_ix - 1], na_vars[[i]], vs[insert_ix:length(vs)])
      vm <- c(vm[1:insert_ix - 1], orig_vms[[i]], vm[insert_ix:length(vs)])
      
    }
    
  }
  
  # update original roadmap
  roadmap$visit_sequence$visit_sequence <- vs 
  roadmap$visit_sequence$visit_method <- vm
  
  # update no_variation variables
  no_variation <- conf_data %>%
    dplyr::select(dplyr::all_of(synth_vars)) %>%
    purrr::map_lgl(.f = ~ length(unique(.x)) == 1)
  
  # update schema using API call
  roadmap <- update_schema(roadmap, 
                           col_schema = col_schema,
                           synth_vars = synth_vars,
                           no_variation = no_variation)
  
  return(roadmap)
  
}
