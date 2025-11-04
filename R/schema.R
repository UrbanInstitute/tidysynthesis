#' Generate a `schema` object.
#'
#' @param conf_data A data frame to be synthesized.
#' @param start_data A data frame with starting variables.
#' @param col_schema An optional named list of columns in the confidential data 
#' with their properties, including data type and factor levels. If NULL or only 
#' partially specified, `col_schema` will be inferred from the confidential data.
#' See example code for formatting.
#' @param enforce Boolean that if true, will preprocess both `conf_data` and `start_data`
#' to align with `col_schema` and the arguments below. 
#' @param coerce_to_factors Boolean that if true, coerces categorical data types 
#' (`chr`, `fct`, `ord`) to base `R` factors when `enforce_schema` is called.
#' @param coerce_to_doubles Boolean that if true, coerces columns specified as `dbl`
#' in `col_schema` to base `R` doubles when `enforce_schema` is called.
#' @param na_factor_to_level Boolean that if true, applies `convert_level_to_na()`
#' to factor variables when `enforce_schema` is called.
#' @param na_numeric_to_ind Boolean that if true, applies `expand_na()` to numeric data
#' to create logical missingness indicators when `enforce_schema` is called.
#'
#' @return A `schema` object.
#'
#' @examples
#' 
#' conf_data <- data.frame(
#'   var1 = c("1", "1", "2"),
#'   var2 = c(1L, 2L, 3L),
#'   var3 = c(1.1, 2.2, 3.3)
#' )
#' 
#' start_data <- dplyr::select(conf_data, var1)
#' 
#' # default inferred schema
#' schema(
#'   conf_data = conf_data,
#'   start_data = start_data
#' )
#' 
#' # overwriting factor levels
#' schema(
#'   conf_data = conf_data,
#'   start_data = start_data,
#'   col_schema = list(
#'     "var1" = list(
#'       "dtype" = "fct",
#'       "levels" = c("1", "2", "3")
#'     )
#'   ),
#'   coerce_to_factors = TRUE
#' )
#' 
#' 
#' @export
schema <- function(conf_data,
                   start_data,
                   col_schema = NULL,
                   enforce = TRUE,
                   coerce_to_factors = FALSE,
                   coerce_to_doubles = FALSE,
                   na_factor_to_level = TRUE, 
                   na_numeric_to_ind = TRUE) {
  
  schema <- new_schema(conf_data = conf_data,
                       start_data = start_data,
                       col_schema = col_schema,
                       enforce = enforce,
                       coerce_to_factors = coerce_to_factors,
                       coerce_to_doubles = coerce_to_doubles,
                       na_factor_to_level = na_factor_to_level, 
                       na_numeric_to_ind = na_numeric_to_ind)
  
  return(schema)

}

new_schema <- function(conf_data,
                       start_data,
                       col_schema = NULL,
                       enforce = TRUE,
                       coerce_to_factors = FALSE,
                       coerce_to_doubles = FALSE,
                       na_factor_to_level = TRUE, 
                       na_numeric_to_ind = TRUE) {
  
  # input type checking
  stopifnot(
    "`conf_data` must be a data.frame" = { is.data.frame(conf_data) },
    "`start_data` must be a data.frame" = { is.data.frame(start_data) },
    "`col_schema`, if supplied, must be a list" = { 
      is.null(col_schema) | is.list(col_schema) 
    },
    "`enforce` must be logical" = { is.logical(enforce) },
    "`coerce_to_factors` must be logical" = { is.logical(coerce_to_factors) },
    "`coerce_to_doubles` must be logical" = { is.logical(coerce_to_doubles) },
    "`na_factor_to_level` must be logical" = { is.logical(na_factor_to_level) },
    "`na_numeric_to_ind` must be logical" = { is.logical(na_numeric_to_ind) }
  )
  
  # infer column schema information
  # first, infer variable names that are being sequentially synthesized
  synth_vars <- setdiff(names(conf_data), names(start_data))
  
  # next, infer variable names with no observed variation
  no_variation <- conf_data %>%
    dplyr::select(dplyr::all_of(synth_vars)) %>%
    purrr::map_lgl(.f = ~ length(unique(.x)) == 1)
  
  dtypes <- conf_data %>% 
    purrr::map(.f = ~ pillar::type_sum(.x))
  
  col_schema_inf <- conf_data %>%
    purrr::map(.f = ~ list("dtype" = pillar::type_sum(.x),
                           "levels" = NULL,
                           "na_value" = NA))
  
  # add factor information 
  factor_cols <- col_schema_inf[dtypes == "fct"]
  for (fc in names(factor_cols)) {
    
    col_schema_inf[[fc]][["levels"]] <- dplyr::pull(conf_data, fc) %>% 
      levels()
    
  }
  
  # apply manual overrides to inferred column schema
  if (!is.null(col_schema)) {
    
    for (col in names(col_schema)) {
      
      for (key in names(col_schema[[col]])) {
        
        col_schema_inf[[col]][[key]] <- col_schema[[col]][[key]]
        
      }
       
      dtypes[[col]] <- col_schema[[col]][["dtype"]]
      
    }
    
  }
  
  # calculate the proportions of values that are missing
  col_schema_na <- conf_data %>%
    purrr::map(.f = ~ mean(is.na(.x)))
  
  for (col in names(col_schema_inf)) {

    col_schema_inf[[col]][["na_prop"]] <- col_schema_na[[col]]
    
  }
  
  # combine everything into the schema object
  schema <- list(
    col_schema = col_schema_inf,
    synth_vars = synth_vars,
    coerce_to_factors = coerce_to_factors,
    coerce_to_doubles = coerce_to_doubles,
    na_factor_to_level = na_factor_to_level, 
    na_numeric_to_ind = na_numeric_to_ind,
    no_variation = no_variation,
    enforce = enforce
  )
  
  schema <- structure(schema, class = "schema")
  
  return(schema)
  
}

is_schema <- function(x) {
  inherits(x, "schema")
}


# validator
validate_schema <- function(roadmap) {
  
  # check that roadmap is supplied
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  # unpack schema arguments
  conf_data <- roadmap[["conf_data"]]
  schema <- roadmap[["schema"]]
  
  # Error checking
  # ensure all col_schema names in conf_data
  extra_schema_names <- !(names(schema[["col_schema"]]) %in% names(conf_data))
  
  if (sum(extra_schema_names) > 0) {
    
    stop("`col_schema` included unknown name(s) ",
         paste0(names(schema$col_schema)[extra_schema_names], collapse = ", "))
    
  }
  
  # ensure all dtypes are supported 1-dimensional tidyverse types
  # source for type list: https://tibble.tidyverse.org/articles/types.html
  invalid_dtypes <- !(
    purrr::map_lgl(
      .x = schema[["col_schema"]], 
      .f = \(col) {
        col[["dtype"]] %in% 
          c("lgl", "int", "dbl", "chr", "fct")
      } 
    )
  )
  
  if (sum(invalid_dtypes) > 0) {
    
    stop("`col_schema` included unsupported dtype(s) ",
         paste0(purrr::map_chr(schema$col_schema, ~ .x[["dtype"]])[invalid_dtypes], 
                collapse = ", "))
    
  }
  
  # ensure no invalid field names
  invalid_names <- !purrr::map_lgl(
    .x = schema$col_schema, 
    .f = \(col) {
      all(names(col) %in% c("dtype", "levels", "na_prop", "na_value"))
    }
  )
  
  if (sum(invalid_names) > 0) {
    stop("Invalid `col_schema` field names for variable(s) ",
         paste0(names(schema$col_schema)[invalid_names], collapse = ", "))
    
  }
  
  # Warning checking 
  # message if any of the factor variables have empty levels
  conf_data_subset <- dplyr::select(conf_data, dplyr::where(is.factor))
  empty_levels_lgl <- purrr::map_lgl(conf_data_subset, ~setequal(levels(.x), unique(.x))) 
  empty_levels_names <- names(empty_levels_lgl)[!empty_levels_lgl]
  
  if (length(empty_levels_names) > 0) {
    
    message(
      "The following factor variables have empty levels: ", 
      paste0(empty_levels_names, collapse = ", "), 
      "\nThe empty levels will not be modeled."
    )
    
  }
  
  # message if any variables have no variation
  if (any(schema[["no_variation"]])) {
    
    message(
      "The following variables have no variation: ", 
      paste0(names(schema[["no_variation"]])[schema[["no_variation"]]], 
             collapse = ", "), 
      "\nThese variables will not be modeled."
    )
    
  }
  
}

#' Print the schema object to the console with formatting
#'
#' @param x A `schema` object
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' 
#' @examples
#' 
#' # default inferred schema
#' schema <- schema(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' )
#' 
#' print(schema)
#' 
#' @export
print.schema <- function(x, ...) {
  
  cat("Schema:", length(x[["col_schema"]]), "columns \n")
  cat(purrr::map_chr(x[["col_schema"]], ~ .x[["dtype"]]))
  
  invisible(x)
  
}


# Tidy API calls -----------------------------------------------------------

#' 
#' Add, update, or reset a `schema` object within an existing `roadmap`.
#'
#' @param roadmap A `roadmap` object
#' @param schema A `schema` object. 
#' @param ... Optional named parameters passed to `schema()`.
#' 
#' @return A new `roadmap` object.
#'
#' @name schema_api
#'
NULL
#> NULL

#'
#' @rdname schema_api
#' 
#' @examples
#' 
#' rm <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' )
#' 
#' acs_schema <- schema(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw,
#'   na_numeric_to_ind = TRUE
#' )
#' 
#' rm |>
#'   add_schema(schema = acs_schema)
#' 
#' @export 
add_schema <- function(roadmap, schema) { 
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`schema` must be a schema object" = { is_schema(schema) }
  )
  
  roadmap[["schema"]] <- schema
  
  return(roadmap)
  
}

#'
#' @rdname schema_api
#' 
#' @examples
#' 
#' rm <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' )
#' 
#' rm |>
#'   update_schema(na_numeric_to_ind = TRUE)
#' 
#' @export 
update_schema <- function(roadmap, ...) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  kwargs <- list(...)
  
  for (name in names(kwargs)) {
    
    if (name == "col_schema") {
      # if col_schema provided...
      
      col_schema_new <- roadmap[["schema"]][["col_schema"]]
      col_schema_args <- kwargs[["col_schema"]]
      
      # for each column in col_schema...
      for (col in names(col_schema_args)) {
        
        # for each key-value pair in the column specification...
        for (key in names(col_schema_args[[col]])) {
          
          # update the underlying roadmap object
          col_schema_new[[col]][[key]] <- col_schema_args[[col]][[key]]
          
        }
        
      }
      
      # update final col_schema
      roadmap[["schema"]][["col_schema"]] <- col_schema_new
      
    } else {
      
      # else, if not col_schema, directly update roadmap
      roadmap[["schema"]][[name]] <- kwargs[[name]]
      
    }
    
  }
  
  return(roadmap)
  
}

#'
#' @rdname schema_api
#' 
#' @examples
#' 
#' rm <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' )
#' 
#' rm <- rm |>
#'   update_schema(na_numeric_to_ind = TRUE)
#' 
#' reset_schema(roadmap = rm)
#'   
#' @export 
reset_schema <- function(roadmap) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  new_schema <- schema(conf_data = roadmap[["conf_data"]],
                       start_data = roadmap[["start_data"]])
  
  roadmap[["schema"]] <- new_schema 
  
  return(roadmap)
  
}


