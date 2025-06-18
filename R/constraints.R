#' 
#' Create a constraints object
#'
#' @param schema A `schema` object
#' @param constraints_df_num A specially formatted data frame with constraints 
#' to be imposed during the synthesis process. See examples for formatting.
#' @param constraints_df_cat A specifically formatted data frame with constraints 
#' to be imposed during the synthesis process.
#' @param max_z_num Numeric vector(s) for the number of times a value should be 
#' resampled before hardbounding if it violates a constraint.
#' @param max_z_cat Numeric vector(s) for the number of times a value should be 
#' resampled before hardbounding if it violates a constraint.
#'
#' @return A `constraints` object.
#' 
#' @examples
#' constraints(
#'   schema = schema(
#'     conf_data = mtcars %>% dplyr::mutate(vs = factor(vs)),
#'     start_data = dplyr::select(mtcars, cyl)
#'   ),
#'   constraints_df_num = tibble::tribble(
#'     ~var, ~min, ~max, ~conditions, 
#'     # ensure all mpg values are greater than 0
#'     "mpg", 0, Inf, "TRUE",
#'     # ensure when cyl == 6, mpg is less than 15
#'     "mpg", -Inf, 15, "cyl == 6",
#'     # ensure disp is always between 0 and 150
#'     "disp", 0, 150, "TRUE"
#'   ),
#'   constraints_df_cat = tibble::tribble(
#'     ~var, ~allowed, ~forbidden, ~conditions, 
#'     # ensure vs != 1 when gear >= 4
#'     "vs", NA,  1, "gear >= 5",
#'     # ensure vs == 1 when gear >= 4
#'     "vs", 0,  NA, "gear == 4"
#'   )
#' )
#' 
#' @export
#' 
constraints <- function(schema,
                        constraints_df_num = NULL,
                        constraints_df_cat = NULL,
                        max_z_num = 0,
                        max_z_cat = 0) {
  
  # create a new constraints object
  constraints <- new_constraints(schema = schema,
                                 constraints_df_num = constraints_df_num,
                                 constraints_df_cat = constraints_df_cat,
                                 max_z_num = max_z_num,
                                 max_z_cat = max_z_cat)
  
  return(constraints)    
  
}


# constructor 
new_constraints <- function(schema,
                            constraints_df_num,
                            constraints_df_cat,
                            max_z_num,
                            max_z_cat) {
  
  # test inputs
  stopifnot(
    "`schema` must be a schema object" = { is_schema(schema) },
    "`constraints_df_num` must be a data.frame if supplied" = {
      is.data.frame(constraints_df_num) | is.null(constraints_df_num)
    },
    "`constraints_df_cat` must be a data.frame if supplied" = {
      is.data.frame(constraints_df_cat) | is.null(constraints_df_cat)
    },
    "`max_z_num` must be numeric or a named list of numerics" = {
      is.numeric(max_z_num) | is.list(max_z_num)
    },
    "`max_z_cat` must be numeric or a named list of numerics" = {
      is.numeric(max_z_cat) | is.list(max_z_cat)
    },
    "`max_z_num` values must be non-negative" = { all(max_z_num >= 0) },
    "`max_z_cat` values must be non-negative" = { all(max_z_cat >= 0) }
  )

  
  if (is.list(max_z_num)) {
    
    stopifnot(
      "`max_z_num` values must be integers" = {
        all(purrr::flatten_dbl(max_z_num) %% 1 == 0) 
      }
    )
    
  } else {
    
    stopifnot(
      "`max_z_num` values must be integers" = { all(max_z_num %% 1 == 0) }
    )
    
  }
  
  if (is.list(max_z_cat)) {
    
    stopifnot(
      "`max_z_num` values must be integers" = {
        all(purrr::flatten_dbl(max_z_cat) %% 1 == 0) 
      }
    )
    
  } else {
    
    stopifnot(
      "`max_z_cat` values must be integers" = { all(max_z_cat %% 1 == 0) }
    )
    
  }
  
  # save inputs for updating
  inputs <- list(
    "input_constraints_df_num" = constraints_df_num,
    "input_constraints_df_cat" = constraints_df_cat,
    "input_max_z_num" = max_z_num,
    "input_max_z_cat" = max_z_cat
  )
  
  # check potential inputs and extract synth_vars
  synth_vars <- schema[["synth_vars"]]
  
  stopifnot(
    "`.assigned_min` cannot be a synthetic variable name" = {
      !(any(grepl(".assigned_min", synth_vars)))
    },
    "`.assigned_max` cannot be a synthetic variable name" = {
      !(any(grepl(".assigned_max", synth_vars))) 
    }
  )
  
  # separate numeric and categorical variables
  synth_vars_is_num <- purrr::map_lgl(
    .x = synth_vars,
    .f = \(x) { schema[["col_schema"]][[x]][["dtype"]] == "dbl" }
  )
  synth_vars_num <- synth_vars[synth_vars_is_num]
  synth_vars_cat <- synth_vars[!synth_vars_is_num]
  
  # if numeric constraints not provided...
  if (is.null(constraints_df_num)) {
    
    # create an empty numeric constraints list
    constraints_list_num <- vector("list", length = length(synth_vars_num))
    names(constraints_list_num) <- synth_vars_num
    
  } else {
    
    # check constraints_df_num formatting
    required_num_names <- c("var", "min", "max", "conditions") 
    num_names_included <- required_num_names %in% names(constraints_df_num)
    
    if (any(!num_names_included)) {
      
      stop("constraints_df_num missing required column(s): ",
           paste0(required_num_names[!num_names_included], collapse = ", "))
      
    }
    
    # create numeric constraints list
    unconstrained_vars_num <- synth_vars_num[
      !synth_vars_num %in% constraints_df_num$var
    ]
    
    expanded_df_num <- dplyr::bind_rows(
      constraints_df_num,
      tibble::tibble(var = unconstrained_vars_num,
                     min = rep(-Inf, length(unconstrained_vars_num)),
                     max = rep(Inf, length(unconstrained_vars_num)),
                     conditions = rep("TRUE", length(unconstrained_vars_num)))
    )
    
    # split and reorder according to synthesis 
    constraints_list_num <- base::split(expanded_df_num, expanded_df_num$var)
    constraints_list_num <- constraints_list_num[synth_vars_num]
    
  }
  
  # if categorical constraints not provided...
  if (is.null(constraints_df_cat)) {
    
    # create an empty categorical constraints list
    constraints_list_cat <- vector("list", length = length(synth_vars_cat))
    names(constraints_list_cat) <- synth_vars_cat
    
  } else {
    
    # check constraints_df_cat formatting
    required_cat_names <- c("var", "allowed", "forbidden", "conditions") 
    cat_names_included <- required_cat_names %in% names(constraints_df_cat)
    
    if (any(!cat_names_included)) {
      
      stop("constraints_df_cat missing required column(s): ",
           paste0(required_cat_names[!cat_names_included], collapse = ", "))
      
    }
    
    # create numeric constraints list
    unconstrained_vars_cat <- synth_vars_cat[
      !synth_vars_cat %in% constraints_df_cat$var
    ]
    
    expanded_df_cat <- dplyr::bind_rows(
      constraints_df_cat,
      tibble::tibble(var = unconstrained_vars_cat,
                     allowed = rep(NULL, length(unconstrained_vars_cat)),
                     forbidden = rep(NULL, length(unconstrained_vars_cat)),
                     conditions = rep("TRUE", length(unconstrained_vars_cat)))
    )
    
    # split and reorder according to synthesis 
    constraints_list_cat <- base::split(expanded_df_cat, expanded_df_cat$var)
    constraints_list_cat <- constraints_list_cat[synth_vars_cat]
    
  }
  
  # max_z_num
  if (length(max_z_num) == 1 & length(synth_vars_num) != 1) {
    
    max_z_num <- rep(
      list(max_z_num), 
      times = length(synth_vars_num)
    )
    
  }
  
  if (is.null(names(max_z_num))) {
    
    names(max_z_num) <- synth_vars_num
    
  }
  
  # max_z_cat
  if (length(max_z_cat) == 1 & length(synth_vars_cat) != 1) {
    
    max_z_cat <- rep(
      list(max_z_cat), 
      times = length(synth_vars_cat)
    )
    
  }
  
  if (is.null(names(max_z_cat))) {
    
    names(max_z_cat) <- synth_vars_cat
    
  }
  
  # create list of objects
  constraints <- list(
    constraints_num = constraints_list_num,
    constraints_cat = constraints_list_cat,
    max_z_num = max_z_num,
    max_z_cat = max_z_cat,
    inputs = inputs
  )
  
  # create class
  constraints <- structure(constraints, class = "constraints")
  
  return(constraints)
  
}

is_constraints <- function(x) {
  inherits(x, "constraints")
}


#' 
#' Validate constraints
#' 
#' @param roadmap A `roadmap` object
#' 
#' @export 
#' 
validate_constraints <- function(roadmap) {
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) } 
  )
  constraints <- roadmap[["constraints"]]
  
  stopifnot(
    "`constraints` must be a constraints object" = { 
      is_constraints(constraints)
    }
  )
  
  # check if all non-null numeric constraints are in the variable list
  num_names_ind <- purrr::map_lgl(
    .x = constraints[["constraints_num"]], .f = \(z) { !is.null(z) }
  )
  num_names <- names(num_names_ind)[num_names_ind]
  
  # handle case when num_names_inc is an empty logical vector
  if (rlang::is_empty(num_names)) {
    
    num_names_inc <- TRUE
    
  } else {
    
    num_names_inc <- num_names %in% roadmap[["schema"]][["synth_vars"]]
    
  }
  
  if (!all(num_names_inc)) {
    
    stop("Numeric constraint variable(s) not set to be synthesized: ", 
         paste0(num_names[!num_names_inc], collapse = ", "))
    
  }
  
  # check if all non-null categorical constraints are in the variable list
  cat_names_ind <- purrr::map_lgl(
    .x = constraints[["constraints_cat"]], .f = \(z) { !is.null(z) }
  )
  cat_names <- names(cat_names_ind)[cat_names_ind]
  
  # handle case when cat_names_inc is an empty logical vector
  if (rlang::is_empty(cat_names)) {
    
    cat_names_inc <- TRUE
    
  } else {
    
    cat_names_inc <- cat_names %in% roadmap[["schema"]][["synth_vars"]]
    
  }
  
  if (!all(cat_names_inc)) {
    
    stop("Categorical constraint variable(s) not set to be synthesized: ", 
         paste0(cat_names[!cat_names_inc], collapse = ", "))
    
  }
  
  # check constraints on confidential data
  visit_seq <- roadmap[["visit_sequence"]][["visit_sequence"]]
  for (vs_ix in seq_along(visit_seq)) {

    # extract visit_sequence    
    v <- visit_seq[vs_ix]
    var_constraints <- NULL
    
    # find variables further along the visit_sequence
    future_vars <- NULL
    if (vs_ix != length(visit_seq)) {
      
      future_vars <- visit_seq[(vs_ix + 1):length(visit_seq)]
      
    }
    
    mode <- NULL
    
    # check if categorical or numeric constraints supplied
    if (v %in% num_names) {
      
      var_constraints <- constraints[["constraints_num"]][[v]]
      mode <- "numeric"
      
    } else if (v %in% cat_names) {
      
      var_constraints <- constraints[["constraints_cat"]][[v]]
      mode <- "categorical"
      
    }
    
    # if supplied...
    if (!is.null(var_constraints)) {
      
      n_con <- nrow(var_constraints) 
      
      # for each constraint...
      for (con_ix in seq_len(n_con)) {
        
        tryCatch(
          {
            # attempt to evaluate the condition on the confidential data...    
            dplyr::mutate(
              roadmap[["conf_data"]],
              .result = eval(
                parse(
                  text = var_constraints[con_ix, ]$conditions
                )
              ),
              .keep = "none"
            ) 
          },
          # if error thrown, provide additional context
          error = function(msg) {
            
            stop(
              "Failed to evaluate constraint in variable '",
              v, 
              "' on confidential data, defined as: \n",
              paste0(
                utils::capture.output(
                  print(var_constraints[con_ix, ]))[c(2, 4)],
                collapse = "\n"
              ),
              "\n",
              "Full message: \n",
              msg
            )
            
          }
          
        )
        
        # next, try and see if variable names later in the visit sequence
        # are used in the constraint definitions
        if (!is.null(future_vars)) {
          
          # for each future variable in the sequence...
          for (f_var in future_vars) {
            
            # raise an error if found in the condition
            if (stringr::str_detect(
                string = as.character(var_constraints[con_ix, "conditions"]),
                pattern = f_var
              )) {
              
              stop(
                "The constraint below uses a condition with variable '",
                f_var,
                "' that appears after '",
                v, 
                "' in the visit sequence: \n",
                paste0(
                  utils::capture.output(
                    print(var_constraints[con_ix, ]))[c(2, 4)],
                  collapse = "\n"
                )
              )
              
            }
            
          }
          
        }
        
        if (mode == "categorical") {
          
          # next, if categorical check for allowed values listed as valid
          # factor levels in col_schema
          
          schema_levels <- roadmap[["schema"]][["col_schema"]][[v]][["levels"]]
          
          allowed <- dplyr::pull(var_constraints[con_ix, "allowed"])
          forbidden <- dplyr::pull(var_constraints[con_ix, "forbidden"])
          
          if (!is.na(allowed) & !(allowed %in% schema_levels)) {
            
            stop(
              "The constraint below has an allowed level, '",
              allowed, 
              "', that is not listed within the col_schema: \n",
              paste0(
                utils::capture.output(
                  print(var_constraints[con_ix, ]))[c(2, 4)],
                collapse = "\n"
              )
            )
            
          }
          
          if (!is.na(forbidden) & !(forbidden %in% schema_levels)) {
            
            warning(
              "The constraint below has a forbidden level, '",
              forbidden, 
              "', that is not listed within the col_schema: \n",
              paste0(
                utils::capture.output(
                  print(var_constraints[con_ix, ]))[c(2, 4)],
                collapse = "\n"
              )
            )
            
          }
          
          
        }
        
      }
      
    }
    
  }
   
}

# Tidy API calls -----------------------------------------------------------

#' 
#' Add, update, or reset a `constraints` object within an existing `roadmap`.
#'
#' @param roadmap A `roadmap` object
#' @param constraints A `constraints` object. 
#' @param ... Optional named parameters passed to `constraints()`.
#' 
#' @return A new `roadmap` object.
#'
#' @name constraints_api
#'
NULL
#> NULL

#'
#' @rdname constraints_api
#' @export 
#' 
add_constraints <- function(roadmap, constraints) {
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`constraints` must be a constraints object" = { 
      is_constraints(constraints)
    }
  )
  
  roadmap[["constraints"]] <- constraints
  
  return(roadmap)
  
}

#'
#' @rdname constraints_api
#' @export 
#' 
update_constraints <- function(roadmap, ...) {
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) } 
  )
  
  kwargs <- list(...)
  
  valid_kwargs <- c("constraints_df_num", 
                    "constraints_df_cat",
                    "max_z_num",
                    "max_z_cat")
  
  # update input constraints
  for (n in names(kwargs)) {
    
    if (!(n %in% valid_kwargs)) {
      
      stop("Invalid update_constraints argument: ", n)
      
    }
    
    roadmap[["constraints"]][["inputs"]][[
      paste0("input_", n)]] <- kwargs[[n]]
    
  }
  
  # reconstruct constraints
  roadmap[["constraints"]] <- new_constraints(
    schema = roadmap[["schema"]],
    constraints_df_num = roadmap[["constraints"]][["inputs"]][[
      "input_constraints_df_num"
      ]],
    constraints_df_cat = roadmap[["constraints"]][["inputs"]][[
      "input_constraints_df_cat"
      ]],
    max_z_num = roadmap[["constraints"]][["inputs"]][["input_max_z_num"]],
    max_z_cat = roadmap[["constraints"]][["inputs"]][["input_max_z_cat"]]
    
  )
  
  return(roadmap)
  
}

#'
#' @rdname constraints_api
#' @export 
#' 
reset_constraints <- function(roadmap) {
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) } 
  )
  
  roadmap[["constraints"]] <- constraints(schema = roadmap[["schema"]])
  
  return(roadmap)
  
}

#'
#' @export 
print.constraints <- function(x, ...) {
  
  # print numeric constraint conditions
  constraint_conditions_num <- purrr::map_int(x$constraints_num, .f = \(z) { 
    
    if (is.data.frame(z)) { return( base::nrow(z)) } else { return(0) }
    
  }) %>%
    stats::setNames(names(x$constraints_num))
  
  cat("Numeric constraints specified per variable: \n")
  
  for (n in names(constraint_conditions_num)) {
    
    cat(paste0(n, ": ", constraint_conditions_num[[n]], "\n"))
    
  }
  
  # print categorical constraint conditions
  constraint_conditions_cat <- purrr::map_int(x$constraints_cat, .f = \(z) { 
    
    if (is.data.frame(z)) { return( base::nrow(z)) } else { return(0) }
    
  }) %>%
    stats::setNames(names(x$constraints_cat))
  
  cat("Categorical constraints specified per variable: \n")
  
  for (n in names(constraint_conditions_cat)) {
    
    cat(paste0(n, ": ", constraint_conditions_cat[[n]], "\n"))
    
  }
  
  invisible(x)
  
}


