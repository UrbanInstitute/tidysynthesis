#' 
#' Generate a visit sequence.
#'
#' @param schema A `schema` object.
#' @param weight_var A numeric weight for the weighted total ordering.
#' @param synthesize_weight Boolean for if weight_var should be included in the
#'   visit sequence. 
#'
#' @return A `visit_sequence` object.
#'
#' @examples
#' 
#' df <- data.frame(
#'   factor_var = c("1", "1", "2"),
#'   vara = c(10000, 20000, 100000),
#'   varb = c(300, 200, 100),
#'   var_loss = c(1999999, 0, -1000000),
#'   weight = c(1000, 1000, 2000)
#' )
#' 
#' start_df <- dplyr::select(df, factor_var)
#' 
#' schema1 <- schema(
#'   conf_data = dplyr::select(df, -weight),
#'   start_data = start_df
#' )
#'                   
#' vs1 <- visit_sequence(
#'   schema = schema1
#' )
#' 
#' schema2 <- schema(
#'   conf_data = df,
#'   start_data = start_df
#' )
#'
#' vs2 <- visit_sequence(
#'   schema = schema2,
#'   weight_var = weight,
#'   synthesize_weight = TRUE
#' )
#' 
#' @export
#' 
visit_sequence <- function(schema,
                           weight_var = NULL,
                           synthesize_weight = TRUE) {
  
  weight_var <- rlang::enquo(weight_var)
  
  # create new_visit_sequence
  visit_sequence <- new_visit_sequence(
    schema = schema,
    weight_var = weight_var,
    synthesize_weight = synthesize_weight
  )
  
  return(visit_sequence)    
  
}

#'
#' Constructor for `visit_sequence`
#' 
#' @param schema A `schema` object.
#' @param weight_var A numeric weight for the weighted total ordering.
#' @param synthesize_weight Boolean for if weight_var should be included in the
#'   visit sequence. 
#'
#' @return A `visit_sequence` object.
#' 
#' @noRd
#' 
new_visit_sequence <- function(schema, 
                               weight_var = NULL,
                               synthesize_weight = TRUE) {
  
  # unpack schema object
  default_sequence <- schema[["synth_vars"]]
  
  if (!synthesize_weight) {
    
    default_sequence <- default_sequence[
      default_sequence != rlang::as_label(weight_var)
    ]
    
  }
  
  # create a list of objects
  visit_sequence <- list(
    default_sequence = default_sequence,
    built_sequence = NULL,
    visit_sequence = default_sequence,
    visit_method = rep("default", length(default_sequence)),
    synthesize_weight = synthesize_weight,
    weight_var = weight_var
  )
  
  visit_sequence <- structure(visit_sequence, class = "visit_sequence")
  
  return(visit_sequence)
  
}

is_visit_sequence <- function(x) {
  inherits(x, "visit_sequence")
}

#'
#' Validate `visit_sequence`
#' 
#' @param roadmap A `roadmap` object.
#' 
#' @return NULL
#' 
#' @noRd
#' 
validate_visit_sequence <- function(roadmap) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  visit_sequence <- roadmap[["visit_sequence"]]
  synth_vars <- roadmap[["schema"]][["synth_vars"]]
  
  # if synthesize_weight, weight_var can't be in start data
  if (visit_sequence[["synthesize_weight"]]) {
    
    # is the weight_var in the start data
    if (rlang::quo_name(visit_sequence[["weight_var"]]) %in% 
        names(roadmap[["start_data"]])) {
      
      stop("Cannot synthesize weight_var if weight_var is in start_data")
      
    }
    
  }
  
  # check handling of NA variables in visit_sequence
  visit_sequence_vec <- visit_sequence[["visit_sequence"]]
  
  for (var in visit_sequence_vec) {
    
    if (stringr::str_detect(var, "_NA$")) {
      
      var_no_NA <- stringr::str_remove(var, "_NA$")
      
      var_location <- which(visit_sequence_vec == var_no_NA)
      var_NA_location <- which(visit_sequence_vec == var)
      
      if (var_location < var_NA_location) {
        
        stop(
          paste("_NA vars must come before their corresponding variables. \n",
                "Issues with", var, "and", var_no_NA)
        )
        
      }
      
    }
    
  }
  
  
  # test that visit_sequence variables are in conf_data
  if (!all(visit_sequence[["visit_sequence"]] %in% synth_vars)) {
    
    stop(
      "Variables from the visit sequence must be in conf_data and not in start_data\n",
      "  Problem variable(s): ", 
      paste0(
        visit_sequence$visit_sequence[!visit_sequence$visit_sequence %in% synth_vars], 
        collapse = ", "
      )
    )
    
  }
  
  # message if any variables still using default method
  default_vars <- (visit_sequence[["visit_method"]] == "default")
  if (any(default_vars)) {
    
    message(
      paste0(
        "Some variable(s) have no non-default visit sequence method specified: ",
        paste0(default_vars, collapse = ", ")
      )
      
    )
    
  }
  
}


#' Print method for `visit_sequence` objects
#' 
#' @param x A `visit_sequence` object
#' @param ... further arguments passed to or from other methods (not currently
#'  used).
#' 
#' @return Invisibly returns the input `visit_sequence` object.
#' 
#' @examples
#' 
#' rm <- roadmap(
#'   conf_data = acs_conf_nw, 
#'   start_data = acs_start_nw
#' )
#' 
#' print(rm[["visit_sequence"]])
#'
#' @export
print.visit_sequence <- function(x, ...) {
  
  output <- tibble::tibble(
    method = as.character(x$visit_method),
    variable = as.character(x$visit_sequence)
  ) |>
    dplyr::transmute(output = paste0(.data$method, ":", .data$variable)) |>
    dplyr::pull(output)
  
  cat("Visit Sequence\n")
  cat("Method:Variable\n")
  cat(output, "\n")
  
  invisible(x)
  
}


