
#' 
#' Extract named vector of model modes from a roadmap
#' 
#' @param roadmap A `roadmap` S3 object.
#' 
#' @return A named vector of `regression` and `classification` strings
#' 
.extract_mode <- function(roadmap) {
  
  visit_sequence <- roadmap[["visit_sequence"]][["visit_sequence"]]
  
  mode <- purrr::map_chr(
    .x = visit_sequence, 
    .f = \(x) {
      if (x %in% names(roadmap[["schema"]][["col_schema"]])) {
        # if name in visit_sequence, use col_schema
        dtype <- roadmap[["schema"]][["col_schema"]][[x]][["dtype"]]
        return(if (dtype == "dbl") "regression" else "classification")
      } else {
        # else, is _NA indicator and default to classification
        return("classification")
      }
    }
  ) %>%
    stats::setNames(visit_sequence)
  
  return(mode)
  
}


#' 
#' Validate construct_* functions for required components (for internal use only)
#' 
#' @param visit_sequence A character vector of `visit_sequence` variable names.
#' @param mode A named character vector mapping `visit_sequence` variable names
#' to one of `regression` or `classification`.
#' @param default_reg A default object used for regression models
#' @param default_class A default object used for classification models
#' @param custom_list A list of named lists mapping variables to objects
#' @param type_check_func A function that returns a boolean if the object if 
#' of the correct type.
#' @param obj_name A string describing the object name, used only for printing
#' explanatory error messages.
#' 
.validate_construct_inputs_required <- function(
    visit_sequence, 
    mode,
    default_reg, 
    default_class, 
    custom_list,
    type_check_func,
    obj_name) {
  
  if ( is.null(default_reg) & is.null(default_class) & is.null(custom_list) ) {
    
    # error if no object specified
    stop("No ", obj_name, " specified")
    
  }
    
  # mixed defaults setting 
  # create object to handle which modes are covered
  modes_needed <- unique(mode)
  covered_modes <- c()
  
  # if default regression object specified, check type
  if (!is.null(default_reg)) {
    
    if (!type_check_func(default_reg)) {
      
      stop("Default regression ",
           obj_name,
           " has incorrect type")
      
    }
    
    covered_modes <- c(covered_modes, "regression")
    
  }
  
  # if default classification object specified, check type
  if (!is.null(default_class)) {
    
    if (!type_check_func(default_class)) {
      
      stop("Default classification ",
           obj_name,
           " has incorrect type")
      
    }
    
    covered_modes <- c(covered_modes, "classification")
    
  }
  
  # determine which variables, if any, need custom specification
  custom_modes <- setdiff(modes_needed, covered_modes)
  required_custom_vars <- visit_sequence[mode %in% custom_modes]
  
  # if custom specifications required...
  if (!rlang::is_empty(required_custom_vars)) {
    
    # ensure custom specification created
    if (is.null(custom_list)) {
      
      stop("Variables missing ",
           obj_name,
           " specification: ",
           paste0(required_custom_vars, collapse = ", "))
      
    }
    
    # objects inside custom_list are already validated via 
    # `.validate_custom_components()`, so we only need to check if all 
    # the names in custom_list are in visit_sequence AND all names in 
    # required_custom_vars are in the names from custom_list
    
    custom_names <- purrr::flatten_chr(purrr::map(custom_list, "vars"))
    in_vs <- custom_names %in% visit_sequence
      
    if (!all(in_vs)) {
      
      stop("Custom ", 
           obj_name, 
           " list has variables not in visit_sequence: ",
           paste0(custom_names[!in_vs], collapse=", "))
      
    }
    
    required_covered <- required_custom_vars %in% custom_names
    if (!all(required_covered)) {
      
      stop("Custom ",
           obj_name, 
           " list missing variable without ",
           obj_name,
           " specification: ",
           paste0(required_custom_vars[!required_covered]))
      
    }
    
    # check no names repeated across list components
    custom_count <- table(custom_names)
    dupl <- (custom_count > 1)
    
    if (any(dupl)) {
      
      stop("Custom ",
           obj_name,
           " list has repeated variable names: ",
           names(custom_count)[dupl])
      
    }
    
  }
  
}


#' 
#' Validate construct_* functions for optional components (for internal use only)
#' 
#' @param visit_sequence A character vector of `visit_sequence` variable names.
#' @param default_reg A default object used for regression models
#' @param default_class A default object used for classification models
#' @param custom_list A list of named lists mapping variables to objects
#' @param type_check_func A function that returns a boolean if the object if 
#' of the correct type.
#' @param obj_name A string describing the object name, used only for printing
#' explanatory error messages.
#' 
.validate_construct_inputs_optional <- function(
    visit_sequence, 
    default_reg, 
    default_class,
    custom_list,
    type_check_func,
    obj_name) {
  
  # if default regression object specified, check type
  if (!is.null(default_reg)) {
    
    if (!type_check_func(default_reg)) {
      
      stop("Default regression ",
           obj_name,
           " has incorrect type")
      
    }
    
  }
  
  # if default classification object specified, check type
  if (!is.null(default_class)) {
    
    if (!type_check_func(default_class)) {
      
      stop("Default classification ",
           obj_name,
           " has incorrect type")
      
    }
    
  }
  

  if (!is.null(custom_list)) {
    
    # objects inside custom_list are already validated via 
    # `.validate_custom_components()`, so we only need to check if all 
    # the names in custom_list are in visit_sequence AND all names in 
    # required_custom_vars are in the names from custom_list
    
    custom_names <- purrr::flatten_chr(purrr::map(custom_list, "vars"))
    in_vs <- custom_names %in% visit_sequence
    
    if (!all(in_vs)) {
      
      stop("Custom ", 
           obj_name, 
           " list has variables not in visit_sequence: ",
           paste0(custom_names[!in_vs], collapse=", "))
      
    }
    
    # check no names repeated across list components
    custom_count <- table(custom_names)
    dupl <- (custom_count > 1)
    
    if (any(dupl)) {
      
      stop("Custom ",
           obj_name,
           " list has repeated variable names: ",
           names(custom_count)[dupl])
      
    }
    
  }
  
}
