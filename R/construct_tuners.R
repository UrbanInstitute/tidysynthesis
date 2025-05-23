#' Construct a list of tuning grids for hyperparameter tuning predictive models
#'
#' @param roadmap A roadmap object
#' @param default_regression_tuner A tuner.
#' @param default_classification_tuner A tuner.
#' @param custom_tuners A formatted list of tuners.
#'
#' @return A named list of tuners
#' 
#' @export
#' 
construct_tuners <- function(
    roadmap, 
    default_regression_tuner = NULL,
    default_classification_tuner = NULL,
    custom_tuners = NULL
) {
  
  # create vectors that we will use below
  if (!is_roadmap(roadmap)) {
    
    stop("`roadmap` must be a roadmap object")
    
  }
  
  visit_sequence <- roadmap[["visit_sequence"]][["visit_sequence"]]
  mode <- .extract_mode(roadmap)
  
  # validate inputs
  .validate_construct_inputs_optional(
    visit_sequence = visit_sequence,
    default_reg = default_regression_tuner, 
    default_class = default_classification_tuner, 
    custom_list = custom_tuners,
    type_check_func = .is_tuner,
    obj_name = "tuner(s)"
  )
  
  # check function inputs ---------------------------------------------------
  
  if (
    is.null(default_regression_tuner) & 
    is.null(default_classification_tuner) & 
    is.null(custom_tuners)
  ) {
    
    warning("No tuners specified, using default tuner")
    
    return(
      purrr::map(purrr::set_names(visit_sequence), \(x) { NULL })
    )
    
  }  
  
  # construct tuners --------------------------------------------------------
  
  # fill in object with default
  tuners <- purrr::map(
    .x = mode,
    .f = ~ if (.x == "regression") { 
      default_regression_tuner 
    } else { 
        default_classification_tuner 
    }
  )
  
  # add names to object
  names(tuners) <- visit_sequence
  
  for (var_ix in seq_along(visit_sequence)) {
    
    
    var <- visit_sequence[var_ix]
    
    # see if there is a custom tuner
    custom_tuner <- NULL
    for (i in seq_along(custom_tuners)) {
      
      if (var %in% custom_tuners[[i]][["vars"]]) {
        
        custom_tuner <- custom_tuners[[i]][["tuner"]]
        
      }
      
    }
    
    # if custom tuner, then replace everything with the custom tuner
    if (!is.null(custom_tuner)) {
      
      # first, check for `NA` tuners converted to `NULL` to bypass tuning
      # for custom variables
      if (all(is.na(custom_tuner))) {
        
        tuners[var_ix] <- list(NULL)
        
      } else {
        
        tuners[[var]] <- custom_tuner
        
      }
      
    } 
    
  }
  
  # overwrite tuners for outcome variables with no variation
  no_var_vars <- roadmap[["schema"]][["no_variation"]]
  
  no_var_vars <- names(no_var_vars)[unname(no_var_vars)]
  
  if (!is.null(no_var_vars)) {
    
    tuners <- purrr::modify_at(
      .x = tuners,
      .at = no_var_vars,
      .f = ~ "identity"
    )
    
  }
  
  return(tuners)
  
}
