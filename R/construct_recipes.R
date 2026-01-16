#' 
#' Construct a sequence of model recipes for sequential synthesis
#'
#' @param roadmap A roadmap object
#' @param default_regression_steps A list containing one or more recipes::step_*()
#' @param default_classification_steps A list containing one or more recipes::step_*()
#' @param custom_steps A list of lists containing one or more recipes::step_*()
#'
#' @return A list of formulas
#'
#' @examples
#' 
#' rm <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' )
#' 
#' construct_recipes(rm)
#'
#' @examples
#' 
#' # construct_recipes() can create a sequence of recipes using a fully-default 
#' # approach, a hybrid approach, or a fully-customized approach. All approaches
#' # require a roadmap and steps. 
#' 
#' rm <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' )
#' 
#' step1 <- function(x) {
#' x |>
#'   recipes::step_center(recipes::all_predictors(), id = "center")
#' }
#' 
#' # Fully-default approach
#' 
#' construct_recipes(
#'   roadmap = rm, 
#'   default_regression_steps = step1, 
#'   default_classification_steps = step1
#' )
#' 
#' # Hybrid approach
#' 
#' step2 <- function(x) {
#'   x |>
#'     recipes::step_scale(recipes::all_predictors(), id = "scale")
#' }
#' 
#' construct_recipes(
#'   roadmap = rm, 
#'   default_regression_steps = step1,
#'   default_classification_steps = step1,
#'   custom_steps = list(
#'     list(vars = "age", step = step2)
#'   )
#' )
#' 
#' # Fully-customized approach
#' 
#' construct_recipes(
#'   roadmap = rm, 
#'   custom_steps = list(
#'     list(vars = c("hcovany", "empstat", "classwkr"), step = step1),
#'     list(vars = c("age", "famsize", "transit_time", "inctot"), step = step1)
#'   )
#' )
#' 
#' @export
construct_recipes <- function(
    roadmap,
    default_regression_steps = NULL,
    default_classification_steps = NULL,
    custom_steps = NULL
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
    default_reg = default_regression_steps, 
    default_class = default_classification_steps, 
    custom_list = custom_steps,
    type_check_func = .is_steps,
    obj_name = "step(s)"
  )
  
  
  # remove empty factor levels in roadmap so the recipes align with the 
  # data used to train the model in synthesize()
  conf_data_drop <- dplyr::mutate(
    roadmap[["conf_data"]], 
    dplyr::across(dplyr::where(is.factor), droplevels)
  )
  
  start_data_drop <- dplyr::mutate(
    roadmap[["start_data"]], 
    dplyr::across(dplyr::where(is.factor), droplevels)
  )
  
  # construct formulas --------------------------------------------------------
  
  # create a vector with the ordered sequence of outcome variables
  outcomes <- visit_sequence
  
  # create a list with vectors of predictors
  # pre-allocate the list
  predictors <- vector(mode = "list", length = length(outcomes))
  
  # capture the starting data
  start_data_predictors <- names(start_data_drop)
  
  # create the first set of predictors (start_data only)
  predictors[[1]] <- start_data_predictors
  
  # iterate and build a list of vectors of predictor variables where 
  # earlier variables in the visit sequence are predictors for the current
  # variable in the visit sequence
  if (length(outcomes) > 1) {
    
    for (ith_predictor in 2:length(outcomes)) {
      
      predictors[[ith_predictor]] <- 
        c(start_data_predictors, outcomes[1:(ith_predictor - 1)])
      
    }
    
  }
  
  # create a recipe for each formula
  # this recipe shouldn't have any steps
  # 
  # note: we don't use the formula to create outcomes and predictors here
  # because that approach to building recipes is slow in high-dimensional 
  # applications
  synth_recipes <- vector(mode = "list", length = length(outcomes))
  
  for (jth_formula in seq_along(synth_recipes)) {
    
    synth_recipes[[jth_formula]] <- 
      recipes::recipe(
        x = utils::head(conf_data_drop), 
        vars = c(predictors[[jth_formula]], outcomes[jth_formula])
      ) |>
      recipes::update_role(outcomes[jth_formula], new_role = "outcome") |>
      recipes::update_role(predictors[[jth_formula]], new_role = "predictor") 
    
  }
  
  
  # construct recipes -----------------------------------------------------
  
  # construct_recipes() builds recipes without any steps by default
  # the next step is only used for adding steps or custom recipes
  if (
    !is.null(default_regression_steps) | 
    !is.null(default_classification_steps) | 
    !is.null(custom_steps)
  ) {
    
    # create a named list of steps
    full_steps <- vector(mode = "list", length = length(visit_sequence))
    
    # add default steps to the list if there is a default step for the model 
    # type
    for (step_i in seq_along(full_steps)) {
      
      if (!is.null(default_regression_steps) & mode[step_i] == "regression") {
        
        full_steps[[step_i]] <- default_regression_steps
        
      }
      
      if (!is.null(default_classification_steps) & mode[step_i] == "classification") {
        
        full_steps[[step_i]] <- default_classification_steps
        
      }
      
    }
    
    # add names to steps
    names(full_steps) <- visit_sequence       
    
    # look for custom steps to overwrite the default steps
    for (var in visit_sequence) {
      
      # see if there is a custom steps
      custom_steps_temp <- NULL
      for (k in seq_along(custom_steps)) {
        
        if (var %in% custom_steps[[k]][["vars"]]) {
          
          custom_steps_temp <- custom_steps[[k]][["steps"]]
          
        }
        
      }
      
      # if custom extractor, then replace everything with the custom extractor
      if (!is.null(custom_steps_temp)) {
        
        full_steps[[var]] <- custom_steps_temp
        
      }
      
    }
    
    # iterate over list of steps and overwrite the default recipes with custom
    # recipes
    for (m in seq_along(visit_sequence)) {
      
      # only update recipe if supplied component is steps
      # this can happen when default regression and/or classification steps
      # are specified but not used 
      synth_recipes[[m]] <- purrr::map(
        full_steps[m], \(.x) {
          if (.is_steps(.x)) {
            .x(synth_recipes[[m]])
          } else {
            synth_recipes[[m]]
          }
        })[[1]]
      
    }
    
  }
  
  names(synth_recipes) <- visit_sequence
  
  # overwrite synth_recipes for outcome variables with no variation
  identity <- list(
    var_info = tibble::tibble(
      variable = NA, 
      type = "identity",
      role = "outcome",
      source = ""
    ),
    term_info = NULL,
    steps = NULL,
    template= NULL,
    levels = NULL,
    retained = NA,
    requirements = NULL
  )
  
  # overwrite variables with no variation
  no_var_vars <- roadmap[["schema"]][["no_variation"]]
  
  no_var_vars <- names(no_var_vars)[unname(no_var_vars)]
  
  if (!is.null(no_var_vars)) { 
    
    for (no_var_var in no_var_vars) {
      
      identity_var <- identity
      identity_var[["var_info"]][["variable"]] <- no_var_var
      synth_recipes[[no_var_var]] <- identity_var
      
    }

  }
  
  return(synth_recipes)
  
}