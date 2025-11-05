.identity_start <- function(start_data, ...) { return(start_data) }

#'
#' Create a `start_method` object.
#' 
#' A `start_method` gets executed prior to running a synthesis. This modifies
#' the `start_data`, typically randomly, to provide greater disclosure risk 
#' protections. 
#' 
#' @param start_func A function that accepts and returns a `data.frame`. If none
#' provided `.identity_start()` is used. 
#' @param ... Optional keyword arguments passed to `start_func(...)`
#' 
#' @return A `start_method` object
#' 
#' @examples
#' 
#' # basic usage
#' start_method(start_func = start_resample)
#' 
#' # adjust the number of observations
#' start_method(
#'   start_func = start_resample,
#'   start_data = acs_start_nw,
#'   n = 10
#' )
#' 
#' # adjust the number of observations and use all combinations as support
#' start_method(
#'   start_func = start_resample,
#'   start_data = acs_start_nw,
#'   n = 10, 
#'   inv_noise_scale = 1,
#'   support = "all"
#' )
#' 
#' @export
start_method <- function(start_func = NULL, ...) {
  
  # ensure first argument is a function; if not, use identity function
  if (is.null(start_func)) {
    
    start_func <- .identity_start 
    
  } 
  
  stopifnot(
    "`start_func` is not a function" = { "function" %in% class(start_func) } 
  )
  
  # capture passed arguments
  kwargs <- list(...) 
  
  # create structure
  start_method <- list(
    start_func = start_func,
    kwargs = kwargs
  )
  
  start_method <- structure(start_method, class = "start_method")
  
  return(start_method)
  
}

is_start_method <- function(x) {
  inherits(x, "start_method")
}

#'
#' Execute a `start_method` instance
#' 
#' @param roadmap A `roadmap` object
#' 
#' @return A `data.frame`
#' @noRd
#' 
exec_start_method <- function(roadmap) {
  
  all_kwargs <- c(
    list(start_data = roadmap[["start_data"]]),
    roadmap[["start_method"]][["kwargs"]]
  )
  
  start_data_reps <- purrr::map(
    .x = 1:roadmap[["replicates"]][["start_data_replicates"]],
    .f = \(x) {
      rlang::exec(roadmap[["start_method"]][["start_func"]], 
                  !!!all_kwargs)
    }
  )
  
  return(
    dplyr::bind_rows(start_data_reps)
  )
  
}

#' 
#' Validate a `start_method`
#' 
#' @param roadmap A `roadmap` object.
#' 
#' @return NULL
#' @noRd
#' 
validate_start_method <- function(roadmap) {
  
  # check input object type
  sm <- roadmap[["start_method"]]
  stopifnot(
    "`start_method` must be a start_method object" = { is_start_method(sm) }
  )
  
  # check kwargs names in function arguments
  kwargs <- sm[["kwargs"]]
  stopifnot(
    "Keyword arguments not aligned with provided start_method function" = {
      all(kwargs %in% names(as.list(args(sm))))
    }
  )
  
}

#' 
#' Add, update, or reset a start method within an existing `roadmap`.
#'
#' @param roadmap A `roadmap` object
#' @param start_method A `start_method` object.
#' @param ... Optional named parameters passed to `start_method()`
#' 
#' @return A new `roadmap` object.
#'
#' @name start_method_api
#'
NULL
#> NULL

#'
#' @rdname start_method_api
#' 
#' @return A new `roadmap` object with added start_method.
#' 
#' @examples 
#' 
#' rm <- roadmap( 
#'  conf_data = acs_conf_nw,
#'  start_data = acs_start_nw,
#' )
#' 
#' add_start_method(
#'   roadmap = rm,
#'   start_method = start_method()
#' )
#'
#' @export 
#' 
add_start_method <- function(roadmap, start_method) {
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`start_method` must be a start_method object" = { 
      is_start_method(start_method)
    }
  )
  
  # add new start_method 
  roadmap[["start_method"]] <- start_method
  
  return(roadmap)
  
}

#'
#' @rdname start_method_api
#' 
#' @return A new `roadmap` object with updated start_method.
#' @examples
#' 
#' rm <- roadmap( 
#'  conf_data = acs_conf_nw,
#'  start_data = acs_start_nw
#' )
#' 
#' update_start_method(
#'   roadmap = rm,
#'   start_method = start_method()
#' )
#'
#' @export
update_start_method <- function(roadmap, ...) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  # update start_method parameters
  kwargs <- list(...) 
  
  for (name in names(kwargs)) {
    
    if (name == "start_func") {
      
      roadmap[["start_method"]][[name]] <- kwargs[[name]]
      
    } else {
      
      roadmap[["start_method"]][["kwargs"]][[name]] <- kwargs[[name]]
      
    }
    
  }
  
  return(roadmap)
  
}

#'
#' @rdname start_method_api
#' 
#' @return A new `roadmap` object with removed start_method.
#' 
#' @examples
#' rm <- roadmap( 
#'  conf_data = acs_conf_nw,
#'  start_data = acs_start_nw,
#'  start_method = start_method()
#' )
#' 
#' remove_start_method(
#'   roadmap = rm
#' )
#' 
#' @export
remove_start_method <- function(roadmap) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  # reset roadmap and return
  roadmap[["start_method"]] <- start_method()
  
  return(roadmap)
  
}

#' 
#'  Print the start_method object to the console with formatting
#' 
#' @param x A `start_method` object
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#'
#' @return A `start_method` object
#' 
#' @examples  
#' 
#' print(start_method())
#' 
#' @export
print.start_method <- function(x, ...) {
  
  if (identical(x$start_func, .identity_start)) {
    
    start_func_name <- "Identity (No Method Specified)"
    
  } else {
    
    start_func_name <- "User-Specified"
    
  }
  
  cat(sprintf("Start Method: %s \n", start_func_name))
  
  if (!rlang::is_empty(x$kwargs)) {
    
    cat("Keyword Arguments: \n")
    
    for (n in names(x$kwargs)) {
      
      cat(paste0(n, ": ", x$kwargs[[n]], "\n"))
      
    }
    
  }
  
  invisible(x)
  
}
