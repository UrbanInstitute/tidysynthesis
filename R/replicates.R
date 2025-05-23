.validate_rep_arg <- function(val, argname) {
  
  if (!is.numeric(val)) {
    
    stop("Argument `", argname, "` is not numeric.")
    
  }
  
  if (length(val) != 1) {
    
    stop("Argument `", argname, "` must be length 1.")
    
  }
  
  if (as.integer(val) != val) {
    
    stop("Argument `", argname, "` must be integer-valued.")
    
  }
  
  if (val < 1) {
    
    stop("Argument `", argname, "` must be >= 1.")
    
  }
  
}

#' Create a replicates object
#'
#' @param start_data_replicates The number of starting data replicates to use.
#' Note that if no `start_method` is provided, all start data replicates will 
#' be identical.
#' @param model_sample_replicates The number of replicates for the conditional 
#' modeling process, including modeling and sampling new synthetic values.
#' @param end_to_end_replicates The number of replicates for the entire synthesis process,
#' including all previously specified steps. 
#'
#' @return A new `replicates` object.
#'
#' @export
#' 
replicates <- function(start_data_replicates = 1, 
                       model_sample_replicates = 1, 
                       end_to_end_replicates = 1) {
  
  # create a new replicates object
  replicates <- new_replicates(
    start_data_replicates = start_data_replicates, 
    model_sample_replicates = model_sample_replicates, 
    end_to_end_replicates = end_to_end_replicates
  )
  
  return(replicates)    
  
}

# constructor (for experienced users only)
new_replicates <- function(start_data_replicates = 1, 
                           model_sample_replicates = 1, 
                           end_to_end_replicates = 1) {
  
  # input checking  
  .validate_rep_arg(start_data_replicates, "start_data_replicates")
  .validate_rep_arg(model_sample_replicates, "model_sample_replicates")
  .validate_rep_arg(end_to_end_replicates, "end_to_end_replicates")
  
  total_replicates <- (start_data_replicates * 
      model_sample_replicates * 
      end_to_end_replicates)
    
  # create list of objects
  replicates <- list(
    start_data_replicates = start_data_replicates,
    model_sample_replicates = model_sample_replicates,
    end_to_end_replicates = end_to_end_replicates,
    total_replicates = total_replicates
  )
  
  # create class
  replicates <- structure(replicates, class = "replicates")
  
  return(replicates)
  
}

is_replicates <- function(x) {
  inherits(x, "replicates")
}



# validator
validate_replicates <- function(roadmap) {
  
  # input checking
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  replicates <- roadmap[["replicates"]]
  stopifnot("`replicates` must be a replicates object" = { 
    is_replicates(replicates) }
  )
  

  for (varname in c("start_data_replicates",
                    "model_sample_replicates",
                    "end_to_end_replicates")) {
    
    .validate_rep_arg(replicates[[varname]], varname)
    
  }
  
  stopifnot(
    "`total_replicates` is inconsistent due to manual overriding" = {
      replicates[["total_replicates"]] == (
      replicates[["start_data_replicates"]] * 
      replicates[["model_sample_replicates"]] * 
      replicates[["end_to_end_replicates"]]
      )
    }
  )
  
}

# print method
#' @export
print.replicates <- function(x, ...) {

  cat("Replicates\n")
  cat("\n")
  cat("Start Data Replicates: ", x[["start_data_replicates"]], "\n") 
  cat("Model Sample Replicates: ", x[["model_sample_replicates"]], "\n") 
  cat("End-to-End Replicates: ", x[["end_to_end_replicates"]], "\n")
  cat("\n")
  cat("Total Replicates: ", x[["total_replicates"]], "\n")

  invisible(x)

}

# Tidy API calls -----------------------------------------------------------


#' 
#' Add, update, or reset a `replicates` object within an existing `roadmap`.
#'
#' @param roadmap A `roadmap` object
#' @param replicates A `replicates` object. 
#' @param ... Optional named parameters passed to `replicates()`.
#' 
#' @return A new `roadmap` object.
#'
#' @name replicates_api
#'
NULL
#> NULL

#'
#' @rdname replicates_api
#' @export 
#' 
add_replicates <- function(roadmap, replicates) {
  
  stopifnot(
    "`roadmap` must be a roadmap object" = { is_roadmap(roadmap) },
    "`replicates` must be a replicates object" = { 
      is_replicates(replicates) 
    }
  )
  
  roadmap[["replicates"]] <- replicates
  
  return(roadmap)
  
}

#'
#' @rdname replicates_api
#' @export 
#' 
update_replicates <- function(roadmap, ...) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  # add arguments 
  kwargs <- list(...)
  for (name in names(kwargs)) {
    
    roadmap[["replicates"]][[name]] <- kwargs[[name]]
    
  }
  
  # recompute total replicates and update
  roadmap[["replicates"]][["total_replicates"]] <- (
    roadmap[["replicates"]][["start_data_replicates"]] * 
      roadmap[["replicates"]][["model_sample_replicates"]] * 
      roadmap[["replicates"]][["end_to_end_replicates"]]
  )
  
  return(roadmap)
  
}

#'
#' @rdname replicates_api
#' @export 
#' 
reset_replicates <- function(roadmap) {
  
  stopifnot("`roadmap` must be a roadmap object" = { is_roadmap(roadmap) })
  
  roadmap[["replicates"]] <- replicates()
  
  return(roadmap)
  
}

