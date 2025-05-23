#' 
#' Create a roadmap
#'
#' A `roadmap` is a container object that aggregates information required to 
#' specify the order of operations for synthesis modeling and sampling steps. 
#' 
#' Users initiate a roadmap object with `conf_data` and `start_data`. All other 
#' objects will either be completed with defaults or specified interactively via
#' the provided API. 
#'
#' @param conf_data A `data.frame` of confidential data.
#' @param start_data A `data.frame` of starting data used to initialize the process.
#' @param start_method An optional `start_method` object.
#' @param schema An optional `schema` object.
#' @param visit_sequence An optional `visit_sequence` object.
#' @param replicates An optional `replicates` object.
#' @param constraints An optional `constraints` object. 
#'
#' @return A new `roadmap` object.
#' 
#' @examples
#' roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw,
#'   start_method = start_method(
#'     start_func = start_resample, n = 1000
#'   )
#' ) 
#' 
#'
#' @export
#' 
roadmap <- function(conf_data, 
                    start_data, 
                    start_method = NULL, 
                    schema = NULL,
                    visit_sequence = NULL,
                    replicates = NULL,
                    constraints = NULL) {
   
  roadmap <- new_roadmap(
    conf_data = conf_data,
    start_data = start_data, 
    start_method = start_method,
    schema = schema, 
    visit_sequence = visit_sequence,
    replicates = replicates,
    constraints = constraints
  )
  
  return(roadmap)
  
}

#' 
#' Roadmap constructor 
#' 
#' @param conf_data A `data.frame` of confidential data.
#' @param start_data A `data.frame` of starting data used to initialize the process.
#' @param start_method An optional `start_method` object.
#' @param schema An optional `schema` object.
#' @param visit_sequence An optional `visit_sequence` object.
#' @param replicates An optional `replicates` object.
#' @param constraints An optional `constraints` object. 
#'
#' @return A new `roadmap` object.
#' 
new_roadmap <- function(conf_data, 
                        start_data, 
                        start_method = NULL, 
                        schema = NULL,
                        visit_sequence = NULL,
                        replicates = NULL,
                        constraints = NULL) {
  
  # check input data
  stopifnot(
    "`conf_data` must be a data.frame" = { is.data.frame(conf_data) },
    "`start_data` must be a data.frame" = { is.data.frame(start_data) }
  )
  
  # if not provided, create schema
  if (is.null(schema)) {
    
    schema <- schema(conf_data = conf_data, start_data = start_data)
    
  } else {
    
    stopifnot("`schema` must be a schema object" = { is_schema(schema) } )
    
  }
  
  # if not provided, create replicates
  if (is.null(start_method)) {
    
    start_method <- start_method()
    
  } else {
    
    stopifnot(
      "`start_method` must be a start_method object" = {
         is_start_method(start_method)
      }
    )
  }
  
  # if not provided, create visit_sequence
  if (is.null(visit_sequence)) {
    
    visit_sequence <- visit_sequence(schema = schema)
    
  } else {
    
    stopifnot(
      "`visit_sequence` must be a visit_sequence object" = {
        is_visit_sequence(visit_sequence)
      }
    )
    
  }
  
  # if not provided, create replicates
  if (is.null(replicates)) {
    
    replicates <- replicates()
    
  } else {
    
    stopifnot(
      "`replicates` must be a replcates object" = {
        is_replicates(replicates)
      }
    )
    
  }
  
  # if not provided, create constraints
  if (is.null(constraints)) {
    
    constraints <- constraints(schema = schema)
    
  } else {
    
    stopifnot(
      "`constraints` must be a constraints object" = {
        is_constraints(constraints)
      }
    )
    
  }
  
  # create list of objects
  roadmap <- list(
    conf_data = conf_data, 
    start_data = start_data, 
    start_method = start_method, 
    schema = schema, 
    visit_sequence = visit_sequence, 
    replicates = replicates, 
    constraints = constraints
  )
  
  # create class
  roadmap <- structure(roadmap, class = "roadmap")
  
  return(roadmap) 
  
}


is_roadmap <- function(x) {
  inherits(x, "roadmap")
}



#' 
#' Roadmap validator
#' 
#' Ensures internal and external consistency between a `roadmap` and its constituent
#' objects, raising errors if the `roadmap` cannot be used for syntheses. This 
#' function is lazily evaluated immediately prior to synthesis.
#' 
#' @param roadmap A `roadmap` object.
#' 
#' @return NULL
#' 
validate_roadmap <- function(roadmap) {
  
  # validate data
  stopifnot(
    "`conf_data` must be a data.frame" = { 
      is.data.frame(roadmap[["conf_data"]]) 
    },
    "`start_data` must be a data.frame" = {
      is.data.frame(roadmap[["start_data"]]) 
    }
  )
  
  # validate constituent components
  validate_schema(roadmap)
  validate_visit_sequence(roadmap)
  validate_replicates(roadmap)
  validate_constraints(roadmap)
  
}


#' @export 
print.roadmap <- function(x, ...) {
  
  cat("Roadmap: \n")
  
  cat(
    base::sprintf(
      "conf_data: %s observations, %s variables \n", 
      dim(x$conf_data)[1], 
      dim(x$conf_data)[2]
    )
  )
  
  cat(
    base::sprintf(
      "start_data: %s observations, %s variables", 
      dim(x$start_data)[1], 
      dim(x$start_data)[2]
    )
  )
  
  invisible(x)
  
}
