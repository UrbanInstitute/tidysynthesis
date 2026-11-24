#' Validate that discarded models can be recovered from the cache
#' 
#' A variable whose model is `"fitted"` but absent from memory exists only in
#' the cache, so its cache file must exist. Variables in `j` are about to be
#' refit and are exempt. Also rejects switching to a different cache directory 
#' while such variables depend on the stored one.
#'
#' @param synth A `synth` object.
#' @param j A character vector of variables about to be fit.
#' @param cache_path The cache directory resolved for the current call.
#'
#' @return NULL
#' @noRd
#'
.validate_synth_cache <- function(synth, j, cache_path) {
  
  vs_names <- names(synth[["workflows"]][["built_models"]])
  
  on_disk_only <- vs_names[
    synth[["roles"]][vs_names] == "fitted" & 
      purrr::map_lgl(synth[["fitted_models"]][vs_names], is.null)
  ]
  on_disk_only <- setdiff(on_disk_only, j)
  
  if (length(on_disk_only) == 0) {
    
    return(invisible(NULL))
    
  }
  
  stored_path <- synth[["cache_path"]]
  
  stopifnot(
    "Models are missing from memory but `synth` has no `cache_path`" = {
      !is.null(stored_path)
    }
  )
  
  cache_files <- file.path(stored_path, paste0(on_disk_only, ".rds"))
  missing_vars <- on_disk_only[!file.exists(cache_files)]
  
  # these messages name specific variables and paths, so they are built 
  # dynamically instead of with `stopifnot()`
  if (length(missing_vars) > 0) {
    
    stop(
      paste0(
        "Cache files are missing from `", stored_path, "` for: ", 
        paste(missing_vars, collapse = ", "), ". Refit them with ", 
        "`synth_fit(synth, j = <variable>)`."
      ),
      call. = FALSE
    )
    
  }
  
  if (!identical(
    normalizePath(cache_path, mustWork = FALSE),
    normalizePath(stored_path, mustWork = FALSE)
  )) {
    
    stop(
      paste0(
        "`cache_path` cannot change from `", stored_path, "` while models for ",
        paste(on_disk_only, collapse = ", "), " exist only in the cache."
      ),
      call. = FALSE
    )
    
  }
  
  return(invisible(NULL))
  
}
