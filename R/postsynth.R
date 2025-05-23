#' Create a postsynth object
#'
#' @param synthetic_data A synthetic data set.
#' @param jth_preprocessing A list of recipes from `library(recipe)`.
#' @param total_synthesis_time A double with the total time of synthesis.
#' @param jth_synthesis_time A vector or dataframe of synthesis times. 
#' @param extractions A list with extracted output from the synthesis model.
#' @param ldiversity A vector or dataframe of ldiversity stats. 
#'
#' @return A `postsynth` object.
#' 
#' @export
postsynth <- function(synthetic_data,
                      jth_preprocessing,
                      total_synthesis_time,
                      jth_synthesis_time,
                      extractions,
                      ldiversity) {
  
  # create new_postsynth
  postsynth <- new_postsynth(synthetic_data = synthetic_data,
                             jth_preprocessing = jth_preprocessing,
                             total_synthesis_time = total_synthesis_time,
                             jth_synthesis_time = jth_synthesis_time,
                             extractions = extractions,
                             ldiversity = ldiversity)
  
  # todo(aaron): add validator
  
  return(postsynth)    
  
}

# constructor (for experienced users only)
new_postsynth <- function(synthetic_data,
                          jth_preprocessing,
                          total_synthesis_time,
                          jth_synthesis_time,
                          extractions,
                          ldiversity) {
  
  # test inputs

  # create list of objects
  postsynth <- list(
    synthetic_data = synthetic_data,
    jth_preprocessing = jth_preprocessing,
    total_synthesis_time = total_synthesis_time,
    jth_synthesis_time = jth_synthesis_time,
    extractions = extractions,
    ldiversity = ldiversity
  )
  
  # create class
  postsynth <- structure(postsynth, class = "postsynth")
  
  return(postsynth)
  
}

is_postsynth <- function(x) {
  inherits(x, "postsynth")
}



# validator
# validate_postsynth <- function() {
#   
#   
#   
# }

#' @export
print.postsynth <- function(x, ...) {

  cat("Postsynth \n") 
  cat(
    base::sprintf(
      "Synthetic Data: %s synthetic observations, %s variables \n", 
      dim(x$synthetic_data)[1], 
      dim(x$synthetic_data)[2]
    )
  )
  cat(base::sprintf("Total Synthesis Time: %s seconds", 
                    x$total_synthesis_time))
  
  invisible(x)

}
