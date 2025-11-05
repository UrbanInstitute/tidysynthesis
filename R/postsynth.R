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
#' @noRd
#' 
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

#' Print the postsynth object to the console with formatting
#'
#' @param x A `postsynth` object
#' @param ... further arguments passed to or from other methods (not currently
#'   used).
#' 
#' @return Invisibly returns the input `postsynth` object.
#'
#' @examples
#' 
#' # create roadmap
#' rm <- roadmap(
#'   conf_data = acs_conf_nw,
#'   start_data = acs_start_nw
#' ) 
#' 
#' rpart_mod_reg <- parsnip::decision_tree() |>
#'   parsnip::set_engine(engine = "rpart") |>
#'   parsnip::set_mode(mode = "regression")
#' 
#' rpart_mod_class <- parsnip::decision_tree() |>
#'   parsnip::set_engine(engine = "rpart") |>
#'   parsnip::set_mode(mode = "classification")
#' 
#' synth_spec1 <- synth_spec(
#'   default_regression_model = rpart_mod_reg,
#'   default_regression_sampler = sample_rpart,
#'   default_classification_model = rpart_mod_class,
#'   default_classification_sampler = sample_rpart
#' )
#' 
#' # create a presynth object
#' # use defaults for noise, constraints, and replicates
#' presynth1 <- presynth(
#'   roadmap = rm,
#'   synth_spec = synth_spec1
#' )
#' 
#' # synthesize!
#' set.seed(1)
#' postsynth1 <- synthesize(presynth = presynth1)
#'
#' print(postsynth1)
#'
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
