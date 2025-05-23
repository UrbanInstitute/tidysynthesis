#' Convert `"NA"` values to `NA` for categorical variables
#'
#' @param data A data frame or tibble
#'
#' @return A data frame or tibble with `"NA"` converted to `NA`
#' 
#' @export
#' 
convert_level_to_na <- function(data) {
  
  level_to_na <- function(x) {
    
    # do nothing if x isn't a character or factor
    if (!pillar::type_sum(x) %in% c("chr", "ord", "fct")) return(x) 
    
    # test if NA is already a level
    if (sum(is.na(x)) > 0) {
      stop("Vector already contains NA; cannot call convert_level_to_NA")
    }
    
    # replace `NA` with `"NA"`
    if (pillar::type_sum(x) == "chr") {
      
      x <- dplyr::na_if(x = x, y = "NA")
      
    } else if (pillar::type_sum(x) %in% c("ord", "fct")) {
      
      ordinal_flag <- pillar::type_sum(x) == "ord"
      
      # store the factor levels
      x_levels <- levels(x)[!levels(x) == "NA"]
      
      # convert to character and replace the NA
      x_chr <- as.character(x)
      
      x_chr <- dplyr::na_if(x = x_chr, y = "NA")
      
      # convert back to a factor
      x <- factor(x_chr, levels = x_levels, ordered = ordinal_flag)
      
    }
    
    return(x)
    
  }

  data_converted <- data %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = level_to_na))
  
  return(data_converted)
  
}



