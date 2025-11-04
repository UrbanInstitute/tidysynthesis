#' Convert `NA` values to `"NA"` for categorical variables
#'
#' @param data A data frame or tibble
#'
#' @return A data frame or tibble with `NA` converted to `"NA"`
#' 
#' @examples
#' 
#' data <- data.frame(
#'   x1 = c(1, 2, NA),
#'   x2 = c("1", "2", NA),
#'   x3 = factor(c("1", "2", NA)),
#'   x4 = factor(c("b", NA, "a"), levels = c("b", NA, "a"), ordered = TRUE)
#' )
#' 
#' convert_na_to_level(data)
#' 
#' @export
convert_na_to_level <- function(data) {
  
  na_to_level <- function(x) {
    
    # do nothing if x isn't a character or factor
    if (!pillar::type_sum(x) %in% c("chr", "ord", "fct")) return(x) 
    
    # test if NA is already a level
    if (sum(x == "NA", na.rm = TRUE) > 0) {
      stop("Can't convert NA to level 'NA' because level 'NA' already exists")
    }
    
    # replace `NA` with `"NA"`
    if (all(!is.na(x))) {
      
      return(x)
      
    } else if (pillar::type_sum(x) == "chr") {
      
      x <- tidyr::replace_na(data = x, replace = "NA")
      
    } else if (pillar::type_sum(x) %in% c("ord", "fct")) {
      
      ordinal_flag <- pillar::type_sum(x) == "ord"
      
      # store the factor levels
      x_levels <- c(levels(x), "NA")
      
      # convert to character and replace the NA
      x_chr <- as.character(x)
      
      x_chr <- tidyr::replace_na(data = x_chr, replace = "NA")
      
      # convert back to a factor
      x <- factor(x_chr, levels = x_levels, ordered = ordinal_flag)
      
    }
    
    return(x)
    
  }

  data_converted <- data |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = na_to_level))
  
  return(data_converted)
  
}



