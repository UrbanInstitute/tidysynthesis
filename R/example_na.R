#' A df with different types of missingness
#' 
#'@format A tibble with `r nrow(example_na)` observations
#'   and `r ncol(example_na)` variables:
#' \describe{
#' \item{age}{Age of respondent}
#' \item{sex}{Sex of respondent with missingness at random}
#' \item{labor_force}{Labor force status of respondent with structural missingness}
#' \item{hours}{Hours work of respondent with missingness at random}
#' \item{wages}{Wages earned with structural missingness}
#' }
#'
"example_na"