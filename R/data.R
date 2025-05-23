#' American Community Survey confidential microdata (with weights)
#'
#' An extract constructed from the 2019 American Community Survey containing a 
#' survey sample of n = 1500 Nebraska respondents, with survey weights included.
#'
#' Original data source:
#' Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, 
#' Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. 
#' IPUMS USA: Version 15.0 \[dataset\]. Minneapolis, MN: IPUMS, 2024. 
#' https://doi.org/10.18128/D010.V15.0
#'
#' @format ## `acs_conf`
#' A data frame with 1,500 rows and 12 columns:
#' \describe{
#'   \item{county}{fct, county}
#'   \item{gq}{fct, group quarter kind}
#'   \item{sex}{fct, sex}
#'   \item{marst}{fct, marital status}
#'   \item{hcovany}{fct, health insurance status}
#'   \item{empstat}{fct, employment status; contains empty levels.}
#'   \item{classwkr}{fct, employment kind (ex: self-employed, etc.); 
#'   contains "N/A" levels.}
#'   \item{age}{dbl, age (in years)}
#'   \item{famsize}{dbl, household/family size}
#'   \item{transit_time}{dbl, transit time to work (in minutes)}
#'   \item{inctot}{dbl, annual income; contains missing values}
#'   \item{wgt}{dbl, survey weight}
#' }
#' @source <https://usa.ipums.org/usa/>
"acs_conf"

#' American Community Survey confidential microdata (without weights)
#'
#' An extract constructed from the 2019 American Community Survey containing a 
#' survey sample of n = 1500 Nebraska respondents, with survey weights included.
#'
#' Original data source:
#' Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, 
#' Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. 
#' IPUMS USA: Version 15.0 \[dataset\]. Minneapolis, MN: IPUMS, 2024. 
#' https://doi.org/10.18128/D010.V15.0
#'
#' @format ## `acs_conf_nw`
#' A data frame with 1,500 rows and 11 columns:
#' \describe{
#'   \item{county}{fct, county}
#'   \item{gq}{fct, group quarter kind}
#'   \item{sex}{fct, sex}
#'   \item{marst}{fct, marital status}
#'   \item{hcovany}{fct, health insurance status}
#'   \item{empstat}{fct, employment status; contains empty levels.}
#'   \item{classwkr}{fct, employment kind (ex: self-employed, etc.); 
#'   contains "N/A" levels.}
#'   \item{age}{dbl, age (in years)}
#'   \item{famsize}{dbl, household/family size}
#'   \item{transit_time}{dbl, transit time to work (in minutes)}
#'   \item{inctot}{dbl, annual income; contains missing values}
#' }
#' @source <https://usa.ipums.org/usa/>
"acs_conf_nw"


#' American Community Survey starting microdata (with weights)
#'
#' An extract constructed from the 2019 American Community Survey containing a 
#' survey sample of n = 1500 Nebraska respondents, with survey weights included.
#'
#' Original data source:
#' Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, 
#' Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. 
#' IPUMS USA: Version 15.0 \[dataset\]. Minneapolis, MN: IPUMS, 2024. 
#' https://doi.org/10.18128/D010.V15.0
#'
#' @format ## `acs_start`
#' A data frame with 1,500 rows and 5 columns:
#' \describe{
#'   \item{county}{fct, county}
#'   \item{gq}{fct, group quarter kind}
#'   \item{sex}{fct, sex}
#'   \item{marst}{fct, marital status}
#'   \item{wgt}{dbl, survey weight}
#' }
#' @source <https://usa.ipums.org/usa/>
"acs_start"

#' American Community Survey starting microdata (without weights)
#'
#' An extract constructed from the 2019 American Community Survey containing a 
#' survey sample of n = 1500 Nebraska respondents, with survey weights included.
#'
#' Original data source:
#' Steven Ruggles, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen, 
#' Grace Cooper, Stephanie Richards, Renae Rogers, and Megan Schouweiler. 
#' IPUMS USA: Version 15.0 \[dataset\]. Minneapolis, MN: IPUMS, 2024. 
#' https://doi.org/10.18128/D010.V15.0
#'
#' @format ## `acs_start_nw`
#' A data frame with 1,500 rows and 4 columns:
#' \describe{
#'   \item{county}{fct, county}
#'   \item{gq}{fct, group quarter kind}
#'   \item{sex}{fct, sex}
#'   \item{marst}{fct, marital status}
#' }
#' @source <https://usa.ipums.org/usa/>
"acs_start_nw"
