library(ipumsr)

set.seed(20241210)

# IPUMPS API extract definition --------------------------------------------

message(
  "This script assumes you have an IPUMS API key already set up within your 
  global environment. If not, use ipumsr::set_ipums_api_key() to do so."
)

acs_extract_request <- ipumsr::define_extract_micro(
  description = "Example 2019 Nebraska ACS Extract",
  collection = "usa",
  samples = c("us2019a"),
  variables = list(
    var_spec("STATEFIP", case_selections = c("31")),
    var_spec("COUNTYFIP"),
    var_spec("GQ"),
    var_spec("SEX"),
    var_spec("MARST"),
    var_spec("HCOVANY"),
    var_spec("EMPSTAT"),
    var_spec("CLASSWKR"),
    var_spec("AGE"),
    var_spec("FAMSIZE"),
    var_spec("TRANTIME"),
    var_spec("INCTOT")
  ),
  data_structure = "rectangular"
)
submitted_extract <- ipumsr::submit_extract(acs_extract_request)
downloadable_extract <- ipumsr::wait_for_extract(submitted_extract)
path_to_data_files <- ipumsr::download_extract(downloadable_extract)

data <- ipumsr::read_ipums_micro(path_to_data_files)

# Gold-standard dataset pre-processing / cleaning -------------------------

acs <- data %>%
  dplyr::transmute(
    county = haven::as_factor(COUNTYFIP) %>%
      forcats::fct_recode(
        "Douglas" = "55", 
        "Lancaster" = "109", 
        "Sarpy" = "153", 
        "Other" = "County not identifiable from public-use data (1950-onward)"
      ), 
    gq = haven::as_factor(GQ) %>%
      forcats::fct_collapse(
        "Household" = c("Households under 1970 definition",
                        "Additional households under 1990 definition",
                        "Additional households under 2000 definition"),
        "Institution" = c("Group quarters--Institutions"),
        "Other GQ" = c("Other group quarters", "Vacant unit", "Fragment")
      ),
    sex = haven::as_factor(SEX) %>% 
      forcats::fct_drop(only = "Missing/blank"),
    marst = haven::as_factor(MARST) %>% 
      forcats::fct_collapse(
        "Married" = c("Married, spouse present", 
                      "Married, spouse absent"),
        "Divorced" = c("Divorced"),
        "Separated" = c("Separated"),
        "Widowed" = c("Widowed"), 
        "Single" = c("Never married/single")
      ) %>%
      forcats::fct_drop(only = "Blank, missing"),
    hcovany = haven::as_factor(HCOVANY),
    empstat = haven::as_factor(EMPSTAT) %>%
      forcats::fct_collapse(
        "Employed" = c("Employed"), 
        "Unemployed" = c("Unemployed"), 
        "Not in labor force" = c("Not in labor force", "N/A")
      ) %>% 
      forcats::fct_drop(only = "Unknown/Illegible") %>%
      forcats::fct_recode(NULL = "Not in labor force"),
    classwkr = haven::as_factor(CLASSWKR) %>%
      forcats::fct_drop(only = "Unknown"),
    age = as.double(AGE),
    famsize = as.double(FAMSIZE),
    transit_time = as.double(TRANTIME),
    inctot = dplyr::if_else(INCTOT == 9999999, NA, as.double(INCTOT))
  ) %>% 
  dplyr::slice_sample(n = 2000) %>%
  dplyr::mutate(wgt = pmin(stats::rgamma(2000, 1, 1) + 1, 10))

acs_conf <- acs[1:1500, ]
acs_conf_nw <- acs[1:1500, ] %>% dplyr::select(-wgt)
acs_start <- acs[1501:2000, c("county", "gq", "sex", "marst", "wgt")]
acs_start_nw <- acs[1501:2000, c("county", "gq", "sex", "marst")]

usethis::use_data(acs_conf, overwrite = TRUE)
usethis::use_data(acs_conf_nw, overwrite = TRUE)
usethis::use_data(acs_start, overwrite = TRUE)
usethis::use_data(acs_start_nw, overwrite = TRUE)
