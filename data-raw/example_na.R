library(tidyverse)

set.seed(20240710)

# data set size
n <- 200

# create combinations of the categorical variables
cat_combinations <- expand_grid(
  age = 10:65,
  sex = factor(c("female", "male")),
  labor_force = factor(c("not in labor force", "in labor force")),
  health = factor(c("good", "mediocre", "bad"), ordered = TRUE)
)

# create beginning of data
example_na <- cat_combinations |>
  slice_sample(n = n, replace = TRUE)

# add hours and wages
example_na <- example_na |>
  mutate(hours = 35 + sample(-10:10, size = n, replace = TRUE))

example_na<- example_na |>
  mutate(wages = hours * rlnorm(n = n, meanlog = 3, sd = 1.3))

# add missing probability
# roughly 10% of observations will be MAR on average
example_na <- example_na |>
  mutate(prob = 0.05 + 0.05 * (sex == "female") + 0.05 * (labor_force == "not in labor force"))

# add MAR for age
example_na <- example_na |>
  mutate(
    sex = if_else(
      runif(n = nrow(example_na)) < prob, 
      NA, 
      sex
    )
  )

# add structural missingness for labor_force
example_na <- example_na |>
  mutate(labor_force = if_else(age < 16, NA, labor_force))

# add MAR for health based on the row number
example_na <- example_na |>
  mutate(health = if_else(row_number() %in% c(1:5, 196:200), NA, health))

# add hours with MAR
example_na <- example_na |>
  mutate(
    hours = if_else(
      runif(n = nrow(example_na)) < prob, 
      NA, 
      hours
    )
  )

# add wages with structural missingness
example_na <- example_na |>
  mutate(wages = if_else(labor_force == "not in labor force", NA, wages))

example_na <- example_na |>
  mutate(
    age = as.double(age)
  ) |>
  select(-prob)

example_na <- as_tibble(example_na)

usethis::use_data(example_na, overwrite = TRUE)
