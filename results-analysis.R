library(dplyr)
library(readr)
library(writexl)
library(janitor)
library(data.table)


results_form <- readxl::read_xlsx("./inter-coder-reliability-data.xlsx")

# select necessary columns from results form 
results_form <- 
  janitor::clean_names(results_form) %>%
  select(name = name_of_contributor, 
         country = which_country_are_you_reporting_on, 
         Date = what_is_the_date_of_the_policy_that_you_are_reporting_on,
         indicator = which_indicator_are_you_reporting_on,
         code = what_code_would_you_assign_to_this_policy_that_you_are_reporting_on_i_e_1t_2g_0_etc)

results_form <- 
  results_form %>%
  mutate(country = str_extract(country, pattern = "^[A-Z]{3}"))

oxcgrtdata <- fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
oxcgrtdata <- as.data.frame(oxcgrtdata)


