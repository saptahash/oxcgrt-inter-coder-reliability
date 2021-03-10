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
  mutate(country = str_extract(country, pattern = "^[A-Z]{3}"),
         Date = lubridate::ymd(Date))

oxcgrtdata <- fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_combined.csv")
oxcgrtdata <- as.data.frame(oxcgrtdata)

country_list <- unique(results_form$country)

oxcgrtdata <- 
  oxcgrtdata %>%
  filter(CountryCode %in% country_list & RegionCode == "") %>%
  select(-contains(c("Index", "Confirmed", "M1", "H4", "H5", "E3", "E4", "numeric"))) %>%
  mutate(Date = lubridate::ymd(Date))

results_form <- left_join(results_form, oxcgrtdata, by = c("country" = "CountryCode", 
                                                           "Date" = "Date"))


results_form <- 
  results_form %>%
  filter(!indicator %in% c("H7", "H8")) %>%
  mutate(code = str_trim(code),
         indicator = str_extract(indicator, pattern = "^[A-Z][0-9]"),
         code1 = ifelse(indicator %in% c("C7", "C8", "H2", "H3", "E2"),
                        str_extract(code, pattern = "^[0-9]"),
                        str_extract(code, pattern = "(^[0-9][A-Z]|^[0-9]$)")))

results_form$indicator1 <- 
  unlist(lapply(results_form$indicator, 
              function(x) { 
                ifelse(length(str_subset(names(results_form), x)) == 0, NA_character_, str_subset(names(results_form), x))
                }
              )) 


results_form$original_code <- 
 unlist( 
  lapply(1:nrow(results_form),
       function(x){
         temp <- (results_form$indicator1)[x]
         if(!is.na(temp)){
           results_form[x,] %>% ungroup() %>% pull({{ temp }})  
         } else {
           NA
         }
       })
  )

# calculate the agreement percentage -----------------------
results_form <- 
  results_form %>%
  filter(!is.na(original_code) & original_code != "") %>%
  mutate(agreement = ifelse(code1 == original_code, 1, 0))

mean(results_form$agreement, na.rm = T)

results_form %>%
  group_by(indicator1) %>%
  summarise(agreement = mean(agreement, na.rm = T),
            n = n())

# create the preliminaries of the matrix ---------------

## names of rows/columns --
spec_indicator <- select(results_form[results_form$indicator == "C2",], indicator, code1, original_code)
dims <- unique(c(spec_indicator$code1, spec_indicator$original_code))

spec_indicator <- 
  spec_indicator %>%
  group_by(code1, original_code) %>%
  summarise(n = n()) %>%
  mutate(n= ifelse(code1 != original_code, n, 2*n))

A <- matrix(NA, nrow = length(dims), ncol = length(dims), byrow = T)

rownames(A) <- dims
colnames(A) <- dims

for(i in 1:nrow(spec_indicator)){
  A[(spec_indicator$code1)[i], (spec_indicator$original_code)[i]] = (spec_indicator$n)[i]
  A[(spec_indicator$original_code)[i], (spec_indicator$code1)[i]] = (spec_indicator$n)[i]
}

