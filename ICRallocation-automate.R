library(googlesheets4)
library(data.table)
library(stringr)

# Read Data collection/latest allocation sheet ----------------
oxcgrt_datacollection_url <- "https://docs.google.com/spreadsheets/d/1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI/edit#gid=2022852213"

oxcgrt_datacollection <- googlesheets4::range_read("1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI", sheet= "19 January 2021")
oxcgrt_datacollection <- as.data.table(oxcgrt_datacollection)

# List all the countries that are currently being allocated -----------
oxcgrtdata <- unique(fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv",
                           select = c("CountryCode")))

# remove any NAs from update column
oxcgrt_datacollection <- oxcgrt_datacollection[!is.na(Update)]

# Get random sample of 10 contributors
ICR_allocation <- data.table(Name = c(sample(oxcgrt_datacollection$Name, 15, replace = F)))

# Which countries are they currently allocated to update? 
ICR_allocation <- oxcgrt_datacollection[ICR_allocation, on = .(Name), .(Name, Update)]

# Cleaning up Update column in case of collateral strings like "Build from.."
ICR_allocation <- ICR_allocation[, Update := str_remove_all(Update, " ")
                                 ][, Update := ifelse(str_length(Update) > 15, str_extract(Update, "[A-Z]{3}"), Update)
                                   ][, Update := str_replace(Update, ",", "|")]

# Generating random allocations and Drop Update Allocation
ICR_allocation <- ICR_allocation[, Allocation := lapply(ICR_allocation$Update, function(x) sample(str_subset(oxcgrtdata$CountryCode, x, negate = T), 1))
                                 ][,.(Name, Allocation)] 

# output to a csv 
fwrite(ICR_allocation, "./InterCoder_Allocation.csv")

