library(googlesheets4)
library(data.table)
library(stringr)
library(lubridate)
# Read Data collection/latest allocation sheet ----------------
#oxcgrt_datacollection_url <- "https://docs.google.com/spreadsheets/d/1mWtOwn07HLJClOtjrNNrX3y4AjB7WdVCJuU1DwwP284/edit#gid=1856435511"

# Process Date to pick out latest sheet -------------------------
current_date <- Sys.Date()
dayofweek <- as.POSIXlt(current_date)$wday
alloc_date <- current_date - ifelse((dayofweek - 2) >= 0, dayofweek - 2, dayofweek + 5)
m <- month.name[month(alloc_date)]
y <- year(alloc_date)
d <- mday(alloc_date)
sheet_name <- paste(d, m, y)

oxcgrt_datacollection <- googlesheets4::range_read("1mWtOwn07HLJClOtjrNNrX3y4AjB7WdVCJuU1DwwP284", sheet= "April 20 Allocation")
oxcgrt_datacollection <- as.data.table(oxcgrt_datacollection)

# List all the states that are currently being published -----------
oxcgrtdata <- unique(fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv",
                           select = c("CountryCode", "RegionCode")))

oxcgrtdata <- oxcgrtdata[CountryCode == "USA" & RegionCode != ""
                         ][, RegionCode := str_extract(RegionCode, "[A-Z][A-Z]$")
                           ][, .(RegionCode)]

# remove any NAs from update column--------------------
oxcgrt_datacollection <- as.data.table(oxcgrt_datacollection)
oxcgrt_datacollection <- oxcgrt_datacollection[!is.na(Update)]

# Get random sample of 15 contributors --------------------------
ICR_allocation <- data.table(Name = c(sample(oxcgrt_datacollection$Name, 15, replace = F)))

# Which countries are they cuirrently allocated to update? --------------------------------
ICR_allocation <- oxcgrt_datacollection[ICR_allocation, on = .(Name), .(Name, Update, `Countries in pod`)]
