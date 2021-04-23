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

# Get random sample of 7 contributors --------------------------
ICR_allocation <- data.table(Name = c(sample(oxcgrt_datacollection$Name, 7, replace = F)))

# Which states are they currently allocated to update? --------------------------------
ICR_allocation <- oxcgrt_datacollection[ICR_allocation, on = .(Name), .(Name, Update)]

ICR_allocation[, Update := str_replace_all(Update, "_ALL", "")]

ICR_allocation <- ICR_allocation[, Allocation := lapply(ICR_allocation$Update, function(x) sample(str_subset(oxcgrtdata$RegionCode, x, negate = T), 1))
                              ][,.(Name, Allocation)]

# Generate random set of indicators --------------------------
indicators <- c(unlist(lapply(seq(1:8), function(x) paste0("C", x))), 
                unlist(lapply(seq(1:2), function(x) paste0("E", x))),
                unlist(lapply(c(seq(1:3), 6,7), function(x) paste0("H", x))))

ICR_allocation[, Indicator := sample(indicators,7, replace = T)]
 
# output to a csv ------------------------
fwrite(ICR_allocation, "./InterCoder_Allocation_US.csv")
