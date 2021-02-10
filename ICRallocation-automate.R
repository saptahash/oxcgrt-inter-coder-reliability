library(googlesheets4)
library(data.table)
library(stringr)
library(lubridate)

# Read Data collection/latest allocation sheet ----------------
#oxcgrt_datacollection_url <- "https://docs.google.com/spreadsheets/d/1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI/edit#gid=2022852213"

# Process Date to pick out latest sheet -------------------------
current_date <- Sys.Date()
dayofweek <- as.POSIXlt(current_date)$wday
alloc_date <- current_date - ifelse((dayofweek - 2) >= 0, dayofweek - 2, dayofweek + 5)
m <- month.name[month(alloc_date)]
y <- year(alloc_date)
d <- mday(alloc_date)
sheet_name <- paste(d, m, y)

oxcgrt_datacollection <- googlesheets4::range_read("1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI", sheet= sheet_name)
oxcgrt_datacollection <- as.data.table(oxcgrt_datacollection)

# Get Pod IDs for the contributors --------------------------------
contributor_pods <- googlesheets4::range_read("1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI", sheet= "Who does what")
contributor_pods <- as.data.table(contributor_pods)[!is.na(Name),.(Name, Group)]
                                                    
contributor_pods[, PodID := unlist(lapply(contributor_pods$Group, function(x) ifelse(!is.na(x), str_extract(x, "[0-9]*$"), NA_character_)))]

# Get country set of pods ------------------------------
pods <- googlesheets4::range_read("1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI", sheet= "Pods")
cols <- c(PodID = "Pod ID", "Countries in pod")
pods <- as.data.table(pods)[,..cols][,`Pod ID` := as.character(`Pod ID`)][!is.na(`Pod ID`)]

# merge pods-countries to contributor pods --------------------------
contributor_pods <- merge(contributor_pods, pods, by.x = "PodID", by.y = "Pod ID")

# merge contributors-pods-countries to countributors-allocations ------------------
oxcgrt_datacollection <- merge(oxcgrt_datacollection, contributor_pods, by = "Name")

# List all the countries that are currently being allocated -----------
oxcgrtdata <- unique(fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv",
                           select = c("CountryCode")))

# remove any NAs from update column--------------------
oxcgrt_datacollection <- oxcgrt_datacollection[!is.na(Update)]

# Get random sample of 15 contributors --------------------------
ICR_allocation <- data.table(Name = c(sample(oxcgrt_datacollection$Name, 15, replace = F)))

# Which countries are they cuirrently allocated to update? --------------------------------
ICR_allocation <- oxcgrt_datacollection[ICR_allocation, on = .(Name), .(Name, Update, `Countries in pod`)]

# Cleaning up Update column in case of collateral strings like "Build from.." -------------------------
ICR_allocation <- ICR_allocation[, Update := str_remove_all(Update, " ")
                                 ][, Update := ifelse(str_length(Update) > 15, str_extract(Update, "[A-Z]{3}"), Update)
                                   ][, Update := str_replace_all(Update, ",", "|")
                                     ][, `Countries in pod` := str_remove_all(`Countries in pod`, " ")
                                       ][, `Countries in pod` := str_replace_all(`Countries in pod`, ",", "|")
                                         ][, pod_countries := lapply(ICR_allocation$`Countries in pod`, function(x) paste0(str_subset(oxcgrtdata$CountryCode, x), collapse = ","))]

# Generating random allocations and Drop Update Allocation -----------------------------
outside_pod <- ICR_allocation[, Allocation := lapply(ICR_allocation$Update, function(x) sample(str_subset(oxcgrtdata$CountryCode, x, negate = T), 1))
                                 ][,.(Name, Allocation)
                                   ][, within_pod := 0][1:8]

# Generating random allocations within Pods ---------------------------------
within_pod <- ICR_allocation[, Allocation := mapply(function(x,y) sample(str_subset(unlist(str_split(x, ",")), y, negate = T),1), 
                                                     x = ICR_allocation$pod_countries, 
                                                     y = ICR_allocation$Update)
                              ][,.(Name, Allocation)
                                ][, within_pod := 1][9:15]

# Combine the two 
ICR_allocation <- rbind(outside_pod, within_pod)

# output to a csv ------------------------
fwrite(ICR_allocation, "./InterCoder_Allocation.csv")
