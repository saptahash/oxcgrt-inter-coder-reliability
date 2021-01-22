library(googlesheets4)
library(data.table)
library(stringr)

# Read Data collection/latest allocation sheet ----------------
oxcgrt_datacollection_url <- "https://docs.google.com/spreadsheets/d/1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI/edit#gid=2022852213"

oxcgrt_datacollection <- googlesheets4::range_read("1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI", sheet= "19 January 2021")

# List all the countries that are currently being allocated -----------
oxcgrtdata <- unique(fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv",
                           select = c("CountryCode")))

# get list of contributors who have been allocated recently

# get random sample of 10 contributors and allocate 10 countries to them
ICR_allocation <- 
  data.table(Name = c(sample(oxcgrt_datacollection$Name, 15, replace = F)), 
              Country = c(sample(oxcgrtdata$CountryCode, 15, replace = F)))

oxcgrt_datacollection <- as.data.table(oxcgrt_datacollection)

ICR_allocation <- merge(ICR_allocation, oxcgrt_datacollection[,.(Name, Update)], by= "Name")

ICR_allocation <- ICR_allocation[,.(Allocation = str_extract_all(Update, "[A-Z]{3}"), Update, Name, Country)
                                 ][,.(Allocation = strsplit(Update, ","), Update, Country, Name)
                                   ][,.(Allocation2 = Country,Update, Country, Name) 
                                     ][,.(Repeat = Country %in% Allocation2)]

, list(Allocation = unlist(str_extract_all(Update, [A-Z]^3)))]


# find list of sheet names
#googlesheets4::sheet_names("1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI")