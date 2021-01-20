library(googlesheets4)

oxcgrt_datacollection_url <- "https://docs.google.com/spreadsheets/d/1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI/edit#gid=2022852213"

oxcgrt_datacollection <- googlesheets4::range_read("1D2ZJcmX0LQVzW9kiyyRrIN8SeuqMlcfcaX9eyK2vqfI", sheet= "19 January 2021")

