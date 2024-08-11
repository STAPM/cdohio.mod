#### Gambling Input Data Processing ####

source("data-raw/code/03_load_packages.R")

######################################
#### process data from gov.uk - source = gambling commission
####
#### https://www.data.gov.uk/dataset/e7032815-5990-4439-b5c8-8553cf5b7fdd/gb-gambling-industry-statistics-february-2024
#### url <- "https://assets.ctfassets.net/j16ev64qyf6l/2h48ZNVuVwR7QzwQK14Jot/512880c3f3760d2dd750b04dab12767d/Industry_statistics_-_February_2024_-_Correction.xlsx"
####
#### Gambling expenditure is estimated as the Gross Gambling Yield (GGY)


gambling_ggy <- read_xlsx(path = "data-raw/data/Gambling_Statistics_Feb24.xlsx",
                          sheet = "1",
                          range = "A8:M23")[,c(1,5:13)] %>% setDT

setnames(gambling_ggy, names(gambling_ggy)[1], "year")

gambling_ggy[, year := substr(year,5,8)]
gambling_ggy[, year := as.numeric(year)]


gambling_expenditure <- melt(gambling_ggy,
                             id.vars = "year",
                             variable.name = "gambling_category",
                             value.name = "ggy_mn")

usethis::use_data(gambling_expenditure, overwrite = TRUE)
