### The purpose of this code is to create a dataset of income tax and national insurance parameters for 2010-2020
### https://www.gov.uk/government/collections/tax-structure-and-parameters-statistics

library(readxl)
library(data.table)

## read in raw data and clean

data <- readxl::read_excel(path = "data-raw/data/income tax and nics.xlsx",
                           sheet = "Sheet1",
                           range = "A1:M14")

setDT(data)

data[, basic_rate := as.numeric(basic_rate)]
data[, higher_thresh := as.numeric(higher_thresh)]
data[, higher_rate := as.numeric(higher_rate)]
data[, add_thresh := as.numeric(add_thresh)]
data[, add_rate := as.numeric(add_rate)]

income_tax_params <- copy(data)

usethis::use_data(income_tax_params, overwrite = TRUE)
