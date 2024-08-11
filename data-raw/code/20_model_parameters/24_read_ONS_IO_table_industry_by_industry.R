#### Read in the ONS industry by industry input-output table for 2020
#### Use this to estimate changes in employment by sector for a given change
#### in output estimated from the product by product table

source("data-raw/code/03_load_packages.R")

#########################
##### 2020 ##############

## https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesindustrybyindustry

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesindustrybyindustry/current/iot2020industry.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### total output

i_by_i_table_output <- readxl::read_excel(temp,
                                          sheet = "IOT",
                                          range = "C120:DC120",
                                          col_names = FALSE) %>% as.matrix %>% as.vector
### total coe

i_by_i_table_coe    <- readxl::read_excel(temp,
                                          sheet = "IOT",
                                          range = "C116:DC116",
                                          col_names = FALSE) %>% as.matrix %>% as.vector

#########################################################################
### Get COE coefficient:
### change in output X change in COE coefficient = change in COE

coe_coefficient_i_by_i <- i_by_i_table_coe / i_by_i_table_output
#saveRDS(coe_coefficient_i_by_i, "data/processed/coe_industry_coefs.rds")

##############################################################################
### Get employment/COE ratio:
### multiply by 2020 employment/coe ratio to get the employment coefficients

source("data-raw/code/20_model_parameters/21_labour_force_survey_employment.R")

emp <- as.vector(as.matrix( lfs_empl[,"tot_emp"] ))
fte <- as.vector(as.matrix( lfs_empl[,"tot_fte"] ))

emp_ratio <- emp / i_by_i_table_coe
fte_ratio <- fte / i_by_i_table_coe

## COE is zero for "imputed rents of owner-occupied dwellings"
## so set this equal to zero in the ratios

emp_ratio[78] <- 0
fte_ratio[78] <- 0

#saveRDS(emp_ratio, "data/processed/employment_coefs.rds")
#saveRDS(fte_ratio, "data/processed/fte_coefs.rds")

fte_coefficients <- coe_coefficient_i_by_i * fte_ratio

usethis::use_data(fte_coefficients, overwrite = TRUE)
