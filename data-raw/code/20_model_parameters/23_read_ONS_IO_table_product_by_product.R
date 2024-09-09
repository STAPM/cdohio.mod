#### Read in the ONS product by product input-output tables and corresponding
#### supply and use tables for years 2017 - 2020

source("data-raw/code/03_load_packages.R")

## run the script files for each year

source("data-raw/code/20_model_parameters/23a_ONS_table_2017.R")
source("data-raw/code/20_model_parameters/23b_ONS_table_2018.R")
source("data-raw/code/20_model_parameters/23c_ONS_table_2019.R")
source("data-raw/code/20_model_parameters/23d_ONS_table_2020.R")

##################################################################
########## write out the datasets ################################

usethis::use_data(inputoutput_2017, overwrite = TRUE)
usethis::use_data(inputoutput_2018, overwrite = TRUE)
usethis::use_data(inputoutput_2019, overwrite = TRUE)
usethis::use_data(inputoutput_2020, overwrite = TRUE)
