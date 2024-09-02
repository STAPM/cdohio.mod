#### RPI inflation ####

source("data-raw/code/03_load_packages.R")

###########################################
### Download the excel sheet from ONS #####

url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/chaw/mm23"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")

rpi <- read_excel(temp,
                  range = "A9:B45",
                  col_names = FALSE) %>% setDT()

setnames(rpi, names(rpi), c("year","rpi_index"))

rpi[, year := as.numeric(year)]

usethis::use_data(rpi, overwrite = TRUE)
