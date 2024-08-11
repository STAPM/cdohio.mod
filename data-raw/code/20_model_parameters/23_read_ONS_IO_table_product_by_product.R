#### Read in the ONS product by product input-output tables and corresponding
#### supply and use tables for years 2017 - 2020

source("data-raw/code/03_load_packages.R")

#########################
##### 2020 ##############

### supply table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_supply <- readxl::read_excel(temp,
                                                sheet = "Table 1 - Supply 2020",
                                                range = "A3:B108")
setnames(product_categories_supply, "...1", "CPA")

supply <- readxl::read_excel(temp,
                             sheet = "Table 1 - Supply 2020",
                             range = "A3:K108")[, c(1:3,8:11)] %>% setDT

setnames(supply, names(supply), c("CPA","Product","output","imports","margins","tax","supply"))

supply[, output_to_supply := supply/output]
supply[, supply_to_output := output/supply]
supply[, tax_pct := tax/supply]
supply[, margins_pct := margins/supply]
supply[, import_pct := imports/supply]

### Retail trade is an exception, since just a distributor of other products
### set equal to zero as produces missings otherwise
supply[CPA == "CPA_G47", output_to_supply := 0]
supply[CPA == "CPA_G47", supply_to_output := 0]
supply[CPA == "CPA_G47", tax_pct := 0]
supply[CPA == "CPA_G47", margins_pct := 0]
supply[CPA == "CPA_G47", import_pct := 0]

supply <- supply[, c("output","supply","tax","margins","imports") := NULL]

### HHFCe - household final consumption expenditure

product_categories_hhfce <- readxl::read_excel(temp,
                                               sheet = "Table 3 - HHFCe 2020",
                                               range = "A4:B107")
setnames(product_categories_hhfce, "...1", "CPA_hhfce")

hhfce <- as.matrix(readxl::read_excel(temp,
                                      sheet = "Table 3 - HHFCe 2020",
                                      range = "C5:AL107",
                                      col_names = FALSE) )

hhfce_totals <- as.matrix(readxl::read_excel(temp,
                                             sheet = "Table 3 - HHFCe 2020",
                                             range = "C108:AL108",
                                             col_names = FALSE) )

hhfce_categories <- as.matrix( readxl::read_excel(temp,
                                                  sheet = "Table 3 - HHFCe 2020",
                                                  range = "C3:AL4",
                                                  col_names = FALSE) )

hhfce_categories <- t(hhfce_categories)

for (i in 1:ncol(hhfce)){
  hhfce[,i] <- hhfce[,i]/hhfce_totals[,i]
}
hhfce[,33] <- 0 # package holiday all zeroes


### input-output table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2020/iot2020product.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_io <- readxl::read_excel(temp,
                                            sheet = "IOT",
                                            range = "A6:B111")

iot <- as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C7:DC111", col_names = FALSE) )
A   <- as.matrix( readxl::read_excel(temp, sheet = "A", range = "C8:DC112", col_names = FALSE) )
L   <- solve(diag(nrow(A)) - A)

out    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C120:DC120", col_names = FALSE) ))
gva    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C119:DC119", col_names = FALSE) ))
tax    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C118:DC118", col_names = FALSE) ))
coe    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C116:DC116", col_names = FALSE) ))
hh_exp <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "DG7:DG111", col_names = FALSE) ))

gva_coefficients <- gva/out
tax_coefficients <- tax/out
coe_coefficients <- coe/out

# leontief type 2

coe2 <- c(coe,0)

iot2 <- cbind(iot,hh_exp)
iot2 <- rbind(iot2,coe2)

tot_hh_exp <- as.numeric( readxl::read_excel(temp, sheet = "IOT", range = "DG115:DG115", col_names = FALSE) )

out2 <- c(out,tot_hh_exp)

A2 <- iot2/out2
L2  <- solve(diag(nrow(A2)) - A2)


inputoutput_2020 <- list(product_categories_io = product_categories_io,
                         product_categories_hhfce = product_categories_hhfce,
                         supply = supply,
                         iot = iot,
                         A = A,
                         L = L,
                         L2 = L2,
                         gva_coefficients = gva_coefficients,
                         tax_coefficients = tax_coefficients,
                         coe_coefficients = coe_coefficients,
                         hhfce_categories = hhfce_categories,
                         hhfce = hhfce,
                         hhfce_totals = hhfce_totals)

##########################
##### 2019 ###############

### supply table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_supply <- readxl::read_excel(temp,
                                                sheet = "Table 1 - Supply 2019",
                                                range = "A3:B108")
setnames(product_categories_supply, "...1", "CPA")

supply <- readxl::read_excel(temp,
                             sheet = "Table 1 - Supply 2019",
                             range = "A3:K108")[, c(1:3,8:11)] %>% setDT

setnames(supply, names(supply), c("CPA","Product","output","imports","margins","tax","supply"))

supply[, output_to_supply := supply/output]
supply[, supply_to_output := output/supply]
supply[, tax_pct := tax/supply]
supply[, margins_pct := margins/supply]
supply[, import_pct := imports/supply]

### Retail trade is an exception, since just a distributor of other products
### set equal to zero as produces missings otherwise
supply[CPA == "CPA_G47", output_to_supply := 0]
supply[CPA == "CPA_G47", supply_to_output := 0]
supply[CPA == "CPA_G47", tax_pct := 0]
supply[CPA == "CPA_G47", margins_pct := 0]
supply[CPA == "CPA_G47", import_pct := 0]

supply <- supply[, c("output","supply","tax","margins","imports") := NULL]

### HHFCe - household final consumption expenditure

product_categories_hhfce <- readxl::read_excel(temp,
                                               sheet = "Table 3 - HHFCe 2019",
                                               range = "A4:B107")
setnames(product_categories_hhfce, "...1", "CPA_hhfce")

hhfce <- as.matrix(readxl::read_excel(temp,
                                      sheet = "Table 3 - HHFCe 2019",
                                      range = "C5:AL107",
                                      col_names = FALSE) )

hhfce_totals <- as.matrix(readxl::read_excel(temp,
                                             sheet = "Table 3 - HHFCe 2019",
                                             range = "C108:AL108",
                                             col_names = FALSE) )

hhfce_categories <- as.matrix( readxl::read_excel(temp,
                                                  sheet = "Table 3 - HHFCe 2019",
                                                  range = "C3:AL4",
                                                  col_names = FALSE) )

hhfce_categories <- t(hhfce_categories)

for (i in 1:ncol(hhfce)){
  hhfce[,i] <- hhfce[,i]/hhfce_totals[,i]
}
hhfce[,33] <- 0 # package holiday all zeroes

### input-output table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2019/nasu1719pr.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_io <- readxl::read_excel(temp,
                                            sheet = "IOT",
                                            range = "A6:B111")

iot <- as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C7:DC111", col_names = FALSE) )
A   <- as.matrix( readxl::read_excel(temp, sheet = "A", range = "C8:DC112", col_names = FALSE) )
L   <- solve(diag(nrow(A)) - A)

out    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C120:DC120", col_names = FALSE) ))
gva    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C119:DC119", col_names = FALSE) ))
tax    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C118:DC118", col_names = FALSE) ))
coe    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C116:DC116", col_names = FALSE) ))
hh_exp <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "DG7:DG111", col_names = FALSE) ))

gva_coefficients <- gva/out
tax_coefficients <- tax/out
coe_coefficients <- coe/out

# leontief type 2

coe2 <- c(coe,0)

iot2 <- cbind(iot,hh_exp)
iot2 <- rbind(iot2,coe2)

tot_hh_exp <- as.numeric( readxl::read_excel(temp, sheet = "IOT", range = "DG115:DG115", col_names = FALSE) )

out2 <- c(out,tot_hh_exp)

A2 <- iot2/out2
L2  <- solve(diag(nrow(A2)) - A2)


inputoutput_2019 <- list(product_categories_io = product_categories_io,
                         product_categories_hhfce = product_categories_hhfce,
                         supply = supply,
                         iot = iot,
                         A = A,
                         L = L,
                         L2 = L2,
                         gva_coefficients = gva_coefficients,
                         tax_coefficients = tax_coefficients,
                         coe_coefficients = coe_coefficients,
                         hhfce_categories = hhfce_categories,
                         hhfce = hhfce,
                         hhfce_totals = hhfce_totals)

##########################
##### 2018 ###############

### supply table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_supply <- readxl::read_excel(temp,
                                                sheet = "Table 1 - Supply 2018",
                                                range = "A3:B108")
setnames(product_categories_supply, "...1", "CPA")

supply <- readxl::read_excel(temp,
                             sheet = "Table 1 - Supply 2018",
                             range = "A3:K108")[, c(1:3,8:11)] %>% setDT

setnames(supply, names(supply), c("CPA","Product","output","imports","margins","tax","supply"))

supply[, output_to_supply := supply/output]
supply[, supply_to_output := output/supply]
supply[, tax_pct := tax/supply]
supply[, margins_pct := margins/supply]
supply[, import_pct := imports/supply]

### Retail trade is an exception, since just a distributor of other products
### set equal to zero as produces missings otherwise
supply[CPA == "CPA_G47", output_to_supply := 0]
supply[CPA == "CPA_G47", supply_to_output := 0]
supply[CPA == "CPA_G47", tax_pct := 0]
supply[CPA == "CPA_G47", margins_pct := 0]
supply[CPA == "CPA_G47", import_pct := 0]

supply <- supply[, c("output","supply","tax","margins","imports") := NULL]

### HHFCe - household final consumption expenditure

product_categories_hhfce <- readxl::read_excel(temp,
                                               sheet = "Table 3 - HHFCe 2018",
                                               range = "A4:B107")
setnames(product_categories_hhfce, "...1", "CPA_hhfce")

hhfce <- as.matrix(readxl::read_excel(temp,
                                      sheet = "Table 3 - HHFCe 2018",
                                      range = "C5:AL107",
                                      col_names = FALSE) )

hhfce_totals <- as.matrix(readxl::read_excel(temp,
                                             sheet = "Table 3 - HHFCe 2018",
                                             range = "C108:AL108",
                                             col_names = FALSE) )

hhfce_categories <- as.matrix( readxl::read_excel(temp,
                                                  sheet = "Table 3 - HHFCe 2018",
                                                  range = "C3:AL4",
                                                  col_names = FALSE) )

hhfce_categories <- t(hhfce_categories)

for (i in 1:ncol(hhfce)){
  hhfce[,i] <- hhfce[,i]/hhfce_totals[,i]
}
hhfce[,33] <- 0 # package holiday all zeroes

### input-output table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2018/nasu1719pr.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_io <- readxl::read_excel(temp,
                                            sheet = "IOT",
                                            range = "A6:B111")

iot <- as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C7:DC111", col_names = FALSE) )
A   <- as.matrix( readxl::read_excel(temp, sheet = "A", range = "C8:DC112", col_names = FALSE) )
L   <- solve(diag(nrow(A)) - A)

out    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C120:DC120", col_names = FALSE) ))
gva    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C119:DC119", col_names = FALSE) ))
tax    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C118:DC118", col_names = FALSE) ))
coe    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C116:DC116", col_names = FALSE) ))
hh_exp <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "DG7:DG111", col_names = FALSE) ))

gva_coefficients <- gva/out
tax_coefficients <- tax/out
coe_coefficients <- coe/out

# leontief type 2

coe2 <- c(coe,0)

iot2 <- cbind(iot,hh_exp)
iot2 <- rbind(iot2,coe2)

tot_hh_exp <- as.numeric( readxl::read_excel(temp, sheet = "IOT", range = "DG115:DG115", col_names = FALSE) )

out2 <- c(out,tot_hh_exp)

A2 <- iot2/out2
L2  <- solve(diag(nrow(A2)) - A2)


inputoutput_2018 <- list(product_categories_io = product_categories_io,
                         product_categories_hhfce = product_categories_hhfce,
                         supply = supply,
                         iot = iot,
                         A = A,
                         L = L,
                         L2 = L2,
                         gva_coefficients = gva_coefficients,
                         tax_coefficients = tax_coefficients,
                         coe_coefficients = coe_coefficients,
                         hhfce_categories = hhfce_categories,
                         hhfce = hhfce,
                         hhfce_totals = hhfce_totals)

##########################
##### 2017 ###############

### supply table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_supply <- readxl::read_excel(temp,
                                                sheet = "Table 1 - Supply 2017",
                                                range = "A3:B108")
setnames(product_categories_supply, "...1", "CPA")

supply <- readxl::read_excel(temp,
                             sheet = "Table 1 - Supply 2017",
                             range = "A3:K108")[, c(1:3,8:11)] %>% setDT

setnames(supply, names(supply), c("CPA","Product","output","imports","margins","tax","supply"))

supply[, output_to_supply := supply/output]
supply[, supply_to_output := output/supply]
supply[, tax_pct := tax/supply]
supply[, margins_pct := margins/supply]
supply[, import_pct := imports/supply]

### Retail trade is an exception, since just a distributor of other products
### set equal to zero as produces missings otherwise
supply[CPA == "CPA_G47", output_to_supply := 0]
supply[CPA == "CPA_G47", supply_to_output := 0]
supply[CPA == "CPA_G47", tax_pct := 0]
supply[CPA == "CPA_G47", margins_pct := 0]
supply[CPA == "CPA_G47", import_pct := 0]

supply <- supply[, c("output","supply","tax","margins","imports") := NULL]

### HHFCe - household final consumption expenditure

product_categories_hhfce <- readxl::read_excel(temp,
                                               sheet = "Table 3 - HHFCe 2017",
                                               range = "A4:B107")
setnames(product_categories_hhfce, "...1", "CPA_hhfce")

hhfce <- as.matrix(readxl::read_excel(temp,
                                      sheet = "Table 3 - HHFCe 2017",
                                      range = "C5:AL107",
                                      col_names = FALSE) )

hhfce_totals <- as.matrix(readxl::read_excel(temp,
                                             sheet = "Table 3 - HHFCe 2017",
                                             range = "C108:AL108",
                                             col_names = FALSE) )

hhfce_categories <- as.matrix( readxl::read_excel(temp,
                                                  sheet = "Table 3 - HHFCe 2017",
                                                  range = "C3:AL4",
                                                  col_names = FALSE) )

hhfce_categories <- t(hhfce_categories)

for (i in 1:ncol(hhfce)){
  hhfce[,i] <- hhfce[,i]/hhfce_totals[,i]
}
hhfce[,33] <- 0 # package holiday all zeroes

### input-output table

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2017/nasu1719pr.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_io <- readxl::read_excel(temp,
                                            sheet = "IOT",
                                            range = "A6:B111")

iot <- as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C7:DC111", col_names = FALSE) )
A   <- as.matrix( readxl::read_excel(temp, sheet = "A", range = "C8:DC112", col_names = FALSE) )
L   <- solve(diag(nrow(A)) - A)

out    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C120:DC120", col_names = FALSE) ))
gva    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C119:DC119", col_names = FALSE) ))
tax    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C118:DC118", col_names = FALSE) ))
coe    <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "C116:DC116", col_names = FALSE) ))
hh_exp <- as.vector(as.matrix( readxl::read_excel(temp, sheet = "IOT", range = "DG7:DG111", col_names = FALSE) ))

gva_coefficients <- gva/out
tax_coefficients <- tax/out
coe_coefficients <- coe/out

# leontief type 2

coe2 <- c(coe,0)

iot2 <- cbind(iot,hh_exp)
iot2 <- rbind(iot2,coe2)

tot_hh_exp <- as.numeric( readxl::read_excel(temp, sheet = "IOT", range = "DG115:DG115", col_names = FALSE) )

out2 <- c(out,tot_hh_exp)

A2 <- iot2/out2
L2  <- solve(diag(nrow(A2)) - A2)


inputoutput_2017 <- list(product_categories_io = product_categories_io,
                         product_categories_hhfce = product_categories_hhfce,
                         supply = supply,
                         iot = iot,
                         A = A,
                         L = L,
                         L2 = L2,
                         gva_coefficients = gva_coefficients,
                         tax_coefficients = tax_coefficients,
                         coe_coefficients = coe_coefficients,
                         hhfce_categories = hhfce_categories,
                         hhfce = hhfce,
                         hhfce_totals = hhfce_totals)

##################################################################
########## write out the datasets ################################

usethis::use_data(inputoutput_2017, overwrite = TRUE)
usethis::use_data(inputoutput_2018, overwrite = TRUE)
usethis::use_data(inputoutput_2019, overwrite = TRUE)
usethis::use_data(inputoutput_2020, overwrite = TRUE)
