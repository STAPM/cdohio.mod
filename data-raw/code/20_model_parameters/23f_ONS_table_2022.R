#########################
##### 2022 ##############

### supply table (need to map to the 104 categories to the 105 categories in the
### input-output table and previous years supply tables) - C25 was once split up
### into two categories

### get 2020 from the older version of the tables

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_supply_2020 <- readxl::read_excel(temp,
                                                     sheet = "Table 1 - Supply 2020",
                                                     range = "A3:B108")
setnames(product_categories_supply_2020, "...1", "CPA")

### get 2022

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb24.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

product_categories_supply <- readxl::read_excel(temp,
                                                sheet = "Table 1 - Supply 2022",
                                                range = "A3:B107")
setnames(product_categories_supply, "...1", "CPA")

### Make a 105-category lookup

setDT(product_categories_supply_2020)
setDT(product_categories_supply)

product_categories_supply_2020[, year := 2020]
product_categories_supply[, year := 2022]

product_categories_supply_merged <- merge(product_categories_supply,
                                          product_categories_supply_2020,
                                          by = c("CPA","Product"),
                                          all = TRUE,
                                          sort = FALSE)

cpa     <- as.character(product_categories_supply_merged[is.na(year.y), "CPA"])
product <- as.character(product_categories_supply_merged[is.na(year.y), "Product"])

product_categories_supply_2020[CPA %in% c("CPA_C254","CPA_C25OTHER"), CPA := cpa ]
product_categories_supply_2020[CPA %in% c("CPA_C25","CPA_C25"), Product := product ]
product_categories_supply_2020[, year := NULL]

product_categories_supply <- copy(product_categories_supply_2020)
rm(product_categories_supply_2020, cpa, product, product_categories_supply_merged)

### Now do the supply table

supply <- readxl::read_excel(temp,
                             sheet = "Table 1 - Supply 2022",
                             range = "A3:K107")[, c(1:3,8:11)] %>% setDT

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

supply <- supply[, c("output","supply","margins","imports") := NULL]

## map to the 105 categories

supply <- merge(product_categories_supply, supply, by = c("CPA","Product"))

supply[37, Product := "Weapons and ammunition"]
supply[37, CPA := "CPA_C254"]
supply[38, Product := "Fabricated metal products, excl. machinery and equipment and weapons & ammunition - 25.1-3/25.5-9"]
supply[38, CPA := "CPA_C25OTHER"]

### HHFCe - household final consumption expenditure

product_categories_hhfce <- readxl::read_excel(temp,
                                               sheet = "Table 3 - HHFCe 2022",
                                               range = "A4:B107")
setnames(product_categories_hhfce, "...1", "CPA_hhfce")

hhfce <- as.matrix(readxl::read_excel(temp,
                                      sheet = "Table 3 - HHFCe 2022",
                                      range = "C5:AL107",
                                      col_names = FALSE) )

hhfce_totals <- as.matrix(readxl::read_excel(temp,
                                             sheet = "Table 3 - HHFCe 2022",
                                             range = "C108:AL108",
                                             col_names = FALSE) )

hhfce_categories <- as.matrix( readxl::read_excel(temp,
                                                  sheet = "Table 3 - HHFCe 2022",
                                                  range = "C3:AL4",
                                                  col_names = FALSE) )

hhfce_categories <- t(hhfce_categories)

for (i in 1:ncol(hhfce)){
  hhfce[,i] <- hhfce[,i]/hhfce_totals[,i]
}
hhfce[,33] <- 0 # package holiday all zeroes

############################
### input-output table #####

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2022/iot2022product.xlsx"
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


inputoutput_2022 <- list(product_categories_io = product_categories_io,
                         product_categories_hhfce = product_categories_hhfce,
                         supply = supply,
                         iot = iot,
                         A = A,
                         L = L,
                         L2 = L2,
                         output = out,
                         gva = gva,
                         tax_on_products = supply$tax,
                         tax_on_production = tax,
                         gva_coefficients = gva_coefficients,
                         tax_coefficients = tax_coefficients,
                         coe_coefficients = coe_coefficients,
                         hhfce_categories = hhfce_categories,
                         hhfce = hhfce,
                         hhfce_totals = hhfce_totals)
