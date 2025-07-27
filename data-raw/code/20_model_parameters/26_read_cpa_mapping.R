################################################################################
### read in a mapping from the 103 CPA categories in the HHEFCE datasets to the
### 105 CPA categories in the input-output table

cpa_conversion_matrix <- readxl::read_excel("data-raw/data/cpa_mapping.xlsx",
                                            range = "A1:D106")

setDT(cpa_conversion_matrix)

usethis::use_data(cpa_conversion_matrix)

#################################################################
### Read in the central government distribution of expenditures

### 2020

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

govt_2020 <- readxl::read_excel(temp, sheet = "Table 2 - Final Demand 2020", range = "A5:E110")

setnames(govt_2020, names(govt_2020), c("CPA","Product","hhold","npish","govt_2020"))

govt_2020 <- govt_2020[, c("CPA","Product","govt_2020")]

### 2019

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

govt_2019 <- readxl::read_excel(temp, sheet = "Table 2 - Final Demand 2019", range = "A5:E110")

setnames(govt_2019, names(govt_2019), c("CPA","Product","hhold","npish","govt_2019"))

govt_2019 <- govt_2019[, c("CPA","Product","govt_2019")]

### 2018

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

govt_2018 <- readxl::read_excel(temp, sheet = "Table 2 - Final Demand 2018", range = "A5:E110")

setnames(govt_2018, names(govt_2018), c("CPA","Product","hhold","npish","govt_2018"))

govt_2018 <- govt_2018[, c("CPA","Product","govt_2018")]

### 2017

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb23v2.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

govt_2017 <- readxl::read_excel(temp, sheet = "Table 2 - Final Demand 2017", range = "A5:E110")

setnames(govt_2017, names(govt_2017), c("CPA","Product","hhold","npish","govt_2017"))

govt_2017 <- govt_2017[, c("CPA","Product","govt_2017")]

########## Merge

govt_spending <- merge(govt_2017, govt_2018, by = c("CPA","Product"))
govt_spending <- merge(govt_spending, govt_2019, by = c("CPA","Product"))
govt_spending <- merge(govt_spending, govt_2020, by = c("CPA","Product"))

setDT(govt_spending)

govt_spending[, govt_2017 := govt_2017/sum(govt_2017)]
govt_spending[, govt_2018 := govt_2018/sum(govt_2018)]
govt_spending[, govt_2019 := govt_2019/sum(govt_2019)]
govt_spending[, govt_2020 := govt_2020/sum(govt_2020)]

usethis::use_data(govt_spending, overwrite = TRUE)
