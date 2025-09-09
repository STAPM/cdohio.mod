#################################################################
### Read in the central government distribution of expenditures

### 2022 (not only 104 categories here, need to expand the dimension)

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb24.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

govt_2022 <- readxl::read_excel(temp, sheet = "Table 2 - Final Demand 2022", range = "A5:E109")

setnames(govt_2022, names(govt_2022), c("CPA","Product","hhold","npish","govt_2022"))

govt_2022 <- govt_2022[, c("CPA","Product","govt_2022")]

### 2021 (not only 104 categories here, need to expand the dimension)

temp <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables/current/supublicationtablesbb24.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

govt_2021 <- readxl::read_excel(temp, sheet = "Table 2 - Final Demand 2021", range = "A5:E109")

setnames(govt_2021, names(govt_2021), c("CPA","Product","hhold","npish","govt_2021"))

govt_2021 <- govt_2021[, c("CPA","Product","govt_2021")]

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

#-----------------------------------------#
## for 2021 and 2022 Weapons and ammunition has been merged with
## Fabricated metal products, excl. machinery and equipment.
##
## These are zeroes anyway, so keep the original 2 from 2017-2020, drop
## the combined category in the merge, and set NAs equal to zero

#-----------------------------------------#

########## Merge

govt_spending <- merge(govt_2017, govt_2018, by = c("CPA","Product"), sort = FALSE)
govt_spending <- merge(govt_spending, govt_2019, by = c("CPA","Product"), sort = FALSE)
govt_spending <- merge(govt_spending, govt_2020, by = c("CPA","Product"), sort = FALSE)
govt_spending <- merge(govt_spending, govt_2021, by = c("CPA","Product"), sort = FALSE, all.x = TRUE)
govt_spending <- merge(govt_spending, govt_2022, by = c("CPA","Product"), sort = FALSE, all.x = TRUE)
setDT(govt_spending)

## preserve the original order
cats <- unique(govt_2017$CPA)
govt_spending[, CPA := factor(CPA, levels = cats)]
govt_spending <- govt_spending[order(CPA),]

## zero the CPA_254 and CPA25OTHER categories
govt_spending[CPA == "CPA_C254", c("govt_2021","govt_2022") := 0]
govt_spending[CPA == "CPA_C25OTHER", c("govt_2021","govt_2022") := 0]

## calculate proportions
govt_spending[, govt_2017 := govt_2017/sum(govt_2017)]
govt_spending[, govt_2018 := govt_2018/sum(govt_2018)]
govt_spending[, govt_2019 := govt_2019/sum(govt_2019)]
govt_spending[, govt_2020 := govt_2020/sum(govt_2020)]
govt_spending[, govt_2021 := govt_2021/sum(govt_2021)]
govt_spending[, govt_2022 := govt_2022/sum(govt_2022)]

usethis::use_data(govt_spending, overwrite = TRUE)
