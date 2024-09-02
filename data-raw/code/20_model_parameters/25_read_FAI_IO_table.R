#### FAI Alcohol-disaggregated input-output table ####

source("data-raw/code/03_load_packages.R")

sheet <- "Sheet1"

###################################################
### Download the excel sheet from Strathclyde #####

url <- "https://pureportal.strath.ac.uk/files/86400329/2010_UK_Alcohol_consumption_disaggregated_IxI.xlsx"
temp <- tempfile()
temp <- curl_download(url = url, destfile = temp, quiet = FALSE, mode = "wb")

###################################################
### Read in the required elements of the table ####

### Read in sector names

sector <- read_excel(temp,
                     sheet = sheet,
                     range = "B6:C111",
                     col_names = FALSE) %>%
  dplyr::rename(IOC = `...1`,
                Sector = `...2`) %>% as_tibble


### Read in the flowtable

flowtable <- read_excel(temp,
                        sheet = sheet,
                        range = "D6:DE111",
                        col_names = FALSE) %>% as.matrix

#### Read in household demand

hhold.demand <- read_excel(temp,
                           sheet = sheet,
                           range = "DG6:DG111",
                           col_names = FALSE) %>% as.matrix %>% as.vector

#### Read in gross value added

gva.total <- read_excel(temp,
                        sheet = sheet,
                        range = "D119:DE119",
                        col_names = FALSE) %>% as.matrix %>% as.vector

#### Read in compensation of employees

gva.coe <- read_excel(temp,
                      sheet = sheet,
                      range = "D117:DE117",
                      col_names = FALSE) %>% as.matrix %>% as.vector

#### Read in total output

total.output <- read_excel(temp,
                           sheet = sheet,
                           range = "D120:DE120",
                           col_names = FALSE) %>% as.matrix %>% as.vector

rm(temp, sheet, url)

#########################################################
##### Calculate the multipliers for output and gva ######

####################################
### (1) calculate Leontief inverse

## A matrix - matrix of technical coefficients

A <- flowtable %*% ((total.output )^-1 * diag(length(total.output)))

## Leontief Inverse L = (I - A)^-1

L1 <- solve(diag(length(total.output)) - A)

## Leontief for direct effects - an identity matrix

L0 <- diag(length(total.output))

####################################
### (1) output multipliers

output_multiplier_type0 <- rep(NA,length(total.output))
output_multiplier_type1 <- rep(NA,length(total.output))

for (i in 1:length(total.output)) {
  output_multiplier_type0[i] <- sum(L0[,i])
  output_multiplier_type1[i] <- sum(L1[,i])
}

