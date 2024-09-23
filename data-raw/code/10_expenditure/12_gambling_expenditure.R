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

########################################################
#### Disaggregated Tax Receipts - HMRC Tax Receipts ####

#### Gambling Commission data only has GB ggy spending. Use disaggregated tax receipts
#### by UK nation to estimate inflation factors for spending.
####
#### GC EXP = TOTAL EXP * (1 - NI%)

# Northern Ireland
tax_receipts_ni <- read_xlsx(path = "data-raw/data/Disaggregated_tax_and_NICs_receipts_-_statistics_table.xlsx",
                             range = "A150:AA170") %>%
  rename(year = "Northern Ireland") %>%
  select("year", duty = "Betting & Gaming") %>%
  mutate(calendar_year = substr(year, 1, 4),
         country = "Northern Ireland") %>%
  filter(calendar_year %in% c("2015","2016","2017","2018") ) %>%
  summarise(duty = mean(duty))

gambling_duty_ni_pct    <- as.numeric(tax_receipts_ni[1,"duty"])

# Adjust GB to a UK estimate
gambling_expenditure[, ggy_mn := ggy_mn/(1 - gambling_duty_ni_pct)]


usethis::use_data(gambling_expenditure, overwrite = TRUE)
