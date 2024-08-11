#### Alcohol Input Data Processing (MESAS/HMRC) ####

source("data-raw/code/03_load_packages.R")

### Data needed:

## Alcohol sales
## Alcohol unit prices
##

#############################################################
#### Volume of pure alcohol (1000L) sold - MESAS ####

# Scotland
mesas_scot_ontrade_volume <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                       sheet = "Scotland data",
                                       range = "B4:AD13",
                                       na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "Scotland",
         sector = "on-trade")

mesas_scot_offtrade_volume <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                        sheet = "Scotland data",
                                        range = "AF4:BH13",
                                        na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "Scotland",
         sector = "off-trade")

# England & Wales
mesas_ew_ontrade_volume <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                     sheet = "England & Wales data",
                                     range = "B5:AD13",
                                     na = "-",
                                     c("(000 litres)",
                                       as.character(seq(from = 1994,to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "England & Wales",
         sector = "on-trade")

mesas_ew_offtrade_volume <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                      sheet = "England & Wales data",
                                      range = "AF5:BH13",
                                      na = "-",
                                      col_names = c("(000 litres)",
                                                    as.character(seq(from = 1994,to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "England & Wales",
         sector = "off-trade")

# Combine volumes sold by country and sector together
mesas_volume <- rbind(mesas_scot_ontrade_volume,
                      mesas_scot_offtrade_volume,
                      mesas_ew_ontrade_volume,
                      mesas_ew_offtrade_volume)

rm(mesas_scot_ontrade_volume,
   mesas_scot_offtrade_volume,
   mesas_ew_ontrade_volume,
   mesas_ew_offtrade_volume)

########################################################
#### Average price per unit of alcohol sold - MESAS ####

# Scotland
mesas_scot_ontrade_unitprice <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                          sheet = "Scotland data",
                                          range = "B43:AD52",
                                          na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "Scotland",
         sector = "on-trade")

mesas_scot_offtrade_unitprice <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                           sheet = "Scotland data",
                                           range = "AF43:BH52",
                                           na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "Scotland",
         sector = "off-trade")

# England & Wales
mesas_ew_ontrade_unitprice <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                        sheet = "England & Wales data",
                                        range = "B44:AD52",
                                        na = "-",
                                        col_names = c("£ per unit of alcohol",
                                                      as.character(seq(from = 1994, to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "England & Wales",
         sector = "on-trade")

mesas_ew_offtrade_unitprice <- read_xlsx(path = "data-raw/data/Alc_mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                         sheet = "England & Wales data",
                                         range = "AF44:BH52",
                                         na = "-",
                                         col_names = c("£ per unit of alcohol",
                                                       as.character(seq(from = 1994, to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "England & Wales",
         sector = "off-trade")

# Combine average prices by country and sector together
mesas_unitprice <- rbind(mesas_scot_ontrade_unitprice,
                         mesas_scot_offtrade_unitprice,
                         mesas_ew_ontrade_unitprice,
                         mesas_ew_offtrade_unitprice)

rm(mesas_scot_ontrade_unitprice,
   mesas_scot_offtrade_unitprice,
   mesas_ew_ontrade_unitprice,
   mesas_ew_offtrade_unitprice)

#############################
#### MESAS Alcohol Sales ####

setDT(mesas_unitprice)
setDT(mesas_volume)

# Join alcohol volume to unit price by alcohol type, year, country and sector
mesas <- merge(mesas_volume, mesas_unitprice, by = c("year","country","sector","alcohol_type"))

# Change classes from character to numeric for volume and unit price
class(mesas$volume)
mesas$volume <- as.numeric(mesas$volume)
class(mesas$unitprice)
mesas$unitprice <- as.numeric(mesas$unitprice)

# Calculate sales in £m
# sales = 1000L * 1000 * 100 * price per unit / 100000
# sales = 1000L * price per unit / 10

mesas[, sales := (volume * unitprice) / 10]

########################################
### Combine into four alcohol groups


# Need to reclassify MESAS data from 8 categories to 4 categories used in HMRC
# tax receipts for duties
# spirits = c("Spirits", "RTDs", "Other") - assume that other is in spirits category
# beer = c("Beer")
# wines = c("Fortified Wines" "Wine")
# cider = c("Cider", "Perry")

mesas[alcohol_type %in% c("Spirits", "RTDs", "Other"), alcohol_category := "spirits"]
mesas[alcohol_type %in% c("Beer"),                     alcohol_category := "beer"]
mesas[alcohol_type %in% c("Fortified Wines", "Wine"),  alcohol_category := "wine"]
mesas[alcohol_type %in% c("Cider", "Perry"),           alcohol_category := "cider"]

mesas <- mesas[alcohol_category %in% c("spirits","beer","wine","cider"),]

# Recalculate sales data by alcohol type
mesas <- mesas[, .(exp_alcohol_mn = sum(sales, na.rm = TRUE)), by = c("year","alcohol_category")]

mesas <- mesas[year >= 2010,]

########################################################
#### Disaggregated Tax Receipts - HMRC Tax Receipts ####

#### MESAS only has GB alcohol spending. Use disaggregated tax receipts
#### by UK nation to estimate inflation factors for spending.
####
#### MESAS EXP = TOTAL EXP * (1 - NI%)

# Northern Ireland
tax_receipts_ni <- read_xlsx(path = "data-raw/data/Disaggregated_tax_and_NICs_receipts_-_statistics_table.xlsx",
                             range = "A150:Z170") %>%
  rename(year = "Northern Ireland") %>%
  select("year", spirits = "Spirits duties", beer = "Beer duties", wine = "Wines duties", cider = "Cider duties") %>%
  mutate(calendar_year = substr(year, 1, 4),
         country = "Northern Ireland") %>%
  summarise(beer = mean(beer),
            cider = mean(cider),
            wine = mean(wine),
            spirits = mean(spirits))

beer_ni_pct    <- as.numeric(tax_receipts_ni[1,"beer"])
cider_ni_pct   <- as.numeric(tax_receipts_ni[1,"cider"])
wine_ni_pct    <- as.numeric(tax_receipts_ni[1,"wine"])
spirits_ni_pct <- as.numeric(tax_receipts_ni[1,"spirits"])

# Tidy
rm(tax_receipts_ni)

mesas[alcohol_category == "beer",    exp_alcohol_mn := exp_alcohol_mn/(1 - beer_ni_pct)]
mesas[alcohol_category == "cider",   exp_alcohol_mn := exp_alcohol_mn/(1 - cider_ni_pct)]
mesas[alcohol_category == "wine",    exp_alcohol_mn := exp_alcohol_mn/(1 - wine_ni_pct)]
mesas[alcohol_category == "spirits", exp_alcohol_mn := exp_alcohol_mn/(1 - spirits_ni_pct)]

mesas[, year := as.numeric(year)]

###############################
#### HMRC Alcohol Bulletin ####

# This contains more up-to-date data on alcohol tax receipts but without country-level disaggregation

alc_tax_year_wine <- read_xlsx(path = "data-raw/data/Alc_Tabs_Jul_23.xlsx",
                               sheet = "Wine_Duty_(wine)_tables",
                               range = "A8:J32")
alc_tax_year_wine <- alc_tax_year_wine[, c(1,9)]
names(alc_tax_year_wine) <- c("tax_year", "wine")

alc_tax_year_spirits <- read_xlsx(path = "data-raw/data/Alc_Tabs_Jul_23.xlsx",
                                  sheet = "Spirits_Duty_tables",
                                  range = "A7:J31")
alc_tax_year_spirits <- alc_tax_year_spirits[, c(1,9)]
names(alc_tax_year_spirits) <- c("tax_year", "spirits")

alc_tax_year_beer_cider <- read_xlsx(path = "data-raw/data/Alc_Tabs_Jul_23.xlsx",
                                     sheet = "Beer_Duty_and_Cider_Duty_tables",
                                     range = "A8:J32")
alc_tax_year_beer_cider <- alc_tax_year_beer_cider[, c(1,9:10)]
names(alc_tax_year_beer_cider) <- c("tax_year", "beer", "cider")

alc_tax_year_types <- list(alc_tax_year_wine, alc_tax_year_spirits, alc_tax_year_beer_cider)
alc_receipts_data <- Reduce(function(x, y) merge(x, y, all=FALSE), alc_tax_year_types)

alc_receipts_data$year <- str_sub(alc_receipts_data$tax_year, 1, 4) # assume 2021-22 tax year = 2021
alc_receipts_data$year <- as.numeric(alc_receipts_data$year)

alc_receipts_data_long <- pivot_longer(alc_receipts_data, cols = "wine":"cider", names_to = "alcohol_category", values_to = "tax")
alc_receipts_data_long <- filter(alc_receipts_data_long, year >= 2010)


#############################################
##### Merge tax data to expenditure data ####

alcohol_expenditure <- merge(mesas, alc_receipts_data_long, by = c("year","alcohol_category"))

alcohol_expenditure[, "tax_year" := NULL]

alcohol_expenditure[, tax_pct := tax/exp_alcohol_mn]

alcohol_expenditure[, exp_alcohol_mn_bp := exp_alcohol_mn*(1 - tax_pct)]

alcohol_expenditure <- alcohol_expenditure[, c("year","alcohol_category","exp_alcohol_mn","exp_alcohol_mn_bp","tax_pct")]

## write out the data

usethis::use_data(alcohol_expenditure, overwrite = TRUE)
