#### Alcohol Input Data Processing ####

#### Set Up ####
library(readxl)
library(tidyverse)

#### Volume of pure alcohol (1000L) sold - MESAS ####

# Scotland
mesas_scot_ontrade_volume <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                 sheet = "Scotland data",
                                 range = "B4:AD13",
                                 na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "Scotland",
         sector = "on-trade")

mesas_scot_offtrade_volume <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                       sheet = "Scotland data",
                                       range = "AF4:BH13",
                                       na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "Scotland",
         sector = "off-trade")

mesas_scot_combined_volume <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                        sheet = "Scotland data",
                                        range = "BJ4:CL13",
                                        na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "Scotland",
         sector = "combined")

# England & Wales
mesas_ew_ontrade_volume <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                     sheet = "England & Wales data",
                                     range = "B5:AD13",
                                     na = "-",
                                     c("(000 litres)",
                                       as.character(seq(from = 1994,to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "England & Wales",
         sector = "on-trade")

mesas_ew_offtrade_volume <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                      sheet = "England & Wales data",
                                      range = "AF5:BH13",
                                      na = "-",
                                      col_names = c("(000 litres)",
                                                    as.character(seq(from = 1994,to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "England & Wales",
         sector = "off-trade")

mesas_ew_combined_volume <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                      sheet = "England & Wales data",
                                      range = "BJ5:CL13",
                                      na = "-",
                                      col_names = c("(000 litres)",
                                                    as.character(seq(from = 1994,to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "volume") %>%
  rename(alcohol_type = "(000 litres)") %>%
  mutate(country = "England & Wales",
         sector = "combined")

# Combine volumes sold by country and sector together
mesas_volume <- rbind(mesas_scot_ontrade_volume,
                      mesas_scot_offtrade_volume,
                      mesas_scot_combined_volume,
                      mesas_ew_ontrade_volume,
                      mesas_ew_offtrade_volume,
                      mesas_ew_combined_volume)

#### Average price per unit of alcohol sold - MESAS ####

# Scotland
mesas_scot_ontrade_unitprice <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                       sheet = "Scotland data",
                                       range = "B43:AD52",
                                       na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "Scotland",
         sector = "on-trade")

mesas_scot_offtrade_unitprice <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                        sheet = "Scotland data",
                                        range = "AF43:BH52",
                                        na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "Scotland",
         sector = "off-trade")

mesas_scot_combined_unitprice <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                           sheet = "Scotland data",
                                           range = "BJ43:CL52",
                                           na = "-") %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "Scotland",
         sector = "combined")

# England & Wales
mesas_ew_ontrade_unitprice <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                        sheet = "England & Wales data",
                                        range = "B44:AD52",
                                        na = "-",
                                        col_names = c("£ per unit of alcohol",
                                                      as.character(seq(from = 1994, to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "England & Wales",
         sector = "on-trade")

mesas_ew_offtrade_unitprice <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                         sheet = "England & Wales data",
                                         range = "AF44:BH52",
                                         na = "-",
                                         col_names = c("£ per unit of alcohol",
                                                       as.character(seq(from = 1994, to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "England & Wales",
         sector = "off-trade")

mesas_ew_combined_unitprice <- read_xlsx(path = "15_input_data/mesas-monitoring-report-2022-alcohol-sales.xlsx",
                                         sheet = "England & Wales data",
                                         range = "BJ44:CL52",
                                         na = "-",
                                         col_names = c("£ per unit of alcohol",
                                                       as.character(seq(from = 1994, to = 2021)))) %>%
  pivot_longer(cols = "1994":"2021", names_to = "year", values_to = "unitprice") %>%
  rename(alcohol_type = "£ per unit of alcohol") %>%
  mutate(country = "England & Wales",
         sector = "combined")

# Combine average prices by country and sector together
mesas_unitprice <- rbind(mesas_scot_ontrade_unitprice,
                         mesas_scot_offtrade_unitprice,
                         mesas_scot_combined_unitprice,
                         mesas_ew_ontrade_unitprice,
                         mesas_ew_offtrade_unitprice,
                         mesas_ew_combined_unitprice)

#### MESAS Alcohol Sales ####

# Join alcohol volume to unit price by alcohol type, year, country and sector
mesas <- left_join(mesas_volume, mesas_unitprice)

# Change classes from character to numeric for volume and unit price
class(mesas$volume)
mesas$volume <- as.numeric(mesas$volume)
class(mesas$unitprice)
mesas$unitprice <- as.numeric(mesas$unitprice)

# Calculate sales in £m
# sales = 1000L * 1000 * 100 * price per unit / 100000
# sales = 1000L * price per unit / 10

mesas <- mesas %>% mutate(sales = volume * unitprice / 10)

# Tidy
rm(mesas_ew_combined_volume, mesas_ew_combined_unitprice,
   mesas_ew_offtrade_volume, mesas_ew_offtrade_unitprice,
   mesas_ew_ontrade_volume, mesas_ew_ontrade_unitprice)
rm(mesas_scot_combined_volume, mesas_scot_combined_unitprice,
   mesas_scot_offtrade_volume, mesas_scot_offtrade_unitprice,
   mesas_scot_ontrade_volume, mesas_scot_ontrade_unitprice)

#### Disaggregated Tax Receipts - HMRC Tax Receipts ####

# UK
tax_receipts_uk <- read_xlsx(path = "15_input_data/Disaggregated_tax_and_NICs_receipts_-_statistics_table.xlsx",
                             range = "A3:Z23") %>%
  rename(year = "United Kingdom") %>%
  select("year", spirits = "Spirits duties", beer = "Beer duties", wines = "Wines duties", cider = "Cider duties") %>%
  mutate(calendar_year = substr(year, 1, 4),
         value = "total",
         country = "UK")

# England
tax_receipts_e <- read_xlsx(path = "15_input_data/Disaggregated_tax_and_NICs_receipts_-_statistics_table.xlsx",
                             range = "A24:Z65") %>%
  rename(year = "England") %>%
  select("year", spirits = "Spirits duties", beer = "Beer duties", wines = "Wines duties", cider = "Cider duties") %>%
  mutate(calendar_year = substr(year, 1, 4),
         value = if_else(spirits < 1 , "percent", "total"),
         country = "England") %>%
  na.omit()

# Wales
tax_receipts_w <- read_xlsx(path = "15_input_data/Disaggregated_tax_and_NICs_receipts_-_statistics_table.xlsx",
                            range = "A66:Z107") %>%
  rename(year = "Wales") %>%
  select("year", spirits = "Spirits duties", beer = "Beer duties", wines = "Wines duties", cider = "Cider duties") %>%
  mutate(calendar_year = substr(year, 1, 4),
         value = if_else(spirits < 1 , "percent", "total"),
         country = "Wales") %>%
  na.omit()

# Scotland
tax_receipts_s <- read_xlsx(path = "15_input_data/Disaggregated_tax_and_NICs_receipts_-_statistics_table.xlsx",
                            range = "A108:Z149") %>%
  rename(year = "Scotland") %>%
  select("year", spirits = "Spirits duties", beer = "Beer duties", wines = "Wines duties", cider = "Cider duties") %>%
  mutate(calendar_year = substr(year, 1, 4),
         value = if_else(spirits < 1 , "percent", "total"),
         country = "Scotland") %>%
  na.omit()

# Northern Ireland
tax_receipts_ni <- read_xlsx(path = "15_input_data/Disaggregated_tax_and_NICs_receipts_-_statistics_table.xlsx",
                            range = "A150:Z191") %>%
  rename(year = "Northern Ireland") %>%
  select("year", spirits = "Spirits duties", beer = "Beer duties", wines = "Wines duties", cider = "Cider duties") %>%
  mutate(calendar_year = substr(year, 1, 4),
         value = if_else(spirits < 1 , "percent", "total"),
         country = "Northern Ireland") %>%
  na.omit()

# Combine all countries with tax receipts together
tax_receipts <- rbind(tax_receipts_uk,
                      tax_receipts_e,
                      tax_receipts_w,
                      tax_receipts_s,
                      tax_receipts_ni) %>%
  rename(tax_year = year,
         year = calendar_year)

tax_receipts_long <- pivot_longer(data = tax_receipts,
                                  cols = spirits:cider,
                                  names_to = "alcohol_type",
                                  values_to = "tax")

# Tidy
rm(tax_receipts_e, tax_receipts_w, tax_receipts_s, tax_receipts_ni, tax_receipts_uk)


#### HMRC Alcohol Bulletin ####
# This contains more up-to-date data on alcohol tax receipts but without country-level disaggregation

alc_tax_year_wine <- read_xlsx(path = "15_input_data/Alc_Tabs_Jul_23.xlsx",
                               sheet = "Wine_Duty_(wine)_tables",
                               range = "A8:J32")
alc_tax_year_wine <- alc_tax_year_wine[, c(1,9)]
names(alc_tax_year_wine) <- c("tax_year", "wines")

alc_tax_year_spirits <- read_xlsx(path = "15_input_data/Alc_Tabs_Jul_23.xlsx",
                                  sheet = "Spirits_Duty_tables",
                                  range = "A7:J31")
alc_tax_year_spirits <- alc_tax_year_spirits[, c(1,9)]
names(alc_tax_year_spirits) <- c("tax_year", "spirits")

alc_tax_year_beer_cider <- read_xlsx(path = "15_input_data/Alc_Tabs_Jul_23.xlsx",
                                     sheet = "Beer_Duty_and_Cider_Duty_tables",
                                     range = "A8:J32")
alc_tax_year_beer_cider <- alc_tax_year_beer_cider[, c(1,9:10)]
names(alc_tax_year_beer_cider) <- c("tax_year", "beer", "cider")

alc_tax_year_types <- list(alc_tax_year_wine, alc_tax_year_spirits, alc_tax_year_beer_cider)
alc_receipts_data <- Reduce(function(x, y) merge(x, y, all=FALSE), alc_tax_year_types)

alc_receipts_data$year <- str_sub(alc_receipts_data$tax_year, 1, 4) # assume 2021-22 tax year = 2021

alc_receipts_data_long <- pivot_longer(alc_receipts_data, cols = "wines":"cider", names_to = "alcohol_type", values_to = "tax")

# Join the UK totals to main tax receipts data
alc_receipts_data_2019_onwards <- alc_receipts_data_long %>% filter(year >= 2019) %>%
  mutate(country = "UK", value = "total", tax_year = gsub(" to ", "-", tax_year))
tax_receipts_long <- rbind(tax_receipts_long, alc_receipts_data_2019_onwards)

#### Alcohol Types ####

unique(mesas$alcohol_type)
unique(tax_receipts_long$alcohol_type)

# Need to reclassify MESAS data from 8 categories to 4 categories used in HMRC tax receipts for duties
# spirits = c("Spirits", "RTDs", "Other") - assume that other is in spirits category
# beer = c("Beer")
# wines = c("Fortified Wines" "Wine")
# cider = c("Cider", "Perry")

mesas$alcohol_type_mesas <- mesas$alcohol_type

mesas[mesas$alcohol_type %in% c("Spirits", "RTDs", "Other"), ]$alcohol_type <- "spirits"
mesas[mesas$alcohol_type == "Beer", ]$alcohol_type <- "beer"
mesas[mesas$alcohol_type %in% c("Fortified Wines", "Wine"), ]$alcohol_type <- "wines"
mesas[mesas$alcohol_type %in% c("Cider", "Perry"), ]$alcohol_type <- "cider"
mesas[mesas$alcohol_type == "Total", ]$alcohol_type <- "total"

# Recalculate sales data by alcohol type
mesas <- mesas %>% group_by(year, country, sector, alcohol_type) %>%
  summarise(sales = sum(sales, na.rm = T))

# Set any 0s as NAs
mesas$sales[mesas$sales == 0] <- NA


#### Estimation of UK (and Northern Ireland) Alcohol Sales and Tax ####

# Assumption: use the tax country disaggregation from 2018 to distribute tax to countries for 2019-2021
tax_receipts_disagg_2018 <- filter(tax_receipts_long, year == 2018, value == "total", !country == "UK") %>%
  group_by(alcohol_type) %>% mutate(country_percent = prop.table(tax)) %>%
  select(alcohol_type, country, country_percent) %>%
  pivot_wider(names_from = country, values_from = country_percent)

# Disaggregate and recalculate tax for each country for 2019 onwards
alc_receipts_disagg_2019_onwards <- left_join(select(alc_receipts_data_2019_onwards, -country),
                                              tax_receipts_disagg_2018, relationship = "many-to-many")
alc_receipts_disagg_2019_onwards$tax <- alc_receipts_disagg_2019_onwards$tax * alc_receipts_disagg_2019_onwards$country_percent
alc_receipts_disagg_2019_onwards <- alc_receipts_disagg_2019_onwards[,c("tax_year", "year", "value", "country", "alcohol_type", "tax")]

# Add disaggregated country-level tax receipts for 2019 onwards to the main tax receipts data
tax_receipts_long <- rbind(tax_receipts_long, alc_receipts_disagg_2019_onwards)

## England & Wales & Scotland ##
# (These are the country groupings in the MESAS data)
tax_receipts_ews <-  tax_receipts_long %>%
  filter(value == "total", !country %in% c("UK", "Northern Ireland")) %>%
  group_by(year, alcohol_type) %>%
  summarise(tax = sum(tax))

# MESAS equivalent of alcohol sales by alcohol type for England & Wales & Scotland
mesas_combined_sales_ews <- mesas %>%
  filter(sector == "combined", !alcohol_type == "total") %>%
  group_by(year, alcohol_type) %>%
  summarise(sales = sum(sales))

# Combine tax receipts with sales to calculate the proportion of tax for sales in England & Wales & Scotland
sales_receipts_ews <- full_join(mesas_combined_sales_ews, tax_receipts_ews) %>%
  mutate(tax_prop = tax/sales)

## Northern Ireland ##
# Use proportions of tax as part of sales for England & Wales & Scotland combined to estimate total sales for NI
sales_receipts_ni <- tax_receipts_long %>%
  filter(value == "total", country == "Northern Ireland") %>% # filter to NI tax receipts
  group_by(year, alcohol_type) %>%
  summarise(tax = sum(tax)) %>%
  full_join(select(sales_receipts_ews, year, alcohol_type, tax_prop)) %>% # join in the EWS tax proportions
  mutate(sales = tax/tax_prop,           # tax = sales * tax_prop so sales = tax/tax_prop
         country = "Northern Ireland",
         sector = "combined") %>%
  select(year, country, alcohol_type, sector, sales, tax, tax_prop)

## Scotland ##
sales_receipts_s <- tax_receipts_long %>%
  filter(value == "total", country == "Scotland") %>%
  group_by(year, alcohol_type) %>%
  summarise(tax = sum(tax)) %>%
  mutate(country = "Scotland") %>%
  full_join(filter(mesas, sector == "combined", !alcohol_type == "total", country == "Scotland")) %>%
  select(year, country, alcohol_type, sector, sales, tax) %>%
  mutate(tax_prop = tax/sales)

## England & Wales ##
sales_receipts_ew <-  tax_receipts_long %>%
  filter(value == "total", country %in% c("England", "Wales")) %>%
  group_by(year, alcohol_type) %>%
  summarise(tax = sum(tax)) %>%
  mutate(country = "England & Wales") %>%
  full_join(filter(mesas, sector == "combined", !alcohol_type == "total", country == "England & Wales")) %>%
  select(year, country, alcohol_type, sector, sales, tax) %>%
  mutate(tax_prop = tax/sales)

## Combine countries of interest: England & Wales, Scotland and Northern Ireland ###
sales_receipts_combined <- rbind(sales_receipts_ew,
                                 sales_receipts_s,
                                 sales_receipts_ni) %>%
  filter(year > 1999) %>% mutate(year = as.numeric(year))


#### Distribution of Sales and Tax by Sector (On-trade and Off-trade) ####

# Proportion of MESAS sales which are off-trade and on-trade by country (E&W and Scotland) and year
sales_by_sector_prop <- mesas %>%
  filter(year >= 2000, !alcohol_type == "total", !sector == "combined") %>%
  group_by(year, country, alcohol_type) %>%
  mutate(sector_percent = prop.table(sales)) %>%
  select(year, country, alcohol_type, sector, sector_percent)

# England & Wales
sales_receipts_ew_sector <- sales_receipts_ew %>% select(-sector) %>%
  left_join(filter(sales_by_sector_prop, country == "England & Wales")) %>%
  mutate(sales = sales * sector_percent,
         tax = tax * sector_percent)

# Scotland
sales_receipts_s_sector <- sales_receipts_s %>% select(-sector) %>%
  left_join(filter(sales_by_sector_prop, country == "Scotland")) %>%
  mutate(sales = sales * sector_percent,
         tax = tax * sector_percent)

# Northern Ireland (assume the same sector proportions as England & Wales)
sales_receipts_ni_sector <- sales_receipts_ni %>% select(-sector) %>%
  left_join(filter(sales_by_sector_prop, country == "England & Wales") %>% ungroup() %>%  select(-country)) %>%
  mutate(sales = sales * sector_percent,
         tax = tax * sector_percent)


#### Final Alcohol Input Data ####

# Combine all together by country, alcohol type and sector
final_demand_country_alcohol_type_sector <- rbind(sales_receipts_ew_sector,
                                                  sales_receipts_s_sector,
                                                  sales_receipts_ni_sector) %>%
  filter(year %in% seq(2000, 2021, 1)) %>% # no full data for sales and tax until 2000 or for 2022
  mutate(demand = sales - tax) %>%  # demand = sales - tax for demand at basic prices
  select(year, country, sector, alcohol_type, sales, tax)

# Save this file of all the sales and tax data by country, sector, alcohol type
saveRDS(object = final_demand_country_alcohol_type_sector,
        file = "15_input_data/final_demand_country_alcohol_type_sector.rds")

# Read in file (if needed)
final_demand_country_alcohol_type_sector <- readRDS("15_input_data/final_demand_country_alcohol_type_sector.rds")

# Summarise to alcohol type and sector for UK overall
final_demand_alcohol_type_sector <- final_demand_country_alcohol_type_sector %>%
  group_by(year, alcohol_type, sector) %>%
  summarise(sales = sum(sales),
            tax = sum(tax)) %>%
  mutate(year = as.numeric(year))

# UK total sales and tax by sector
final_demand_uk_sector <- final_demand_country_alcohol_type_sector %>%
  group_by(year, sector) %>%
  summarise(sales = sum(sales),
            tax = sum(tax)) %>%
  mutate(year = as.numeric(year))

# Save as a csv file
write.csv(x = final_demand_uk_sector, file = "15_input_data/alcohol_sales_tax_2000_2021.csv", row.names = FALSE)

# Read in file (if necessary)
final_demand_uk_sector <- read.csv("15_input_data/alcohol_sales_tax_2000_2021.csv")


#### Plots ####

# Plot of alcohol sales by sector and alcohol type for UK
alcohol_type_sales_timeseries_plot <- ggplot(data = final_demand_alcohol_type_sector, aes(x = year, y = sales)) +
  geom_point(aes(colour = alcohol_type, shape = sector)) +
  geom_line(aes(colour = alcohol_type, linetype = sector)) +
  theme_minimal() +
  labs(x = "Year", y = "Sales (£m)",
       shape = "Sector", linetype = "Sector",
       colour = "Alcohol Type",
       title = "Alcohol Sales (£m) by Alcohol Type and Sector, United Kingdom, 2000-2021")

# Plot of total alcohol sales by sector
alcohol_sales_timeseries_plot <- ggplot(data = final_demand_uk_sector, aes(x = year, y = sales, group = sector)) +
  geom_line(aes(colour = sector)) +
  geom_point(aes(colour = sector)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2022, 4),
                     labels = seq(2000, 2022, 4),
                     limits = c(2000, 2021)) +
  labs(x = "Year", y = "Sales (£m)", colour = "Sector",
       title = "Alcohol Sales (£m) by Sector, United Kingdom, 2000-2021")

# Plot of total tax from alcohol sales by sector
alcohol_tax_timeseries_plot <- ggplot(final_demand_uk_sector, aes(x = year, y = tax, group = sector)) +
  geom_line(aes(colour = sector)) +
  geom_point(aes(colour = sector)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2022, 4),
                     labels = seq(2000, 2022, 4),
                     limits = c(2000, 2021)) +
  labs(x = "Year", y = "Tax (£m)", colour = "Sector",
       title = "Alcohol Tax Receipts (£m) by Sector, United Kingdom, 2000-2021")

# Plot of final demand at basic prices from alcohol sales by sector
alcohol_demand_timeseries_plot <- ggplot(final_demand_uk_sector, aes(x = year, y = demand, group = sector)) +
  geom_line(aes(colour = sector)) +
  geom_point(aes(colour = sector)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2022, 4),
                     labels = seq(2000, 2022, 4),
                     limits = c(2000, 2021)) +
  labs(x = "Year", y = "Demand (£m)", colour = "Sector",
       title = "Alcohol Demand (£m) by Sector, United Kingdom, 2000-2021")

# Export plot of timeseries of final demand for alcohol by sector
ggsave("15_input_data/alcohol_demand_timeseries_plot.jpeg", alcohol_demand_timeseries_plot, height = 4, width = 8)
