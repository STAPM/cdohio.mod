#### Tobacco Input Data Processing ####

source("data-raw/code/03_load_packages.R")

### Arguments

cig_price_opt <- 4 #(use WHO prices)
hrt_price_opt <- 1 #(ONS price quotes)

#source("data-raw/code/10_expenditure/14a_get_hrt_price_data.R")
#source("data-raw/code/10_expenditure/14b_cigarette_price_data_alternatives.R")

### Data needed:

## price for FM and HRT
## duty rates for FM and HRT
## duty receipts for FM and HRT

### Stages:

# 1) Clean and combine data on tobacco prices, duty rates, duty receipts, and illicit consumption
#      Two options for cigarette prices;
#      (i) ONS price of 20pk superkings,
#      (ii) EU WAP uprated using CPI INDEX 02.2.0.1 Cigarettes
# 2) Calculate expenditure by working out excise duty as % of retail price and scaling duty receipts
# 3) Estimate illicit expenditure on tobacco using HMRC Measuring Tax Gaps

##################################################
#### Tobacco Receipts - Tobacco Bulletin HMRC ####

## https://assets.publishing.service.gov.uk/media/66507ca38f4cb8fef9f64fba/Tobacco_Tab_Apr_24.ods
##
## accessed date: 08/07/2024. time horizon: 1990/91 - 2023/24. saved to data/raw/Tobacco_Tab_Apr_24.xlsx

# Tobacco receipts by tax year (Table 1a)
tobacco_receipts_tax_year <- read_xlsx(path = "data-raw/data/Tobacco_Duty_Receipts_Apr_24.xlsx",
                                       sheet = "Table_1_receipts",
                                       range = "A5:F39")

colnames(tobacco_receipts_tax_year) <- c("tax_year", "cigs_duty_receipt_mn", "cigars_duty_receipt_mn", "HRT_duty_receipt_mn", "other_duty_receipt_mn", "overall_duty_receipt_mn")

tobacco_receipts_tax_year$year <- str_sub(tobacco_receipts_tax_year$tax_year, 1, 4) # assume 2021-22 tax year = 2021

##############################################
#### Historical Tobacco Duty Rates - HMRC ####


tobacco_duty_rates <- read_xlsx(path = "data-raw/data/Tobacco_Duty_Rates.xlsx",
                                sheet = "Historical_Rates_Mar10_Mar23")
colnames(tobacco_duty_rates) <- c("date", "cig_adval", "min_ex_cig_per1000", "cig_per1000", "cigars_kg", "HRT_kg", "TfH_kg", "other_kg")

# Years - assumption that a tax year can be used for a calendar year
tobacco_duty_rates$year <- str_sub(tobacco_duty_rates$date, 1, 4)

# Values missing for the years 2019 and 2022 - assume no change from the previous year
tobacco_duty_rates_2019 <- tobacco_duty_rates[tobacco_duty_rates$year == "2018", ]
tobacco_duty_rates_2019$year <- "2019"
tobacco_duty_rates_2019$date <- NA
tobacco_duty_rates_2022 <- tobacco_duty_rates[tobacco_duty_rates$year == "2021", ]
tobacco_duty_rates_2022$year <- "2022"
tobacco_duty_rates_2022$date <- NA

tobacco_duty_rates <- rbind(tobacco_duty_rates, tobacco_duty_rates_2019, tobacco_duty_rates_2022)

setDT(tobacco_duty_rates)

tobacco_duty_rates[, N := .N, by = "year"]
tobacco_duty_rates[, k := 1, by = "year"]
tobacco_duty_rates[, cu := cumsum(k), by = "year"]

tobacco_duty_rates <- tobacco_duty_rates[cu == N,]
tobacco_duty_rates <- tobacco_duty_rates[order(year),]

tobacco_duty_rates[, c("N","k","cu","date") := NULL]
tobacco_duty_rates[, TfH_kg := as.numeric(TfH_kg)]
# Clean up
rm(tobacco_duty_rates_2019, tobacco_duty_rates_2022)

##################################
#### Price Data - ONS ############


#### Cigarette prices
####
#### Multiple data sources. Defaulting to using WHO price of most commonly purchased brand
####
#### (i) RPI Ave price 20 super king, (ii) Morris et al. (2024), (iii) EU Commission,
#### (iv) World Health Organisation, (v) STAPM

ave_price_cigarettes_20pack <- read.csv("data-raw/data/Tobacco_Price_Cigarettes_20pk.csv") %>% setDT
ave_price_cigarettes_20pack[, X := NULL]

ave_price_cigarettes_20pack[, year := as.character(year)]

#### Handrolled tobacco prices
####
#### Price quotes data - average price of 30g of handrolled tobacco
#### https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes
####
#### Also from STAPM

ave_price_hrt_30g_1 <- read.csv("data-raw/data/Tobacco_Price_HRT_30g.csv") %>% setDT
ave_price_hrt_30g_2 <- read.csv("data-raw/data/Tobacco_Price_HRT_STAPM_30g.csv") %>% setDT

ave_price_hrt_30g <- merge(ave_price_hrt_30g_1, ave_price_hrt_30g_2, by = "year", all = TRUE)

ave_price_hrt_30g[, price_hrt_100g_1 := (price_hrt_30g_1*100)/30]
ave_price_hrt_30g[, price_hrt_100g_2 := (price_hrt_30g_2*100)/30]

ave_price_hrt_30g[, year := as.character(year)]

ave_price_hrt_30g[, c("X.x","X.y","price_hrt_30g_1","price_hrt_30g_2") := NULL]

#######################################
#### ILLICIT TOBACCO ##################

## Source: HMRC measuring tax gaps data
## https://www.gov.uk/government/statistics/measuring-tax-gaps-tables

### Table 3.13: Cigarettes - illicit market volume (billion cigarettes)
### Table 3.17: Hand-rolling tobacco - illicit market volume (million kg)

temp <- tempfile()
url <- "https://assets.publishing.service.gov.uk/media/66702835a471d1f120195ac5/Measuring_tax_gap_online_tables_2024.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### Cigarettes

fm_data <- read_xlsx(temp,
                     sheet = "Table 3.11 - 3.18",
                     range = "A34:X37") %>%
  dplyr::filter(`Illicit market` == "Central estimate") %>%
  dplyr::select(-c(`Illicit market`)) %>%
  tidyr::pivot_longer(names_to = "tax_year", values_to = "illicit_cigs_billion", cols = everything()) %>%
  dplyr::mutate(year = as.numeric(str_sub(tax_year, 1, 4))) %>%
  dplyr::mutate(illicit_cigs_billion = as.numeric(illicit_cigs_billion)) %>%
  dplyr::mutate(illicit_cigs_pk20 = (illicit_cigs_billion*1000000000)/20)

### Handrolled tobacco

hrt_data <- read_xlsx(temp,
                      sheet = "Table 3.11 - 3.18",
                      range = "A89:X91") %>%
  dplyr::filter(`Illicit market` == "Central estimate") %>%
  dplyr::select(-c(`Illicit market`)) %>%
  tidyr::pivot_longer(names_to = "tax_year", values_to = "illicit_hrt_kg_mn", cols = everything()) %>%
  dplyr::mutate(year = as.numeric(str_sub(tax_year, 1, 4))) %>%
  dplyr::mutate(illicit_hrt_kg_mn = as.numeric(illicit_hrt_kg_mn)) %>%
  dplyr::mutate(illicit_hrt_100g = (illicit_hrt_kg_mn*1000000*10))

tax_gaps_tobacco <- merge(fm_data, hrt_data, by = "year")

setDT(tax_gaps_tobacco)

tax_gaps_tobacco[, year := as.character(year)]
tax_gaps_tobacco[, c("tax_year.x","tax_year.y") := NULL]

write.csv(tax_gaps_tobacco,"data-raw/data/Tobacco_Illicit_Market.csv")

setDT(tax_gaps_tobacco)


##############################################################################
#### COMBINE TAX DUTY RECEIPTS, RATES, PRICE, AND ILLICIT CONSUMPTION DATA

tobacco_expenditure <- merge(tobacco_duty_rates, tobacco_receipts_tax_year, by = "year", all.y = TRUE)
tobacco_expenditure <- merge(tobacco_expenditure, ave_price_cigarettes_20pack, by = "year", all.x = TRUE)
tobacco_expenditure <- merge(tobacco_expenditure, ave_price_hrt_30g, by = "year", all.x = TRUE)
tobacco_expenditure <- merge(tobacco_expenditure, tax_gaps_tobacco, by = "year", all.x = TRUE)


tobacco_expenditure[, c("cigars_kg","TfH_kg","other_kg","tax_year",
                        "cigars_duty_receipt_mn","other_duty_receipt_mn",
                        "overall_duty_receipt_mn") := NULL]
tobacco_expenditure[, year := as.numeric(year)]

rm(ave_price_cigarettes_20pack, ave_price_hrt_30g, tobacco_duty_rates,
   tobacco_receipts_tax_year, tax_gaps_tobacco, fm_data, hrt_data)

### select which cigarette price to use

if (cig_price_opt == 1){
  setnames(tobacco_expenditure, "price_cigs_20pk_1", "price_cigs_20pk")
  tobacco_expenditure[, c("price_cigs_20pk_2","price_cigs_20pk_3",
                          "price_cigs_20pk_4","price_cigs_20pk_5") := NULL]
}
if (cig_price_opt == 2){
  setnames(tobacco_expenditure, "price_cigs_20pk_2", "price_cigs_20pk")
  tobacco_expenditure[, c("price_cigs_20pk_1","price_cigs_20pk_3",
                          "price_cigs_20pk_4","price_cigs_20pk_5") := NULL]
}
if (cig_price_opt == 3){
  setnames(tobacco_expenditure, "price_cigs_20pk_3", "price_cigs_20pk")
  tobacco_expenditure[, c("price_cigs_20pk_1","price_cigs_20pk_2",
                          "price_cigs_20pk_4","price_cigs_20pk_5") := NULL]
}
if (cig_price_opt == 4){
  setnames(tobacco_expenditure, "price_cigs_20pk_4", "price_cigs_20pk")
  tobacco_expenditure[, c("price_cigs_20pk_1","price_cigs_20pk_2",
                          "price_cigs_20pk_3","price_cigs_20pk_5") := NULL]
}
if (cig_price_opt == 5){
  setnames(tobacco_expenditure, "price_cigs_20pk_5", "price_cigs_20pk")
  tobacco_expenditure[, c("price_cigs_20pk_1","price_cigs_20pk_2",
                          "price_cigs_20pk_3","price_cigs_20pk_4") := NULL]
}

### select which hrt price to use

if (hrt_price_opt == 1){
  setnames(tobacco_expenditure, "price_hrt_100g_1", "price_hrt_100g")
  tobacco_expenditure[, c("price_hrt_100g_2") := NULL]
}
if (hrt_price_opt == 2){
  setnames(tobacco_expenditure, "price_hrt_100g_2", "price_hrt_100g")
  tobacco_expenditure[, c("price_hrt_100g_1") := NULL]
}

#############################################################
##### CALCULATE SPENDING ON LICIT and ILLICIT TOBACCO #######

## get proportion of mean retail price which is excise duty, use this to scale
## up duty receipts into total expenditure

#### excise duty % of price ####

tobacco_expenditure[, fm_excise_pct  := ((cig_adval/100)*price_cigs_20pk + cig_per1000*(20/1000)) / price_cigs_20pk ]
tobacco_expenditure[, hrt_excise_pct := (HRT_kg * (100/1000)) / price_hrt_100g ]

############################
#### PURCHASER PRICES ######

#### total licit expenditure

tobacco_expenditure[, spend_licit_fm_mn  := cigs_duty_receipt_mn / fm_excise_pct  ]
tobacco_expenditure[, spend_licit_hrt_mn := HRT_duty_receipt_mn / hrt_excise_pct ]

#### total illicit expenditure (assume illicit price is half of the licit price)

tobacco_expenditure[, spend_illicit_fm_mn    := (illicit_cigs_pk20 * price_cigs_20pk * 0.5)/1000000]
tobacco_expenditure[, spend_illicit_hrt_mn   := (illicit_hrt_100g * price_hrt_100g * 0.5)/1000000 ]

#################################################
#### BASIC PRICES (minus excise tax + VAT) ######

#### total licit expenditure

tobacco_expenditure[, spend_licit_fm_mn_bp  := spend_licit_fm_mn - (fm_excise_pct*spend_licit_fm_mn) - spend_licit_fm_mn*(0.2/1.2)  ]
tobacco_expenditure[, spend_licit_hrt_mn_bp := spend_licit_hrt_mn - (hrt_excise_pct*spend_licit_hrt_mn) - spend_licit_hrt_mn*(0.2/1.2) ]

#### total illicit expenditure

tobacco_expenditure[, spend_illicit_fm_mn_bp  := spend_illicit_fm_mn]
tobacco_expenditure[, spend_illicit_hrt_mn_bp := spend_illicit_hrt_mn]



#################################
### Tidy expenditure data


tobacco_expenditure <- tobacco_expenditure[, c("year",
                                               "spend_licit_fm_mn","spend_licit_hrt_mn",
                                               "spend_illicit_fm_mn","spend_illicit_hrt_mn",
                                               "fm_excise_pct","hrt_excise_pct")]

tobacco_expenditure <- tobacco_expenditure[year %in% 2019:2022]

tobacco_expenditure <- melt(tobacco_expenditure,
                            id.vars = "year",
                            measure.vars = list(c("spend_licit_fm_mn","spend_licit_hrt_mn"),
                                                c("spend_illicit_fm_mn","spend_illicit_hrt_mn"),
                                                c("fm_excise_pct","hrt_excise_pct")),
                            variable.name = "tobacco_category",
                            value.name = c("spend_licit_mn","spend_illicit_mn","excise_pct"))

tobacco_expenditure[, tobacco_category := factor(tobacco_category, levels = 1:2, labels = c("cigarettes","hrt"))]

## tax, distributor margins, and imports as % of spending

tobacco_expenditure[, tax_pct := excise_pct + 0.2/(1 + 0.2)]

## write out the data

usethis::use_data(tobacco_expenditure, overwrite = TRUE)

##############################################################################
### From the supply tables: imports / distributor margins as % of supply

## Imports

print(paste0("Import as % of Supply, 2021: ", round(9897/69415,4)*100 ))
print(paste0("Import as % of Supply, 2020: ", round(9519/64690,4)*100 ))
print(paste0("Import as % of Supply, 2019: ", round(14026/69358,4)*100 ))
print(paste0("Import as % of Supply, 2018: ", round(13437/70292,4)*100 ))
print(paste0("Import as % of Supply, 2017: ", round(12935/65796,4)*100 ))


## Distributors trading margins

print(paste0("Distributors Trading Margins as % of Supply, 2021: ", round(17455/69415,4)*100 ))
print(paste0("Distributors Trading Margins as % of Supply, 2020: ", round(15957/64690,4)*100 ))
print(paste0("Distributors Trading Margins as % of Supply, 2019: ", round(17019/69358,4)*100 ))
print(paste0("Distributors Trading Margins as % of Supply, 2018: ", round(18445/70292,4)*100 ))
print(paste0("Distributors Trading Margins as % of Supply, 2017: ", round(16532/65796,4)*100 ))
