#### Explore different sources of tobacco price data ####

source("src/03_load_packages.R")

#### Cigarette prices (i)
####
#### RPI: Ave price - Cigarettes 20 king size filter ####
#### https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czmp

# Annual data on average price for 20 pack of king size filter cigarettes from ONS
ave_price_cigarettes_20pack_1 <- read_xls(path = "data/raw/Tobacco_Price_Cigs_20_king_size.xls",
                                          range = "A9:B45",
                                          col_names = F) %>% setDT

setnames(ave_price_cigarettes_20pack_1, names(ave_price_cigarettes_20pack_1), c("year", "price_cigs_20pk_1"))
ave_price_cigarettes_20pack_1[, year := as.character(year)]
ave_price_cigarettes_20pack_1[, price_cigs_20pk_1 := price_cigs_20pk_1/100]

#### Cigarette prices (ii)
####
#### CPI index for cigarettes (CPI index 02.2.0.1)
#### https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l7aq/mm23
####
#### Use this to adjust the WAP for Jan 2019 (404.15*(20/1000) = 8.083) 
#### used in Morris et al. 2024 (smoke-free dividend paper) OR 8.083

ave_price_cigarettes_20pack_2 <- read_xls(path = "data/raw/Tobacco_Price_Cigs_CPI_Index.xls",
                                          range = "A9:B169",
                                          col_names = F) %>% setDT

setnames(ave_price_cigarettes_20pack_2, names(ave_price_cigarettes_20pack_2), c("time", "cpi_index"))

index <- as.numeric(ave_price_cigarettes_20pack_2[time == "2019 JAN", "cpi_index"])

ave_price_cigarettes_20pack_2 <- ave_price_cigarettes_20pack_2[substr(time,8,8) != ""]
ave_price_cigarettes_20pack_2[, year := substr(time,1,4)]
ave_price_cigarettes_20pack_2[, cpi_index := cpi_index/index]

ave_price_cigarettes_20pack_2[, price_cigs_20pk_2 := cpi_index * 8.083]

ave_price_cigarettes_20pack_2 <- ave_price_cigarettes_20pack_2[, .(price_cigs_20pk_2 = mean(price_cigs_20pk_2)), by = "year"]

#### (iii) right figure for WAP might be 8.83, use that 
####
#### Use this to adjust the WAP for Jan 2019 (404.15*(20/1000)) 
#### used in Morris et al. 2024 (smoke-free dividend paper) 

ave_price_cigarettes_20pack_3 <- read_xls(path = "data/raw/Tobacco_Price_Cigs_CPI_Index.xls",
                                          range = "A9:B169",
                                          col_names = F) %>% setDT

setnames(ave_price_cigarettes_20pack_3, names(ave_price_cigarettes_20pack_3), c("time", "cpi_index"))

index <- as.numeric(ave_price_cigarettes_20pack_3[time == "2019 JAN", "cpi_index"])

ave_price_cigarettes_20pack_3 <- ave_price_cigarettes_20pack_3[substr(time,8,8) != ""]
ave_price_cigarettes_20pack_3[, year := substr(time,1,4)]
ave_price_cigarettes_20pack_3[, cpi_index := cpi_index/index]

ave_price_cigarettes_20pack_3[, price_cigs_20pk_3 := cpi_index * 8.83]

ave_price_cigarettes_20pack_3 <- ave_price_cigarettes_20pack_3[, .(price_cigs_20pk_3 = mean(price_cigs_20pk_3)), by = "year"]


#### (iv) World Health Organisation
####
#### reported biennially - interpolate the prices for off years
#### https://apps.who.int/gho/data/node.main-eu.TOBRAISETAXES?lang=en

ave_price_cigarettes_20pack_4 <- read_xlsx(path = "data/raw/Tobacco_Price_Cigs_WHO.xlsx",
                                          range = "A1:AE29",
                                          col_names = TRUE) %>% setDT

ave_price_cigarettes_20pack_4 <- ave_price_cigarettes_20pack_4[,c("...1",
                                                                  "Most sold brand of cigarettes - price in currency reported...11",            
                                                                  "Most sold brand of cigarettes - price in currency reported...12",             
                                                                  "Most sold brand of cigarettes - price in currency reported...13",             
                                                                  "Most sold brand of cigarettes - price in currency reported...14",             
                                                                  "Most sold brand of cigarettes - price in currency reported...15",             
                                                                  "Most sold brand of cigarettes - price in currency reported...16",             
                                                                  "Most sold brand of cigarettes - price in currency reported...17")]

ave_price_cigarettes_20pack_4 <- ave_price_cigarettes_20pack_4[`...1` == "United Kingdom of Great Britain and Northern Ireland"]

setnames(ave_price_cigarettes_20pack_4, "...1", "country")

ave_price_cigarettes_20pack_4 <- melt(ave_price_cigarettes_20pack_4,
                                      id.vars = "country")

ave_price_cigarettes_20pack_4[, year := seq(2020,2008,-2)]
setnames(ave_price_cigarettes_20pack_4, "value", "price_cigs_20pk_4")

ave_price_cigarettes_20pack_4 <- ave_price_cigarettes_20pack_4[, c("year","price_cigs_20pk_4")]

fill <- data.table(year = 2008:2024)

ave_price_cigarettes_20pack_4 <- merge(ave_price_cigarettes_20pack_4, fill, by = "year", all.y = TRUE) 

ave_price_cigarettes_20pack_4[, price_cigs_20pk_4 := zoo::na.approx(price_cigs_20pk_4, na.rm = FALSE)] 


cig_price_cpi <- read_xls(path = "data/raw/Tobacco_Price_Cigs_CPI_Index.xls",
                          range = "A9:B17",
                          col_names = F) %>% setDT
setnames(cig_price_cpi, names(cig_price_cpi), c("year","cpi"))

### extrapolate beyond 2020

index_2018 <- 1 + ((as.numeric(cig_price_cpi[year == 2018, "cpi"]) - as.numeric(cig_price_cpi[year == 2017, "cpi"]))/as.numeric(cig_price_cpi[year == 2018, "cpi"]))
index_2019 <- 1 + ((as.numeric(cig_price_cpi[year == 2019, "cpi"]) - as.numeric(cig_price_cpi[year == 2018, "cpi"]))/as.numeric(cig_price_cpi[year == 2019, "cpi"]))
index_2020 <- 1 + ((as.numeric(cig_price_cpi[year == 2020, "cpi"]) - as.numeric(cig_price_cpi[year == 2019, "cpi"]))/as.numeric(cig_price_cpi[year == 2020, "cpi"]))

index_2021 <- 1 + ((as.numeric(cig_price_cpi[year == 2021, "cpi"]) - as.numeric(cig_price_cpi[year == 2020, "cpi"]))/as.numeric(cig_price_cpi[year == 2020, "cpi"]))
index_2022 <- 1 + ((as.numeric(cig_price_cpi[year == 2022, "cpi"]) - as.numeric(cig_price_cpi[year == 2021, "cpi"]))/as.numeric(cig_price_cpi[year == 2021, "cpi"]))
index_2023 <- 1 + ((as.numeric(cig_price_cpi[year == 2023, "cpi"]) - as.numeric(cig_price_cpi[year == 2022, "cpi"]))/as.numeric(cig_price_cpi[year == 2022, "cpi"]))

price_2021 <- ave_price_cigarettes_20pack_4[year == 2020,"price_cigs_20pk_4"]*index_2021
ave_price_cigarettes_20pack_4[year == 2021, price_cigs_20pk_4 := price_2021 ]

price_2022 <- ave_price_cigarettes_20pack_4[year == 2021,"price_cigs_20pk_4"]*index_2022
ave_price_cigarettes_20pack_4[year == 2022, price_cigs_20pk_4 := price_2022 ]

price_2023 <- ave_price_cigarettes_20pack_4[year == 2022,"price_cigs_20pk_4"]*index_2023
ave_price_cigarettes_20pack_4[year == 2023, price_cigs_20pk_4 := price_2023 ]

#### (v) STAPM TAX-sim 2.5.0
####
#### STAPM modelling control arm B price for 2017, then uprated using the cigarette CPI
#### The mean price of FM cigs in the STAPM model is 0.444582568529469 per stick = 8.89 per pack 20

ave_price_cigarettes_20pack_5 <- data.table(year = 2017:2023,
                                            price_cigs_20pk_5 = c(8.89,
                                                                  8.89*index_2018,
                                                                  8.89*index_2018*index_2019,
                                                                  8.89*index_2018*index_2019*index_2020,
                                                                  8.89*index_2018*index_2019*index_2020*index_2021,
                                                                  8.89*index_2018*index_2019*index_2020*index_2021*index_2022,
                                                                  8.89*index_2018*index_2019*index_2020*index_2021*index_2022*index_2023))


############################################################
######### COMBINE PRICE DATA AND PLOT ######################

cig_price_data <- merge(ave_price_cigarettes_20pack_1, ave_price_cigarettes_20pack_2, by = "year", all.y = TRUE)
cig_price_data <- merge(cig_price_data, ave_price_cigarettes_20pack_3, by = "year", all = TRUE)

cig_price_data[, year := as.numeric(year)]

cig_price_data <- merge(cig_price_data, ave_price_cigarettes_20pack_4, by = "year", all = TRUE)
cig_price_data <- merge(cig_price_data, ave_price_cigarettes_20pack_5, by = "year", all = TRUE)

write.csv(cig_price_data,"data/raw/Tobacco_Price_Cigarettes_20pk.csv")
