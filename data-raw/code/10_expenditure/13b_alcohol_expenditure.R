#### Alcohol Input Data Processing (BBPA) ####

source("data-raw/code/03_load_packages.R")

path <- "G:/Shared drives/SARG Team/BBPA Handbook/"

file <- "2023/Digital Handbook 2023 FINAL.xlsx"

################################################################################
#######----------------------------------------------------------------#########
###### ------------------------------ SALES --------------------------- ########
#######----------------------------------------------------------------#########
################################################################################

#### beer (thousands of hectolitres)

total_sales <- read_xlsx(path = paste0(path,file),
                         sheet = "A1",
                         range = "A4:B56") %>% setDT
setnames(total_sales, names(total_sales), c("year","amount"))

on_off_split <- read_xlsx(path = paste0(path,file),
                          sheet = "A6",
                          range = "A5:C57") %>% setDT
setnames(on_off_split, names(on_off_split), c("year","on","off"))

total_beer_sales <- merge(total_sales, on_off_split, by = c("year"))

total_beer_sales[, sales_on_hctlitres := (amount*1000) * (on/100) ]
total_beer_sales[, sales_off_hctlitres := (amount*1000) * (off/100) ]

total_beer_sales[, c("amount","on","off") := NULL]

#### wine (thousands of hectolitres)

total_sales <- read_xlsx(path = paste0(path,file),
                         sheet = "B1",
                         range = "A6:G31") %>% setDT
setnames(total_sales, c("...1","...7"), c("year","amount"))
total_sales <- total_sales[, c("year","amount")]

#### spirits (thousands of hectolitres of 100% alcohol)

total_sales <- read_xlsx(path = paste0(path,file),
                         sheet = "B2",
                         range = "A5:E30") %>% setDT
setnames(total_sales, c("Year","Total"), c("year","amount"))
total_sales <- total_sales[, c("year","amount")]

#### cider (thousands of hectolitres)

total_sales <- read_xlsx(path = paste0(path,file),
                         sheet = "B3",
                         range = "A6:F32") %>%
  filter(`...1` != 1970) %>%
  mutate(`...6` = as.numeric(`...6`)) %>% setDT
setnames(total_sales, c("...1","...6"), c("year","amount"))
total_sales <- total_sales[, c("year","amount")]


################################################################################
#######----------------------------------------------------------------#########
###### ------------------------------ PRICE --------------------------- ########
#######----------------------------------------------------------------#########
################################################################################

file <- "2023/Digital Handbook 2023 FINAL.xlsx"

##############################################
#### Off-trade prices (Source: Nielsen) ######

### beer (pint = 568.261ml)
### spirits (700ml)
### cider (pint = 568.261ml)
### wine (750ml)

off_trade_prices <- read_xlsx(path = paste0(path,file),
                              sheet = "C11",
                              range = "B22:E22",
                              col_names = FALSE)

#########################################
#### on-trade prices (source: CGA) ######

### beer (pint = 568.261ml)
### spirits (25ml)
### cider (pint = 568.261ml)
### wine (175ml)

on_trade_prices <- read_xlsx(path = paste0(path,file),
                             sheet = "C10",
                             range = "B36:E36",
                             col_names = FALSE)
