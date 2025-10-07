##########################################
#### Read in and process the LFS data

source("data-raw/code/03_load_packages.R")


root <- ""
file <- here::here("data-raw/data/")

keep_vars <- c("year","quarter","pwt","piwt","d_country","l_lmstatus_8cat",
               "eh_weekly_earnings_nom","l_full_time","l_sic2007code")

data <- lfsclean::lfsclean(root = root,
                           file = file,
                           year = 2016:2023,
                           ages = 16:89,
                           keep_vars = keep_vars,
                           complete_vars = NULL,
                           deflator = "cpih")

##########################################################################
## restrict to all employed/self-employed with complete information on
## full time status and industry

data <- data[l_lmstatus_8cat=="employed" | l_lmstatus_8cat=="self_employed" ,]
data <- data[l_full_time == "full_time" ,]
data <- data[!is.na(l_sic2007code) ,]

###############################################################################
## construct mean wages by industry for each year as a rolling 3-year average

data_uk_2017 <- data[year %in% 2016:2018, .(total = sum(pwt/12 ),
                                            earn = weighted.mean(eh_weekly_earnings_nom,w = piwt,na.rm = TRUE)),
                     by = c("l_sic2007code")]

data_uk_2018 <- data[year %in% 2017:2019, .(total = sum(pwt/12 ),
                                            earn = weighted.mean(eh_weekly_earnings_nom,w = piwt,na.rm = TRUE)),
                     by = c("l_sic2007code")]

data_uk_2019 <- data[year %in% 2018:2020, .(total = sum(pwt/12 ),
                                            earn = weighted.mean(eh_weekly_earnings_nom,w = piwt,na.rm = TRUE)),
                     by = c("l_sic2007code")]

data_uk_2020 <- data[year %in% 2019:2021, .(total = sum(pwt/12 ),
                                            earn = weighted.mean(eh_weekly_earnings_nom,w = piwt,na.rm = TRUE)),
                     by = c("l_sic2007code")]

data_uk_2021 <- data[year %in% 2020:2022, .(total = sum(pwt/12 ),
                                            earn = weighted.mean(eh_weekly_earnings_nom,w = piwt,na.rm = TRUE)),
                     by = c("l_sic2007code")]

data_uk_2022 <- data[year %in% 2021:2023, .(total = sum(pwt/12 ),
                                            earn = weighted.mean(eh_weekly_earnings_nom,w = piwt,na.rm = TRUE)),
                     by = c("l_sic2007code")]

data_uk_2017[, year := 2017]
data_uk_2018[, year := 2018]
data_uk_2019[, year := 2019]
data_uk_2020[, year := 2020]
data_uk_2021[, year := 2021]
data_uk_2022[, year := 2022]

data_uk <- rbindlist(list(data_uk_2017, data_uk_2018, data_uk_2019,
                          data_uk_2020, data_uk_2021, data_uk_2022))

setnames(data_uk, c("l_sic2007code"), c("SIC_code"))

data_uk[, SIC_code := as.numeric(as.character(SIC_code))]

###################################
## read in the SIC mapping sheet

map <- readxl::read_excel("data-raw/data/SIC_to_IOTABLE_mapping.xlsx",
                          range = "A2:L614",
                          col_names = TRUE) %>% setDT

map[, SIC_code := as.numeric(SIC_code)]

for (y in 2017:2022){
#################################################
## collapse employment to IO table industries ###

earn <- data_uk[year == y,]

#################################################################
## Map employment and earnings data onto the IO table sectors

merge <- merge.data.table(map, earn, by = "SIC_code", all.x = TRUE)

merge[is.na(total), total := 0]

lfs_earn <- merge[, .(earn = weighted.mean(earn, w = total, na.rm = TRUE)), by = c("SIC","SIC_Industry")]

### annualise
lfs_earn[, earn := earn*52]

setnames(lfs_earn, "earn", paste0("earn_",y))

#########################
### Combine data

if (y == 2017){
lfs_earn_out <- copy(lfs_earn)
} else {
lfs_earn_out <- merge(lfs_earn_out, lfs_earn, by = c("SIC","SIC_Industry"))
}


}

lfs_wage_data <- copy(lfs_earn_out)
setDT(lfs_wage_data)

usethis::use_data(lfs_wage_data, overwrite = TRUE)

