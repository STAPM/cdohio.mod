##########################################
#### Read in and process the LFS data

source("data-raw/code/03_load_packages.R")


root <- ""
file <- here::here("data-raw/data/")

keep_vars <- c("year","quarter","pwt","piwt","d_country","l_lmstatus_8cat",
               "eh_weekly_earnings_nom","l_full_time","l_sic2007code")

lfs_data <- lfsclean::lfsclean(root = root,
                               file = file,
                               year = 2017:2022,
                               ages = 16:89,
                               keep_vars = keep_vars,
                               complete_vars = NULL,
                               deflator = "cpih")

##########################################################################
## restrict to all employed/self-employed with complete information on
## full time status and industry

for (y in 2017:2022){

data <- lfs_data[l_lmstatus_8cat=="employed" | l_lmstatus_8cat=="self_employed" ,]
data <- data[!is.na(l_full_time) ,]
data <- data[!is.na(l_sic2007code) ,]
data <- data[year == y, ]

data[l_full_time == "full_time",fte_ := 1]
data[l_full_time == "part_time",fte_ := 0.5]

data_uk <- data[, .(fte   = sum(pwt*fte_),
                    total = sum(pwt ),
                    earn = weighted.mean(eh_weekly_earnings_nom,w = piwt,na.rm = TRUE)),
                by = c("year","quarter","l_sic2007code")]

data_uk <- data_uk[, .(fte = mean(fte), total = mean(total)), by = c("year","l_sic2007code")]

setnames(data_uk, c("l_sic2007code"), c("SIC_code"))

data_uk[, SIC_code := as.numeric(as.character(SIC_code))]
data_uk[, year := y]

###################################
## read in the SIC mapping sheet

map <- readxl::read_excel("data-raw/data/SIC_to_IOTABLE_mapping.xlsx",
                          range = "A2:L614",
                          col_names = TRUE) %>% setDT

map[, SIC_code := as.numeric(SIC_code)]

#################################################
## collapse employment to IO table industries ###

  empl <- data_uk[year == y,]

  ##############################################
  ## Map employment onto the IO table sectors

  merge <- merge.data.table(map, empl, by = "SIC_code", all.x = TRUE)

  merge[is.na(total), total := 0]
  merge[is.na(fte), fte := 0]

lfs_empl <- merge[, .(tot_emp = sum(total, na.rm = TRUE),
                      tot_fte = sum(fte,   na.rm = TRUE) ), by = c("SIC","SIC_Industry")]

setnames(lfs_empl, "tot_emp", paste0("empl_",y))
setnames(lfs_empl, "tot_fte", paste0("fte_",y))


if (y == 2017){
  lfs_empl_data <- copy(lfs_empl)
} else {
  lfs_empl_data <- merge(lfs_empl_data, lfs_empl)
}


}

usethis::use_data(lfs_empl_data, overwrite = TRUE)
