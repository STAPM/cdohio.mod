#### Food Input Data Processing ####

source("data-raw/code/03_load_packages.R")

######################################
#### process data from the Family Food Survey / EFS
####
#### https://www.gov.uk/government/statistical-data-sets/family-food-datasets

### food categories to keep:

food_codes <- c("cat520","22023","31041","46095","100123",
                "12901","135148","150154","cat701","cat811",
                "251263","26401","267270","271277","281301",
                "304313","314339","340344","350355")

food_expenditure <- read_xlsx(path = "data-raw/data/Food_Expenditure_FF.xlsx",
                              sheet = "expenditure",
                              range = "A26:BC371") %>%
  filter(Code %in% food_codes) %>% setDT

food_expenditure[Code %in% c("31041","46095")           , group := "meat"]
food_expenditure[Code %in% c("100123","cat701","cat811"), group := "fish_fruit_and_veg"]
food_expenditure[Code %in% c("135148")                  , group := "oils_and_fats"]
food_expenditure[Code %in% c("22023","cat520","12901")  , group := "dairy"]
food_expenditure[Code %in% c("281301")                  , group := "grains_and_starch"]
food_expenditure[Code %in% c("26401","267270","271277",
                             "251263")                  , group := "bakery"]
food_expenditure[Code %in% c("304313","314339",
                             "340344","350355","150154"), group := "other_food"]



food_expenditure <- food_expenditure[, c("Code","Food Category","Food Group","group",
                                         "...48","...49","...50","...51","...52",
                                         "...53","...54","...55")]

setnames(food_expenditure,
         names(food_expenditure),
         c("code","food_category","food_group","cpa","2015","2016","2017","2018","2019","2020","2021","y2022"))


food_expenditure[is.na(food_category), food_category := food_group]

food_expenditure[, c("code","food_group") := NULL]

food_expenditure[, y2022 := as.numeric(y2022)]

setnames(food_expenditure, "y2022", "2022")

food_expenditure <- melt(food_expenditure,
                         id.vars = c("food_category","cpa"),
                         variable.name = "year",
                         value.name = "exp_pence_per_person_per_wk")

### annualise and express in pounds

food_expenditure[, exp_per_person_per_year := (exp_pence_per_person_per_wk/100)*52]

### scale up to population by using ONS mid-year population estimates
###
### https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/ukpop/pop


population <- read_xlsx(path = "data-raw/data/Population_MidYear.xlsx",
                        range = "A53:B60",
                        col_names = FALSE) %>% setDT

setnames(population, names(population), c("year","population"))


food_expenditure <- merge(food_expenditure, population, by = "year")

food_expenditure[, exp_per_year := (exp_per_person_per_year * population)]

food_expenditure[, exp_per_year_mn := exp_per_year/1000000]

food_expenditure[, year := as.numeric(year)]

food_expenditure <- food_expenditure[, c("year","food_category","cpa","exp_per_year_mn")]

## write out the data

usethis::use_data(food_expenditure, overwrite = TRUE)

