
### read in outputs from the HEALTHEI model, clean, and match to the
### input-output model food categories

library(data.table)

path <- "X:/HAR_PR/PR/InputOutput/Projects/HEALTHEI paper/input-output-healthei-paper/"

### Define population size to scale up per-person values

pop_size <- 1

###################################################
## read in baseline prices (mean price per gram)

prices <- read.csv(paste0(path,"data/inputs/input_output_price_baseline.csv"), header = FALSE)

setDT(prices)

prices[,"V1" := NULL]

prices <- as.matrix(prices)

prices <- t(prices)

price_data <- data.table(product = prices[,1],
                         price = prices[,2])

price_data[, price := as.numeric(price)]

##################################################
## read in baseline grams per person

consumption <- read.csv(paste0(path,"data/inputs/input_output_baseline_grams.csv"), header = FALSE)

setDT(consumption)

consumption[,"V1" := NULL]

consumption <- as.matrix(consumption)

consumption <- t(consumption)

consumption_data <- data.table(product = consumption[,1],
                               baseline_grams = consumption[,3])

consumption_data[, baseline_grams := as.numeric(baseline_grams)]


##################################################
## read in policy effects on grams per person

policy <- read.csv(paste0(path,"data/inputs/input_output_change.csv"), header = FALSE)

setDT(policy)

policy[,"V1" := NULL]

policy <- as.matrix(policy)

policy <- t(policy)

policy_data <- data.table(product = policy[,1],
                          change_SDIL_ext = policy[,3],
                          change_HFSS = policy[,4],
                          change_SUG_SALT = policy[,5])

policy_data[, change_SDIL_ext := as.numeric(change_SDIL_ext)]
policy_data[, change_HFSS := as.numeric(change_HFSS)]
policy_data[, change_SUG_SALT := as.numeric(change_SUG_SALT)]


#### merge datasets and scale all consumption by population

healthei_outputs <- merge(price_data, consumption_data, by = "product", sort = FALSE)
healthei_outputs <- merge(healthei_outputs, policy_data, by = "product", sort = FALSE)

healthei_outputs[, baseline_grams  := baseline_grams  * pop_size]
healthei_outputs[, change_SDIL_ext := change_SDIL_ext * pop_size]
healthei_outputs[, change_HFSS     := change_HFSS     * pop_size]
healthei_outputs[, change_SUG_SALT := change_SUG_SALT * pop_size]

setnames(healthei_outputs, "product","healthei_product")

#################################################
### Allocate to input-output model categories ###

#### create a lookup table of HEALTHEI categories to IO table

cats <- unique(cdohio.mod::food_expenditure$food_category)

lkup <- data.table()
lkup$healthei_product <- healthei_outputs$healthei_product
lkup$food_category <- ""


lkup[c(19:21,41), food_category := cats[1] ] ## Milk and milk products excluding cheese
lkup[22,    food_category := cats[2] ] ## Cheese
lkup[6:10,  food_category := cats[3] ] ## Carcase meat
lkup[11:15, food_category := cats[4] ] ## Non-carcase meat and meat products
lkup[16:18, food_category := cats[5] ] ## Fish
lkup[23,    food_category := cats[6] ] ## Eggs
lkup[24:28, food_category := cats[7] ] ## Fats
lkup[36:38, food_category := cats[8] ] ## Sugar and preserves
lkup[c(31:32,53), food_category := cats[9] ] ## Fresh and processed potatoes
lkup[c(29:30,33:35,45), food_category := cats[10] ] ## Fresh and processed fruit and vegetables, excluding potatoes
lkup[1:3,   food_category := cats[11] ] ## Bread
#lkup[, food_category := cats[12] ] ## Flour
lkup[40,   food_category := cats[13] ] ## Cakes, buns and pastries
lkup[52,   food_category := cats[14] ] ## Biscuits and crispbreads
lkup[4:5,   food_category := cats[15] ] ## Other cereals and cereal products
lkup[48:50,   food_category := cats[16] ] ## Beverages
lkup[c(39,42:44,51),   food_category := cats[17] ] ## Other food and drink
lkup[c(46:47),   food_category := cats[18] ] ## soft drinks
lkup[c(54),   food_category := cats[19] ] ## soft drinks
lkup[c(55),   food_category := "Out of Home" ] ## out of home

################

food <- cdohio.mod::food_expenditure[year == 2015, c("food_category","cpa")]

food_cat_lookup <- merge(lkup, food, by = "food_category", all = TRUE)

###############
##

cpa_cats <- cdohio.mod::cpa_categories[c(8:14,17,68),]


## preserved meat and meat products
food_cat_lookup[cpa == "meat", CPA := cpa_cats[1,"CPA"]]
food_cat_lookup[cpa == "meat", Product := cpa_cats[1,"Product"]]

## processed and preserved fish, crustaceans, molluscs, fruit and vegetables
food_cat_lookup[cpa == "fish_fruit_and_veg", CPA := cpa_cats[2,"CPA"]]
food_cat_lookup[cpa == "fish_fruit_and_veg", Product := cpa_cats[2,"Product"]]

## Vegetable and animal oils and fats
food_cat_lookup[cpa == "oils_and_fats", CPA := cpa_cats[3,"CPA"]]
food_cat_lookup[cpa == "oils_and_fats", Product := cpa_cats[3,"Product"]]

## Dairy products
food_cat_lookup[cpa == "dairy", CPA := cpa_cats[4,"CPA"]]
food_cat_lookup[cpa == "dairy", Product := cpa_cats[4,"Product"]]

## Grain mill products, starches and starch products
food_cat_lookup[cpa == "grains_and_starch", CPA := cpa_cats[5,"CPA"]]
food_cat_lookup[cpa == "grains_and_starch", Product := cpa_cats[5,"Product"]]

## Bakery and farinaceous products
food_cat_lookup[cpa == "bakery", CPA := cpa_cats[6,"CPA"]]
food_cat_lookup[cpa == "bakery", Product := cpa_cats[6,"Product"]]

## Other food products
food_cat_lookup[cpa == "other_food", CPA := cpa_cats[7,"CPA"]]
food_cat_lookup[cpa == "other_food", Product := cpa_cats[7,"Product"]]

## Other food products
food_cat_lookup[cpa == "soft_drinks", CPA := cpa_cats[8,"CPA"]]
food_cat_lookup[cpa == "soft_drinks", Product := cpa_cats[8,"Product"]]

food_cat_lookup[food_category == "Soft drinks", CPA := cpa_cats[8,"CPA"]]
food_cat_lookup[food_category == "Soft drinks", Product := cpa_cats[8,"Product"]]

## food and beverage serving services
food_cat_lookup[is.na(cpa), CPA := cpa_cats[9,"CPA"]]
food_cat_lookup[is.na(cpa), Product := cpa_cats[9,"Product"]]

#########################
### tidy ################

food_cat_lookup <- food_cat_lookup[, c("healthei_product","food_category","CPA","Product")]

usethis::use_data(food_cat_lookup, overwrite = TRUE)

