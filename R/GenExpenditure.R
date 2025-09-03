#' Calculate Demand Changes
#'
#' Generate changes in demand for alcohol, tobacco, gambling, and food using the
#' saved expenditure data for the four product types in the model.
#'
#' Note that for alcohol and tobacco this function also calculates changes in demand in basic prices as well as
#' in purchaser prices. As alcohol and tobacco are combined in the model, and have very different levels of taxation
#' relative to retail price, the conversion from purchaser prices to basic prices is done here separately for
#' alcohol and tobacco rather than within the model, as is the case for other products. Because of this, this function also
#' calculates the change in tax less subsidies on products for alcohol and tobacco so that this is
#' accounted for in the model.
#'
#' @param year Numeric integer. Year of expenditure data to use - default is 2019.
#' @param change_food Numeric integer. Percentage change in expenditure on 19 categories of food;
#' 1. Milk, 2. Cheese, 3. Carcase meat, 4. Non-carcase meat and meat products, 5. Fish, 6. Eggs,
#' 7. Fats, 8. Sugar and preserves, 9. Potatoes, 10. Fruit and veg, 11. Bread, 12. Flour, 13. Cakes, buns, and pastries,
#' 14. Biscuits and crispbreads, 15. Cereals and cereal products, 16. Beverages, 17. Other food and drink, 18. Soft drinks,
#' 19. Confectionary.
#' @param change_gambling Numeric integer. Percentage change in expenditure (gross gambling yield) on 9
#' gambling categories (arcades, betting (non-remote), bingo (non-remote), casino (non-remote),
#' betting (remote), bingo (remote), casino (remote), lotteries, the National Lottery)
#' @param change_tobacco_licit Numeric vector. Percentage change in expenditure on 2
#' licit tobacco product categories (cigarettes, handrolled tobacco)
#' @param change_tobacco_illicit Numeric vector. Percentage change in expenditure on 2
#' illicit tobacco product categories (cigarettes, handrolled tobacco)
#' @param change_alcohol_on Numeric vector. Percentage change in expenditure on 4 alcohol
#' product categories in the on-trade (beer, cider, spirits, wine)
#' @param change_alcohol_off Numeric vector. Percentage change in expenditure on 4 alcohol
#' product categories in the off-trade (beer, cider, spirits, wine)
#' @param reallocate_food Numeric. Reallocating changed expenditure on food to other food categories.
#' Select one of the seven food categories in the input-output model to reallocate the changed spending
#' on food to; (1) meat and meat products (2) fish, fruit, and vegetables (3) oils and fats (4) dairy
#' products (5) grains and starch (6) bakery and farinaceous products (7) other food products. (Default value
#' is `NULL` which means no reallocation across food categories).
#'
#'
#' @return A list object
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
GenExpenditure <- function(year = 2019,
                           change_food =            rep(0,19),
                           change_gambling =        rep(0,9),
                           change_tobacco_licit =   rep(0,2),
                           change_tobacco_illicit = rep(0,2),
                           change_alcohol_on =      rep(0,4),
                           change_alcohol_off =     rep(0,4),
                           reallocate_food = NULL){

  y <- copy(year)

  ###################################
  #### Changes in food expenditure

  food_data <- cdohio.mod::food_expenditure[year == y,]

  food_data[, pct_change := change_food]

  food_data[, exp_change := ((1 + pct_change) * exp_food_mn) - exp_food_mn]

  food_data <- food_data[, .(exp_change = sum(exp_change)), by = "cpa"]

  food_vec <- c(as.numeric(food_data[cpa == "meat"              ,"exp_change"]),
                as.numeric(food_data[cpa == "fish_fruit_and_veg","exp_change"]),
                as.numeric(food_data[cpa == "oils_and_fats"     ,"exp_change"]),
                as.numeric(food_data[cpa == "dairy"             ,"exp_change"]),
                as.numeric(food_data[cpa == "grains_and_starch" ,"exp_change"]),
                as.numeric(food_data[cpa == "bakery"            ,"exp_change"]),
                as.numeric(food_data[cpa == "other_food"        ,"exp_change"]),
                as.numeric(food_data[cpa == "soft_drinks"       ,"exp_change"]))

  ### Edit 22/08/2025 - add another element for out of home - zero in this case,
  ### but keeps the data structure consistent with InputExpenditure() which has
  ### an entry for out of home

  food_vec <- c(food_vec, 0)

  names(food_vec) <- c("meat","fish_fruit_and_veg","oils_and_fats","dairy",
                       "grains_and_starch","bakery","other_food","soft_drinks","out_of_home")

  ## reallocate change in spending on food to the selected category

  if (!is.null(reallocate_food)){

    j <- reallocate_food

    food_exp_all_reallocate <- -1*sum(food_vec)

    food_vec[j] <- food_vec[j] + food_exp_all_reallocate

  }

  #########################################
  #### Changes in gambling expenditure

  gambling_data <- cdohio.mod::gambling_expenditure[year == y,]

  gambling_data[, pct_change := change_gambling]

  gambling_data[, exp_change := ((1 + pct_change) * ggy_mn) - ggy_mn]

  gambling_data <- gambling_data[, .(exp_change = sum(exp_change))]

  gambling_vec <- as.numeric(gambling_data[,"exp_change"])

  names(gambling_vec) <- c("gambling")

  #########################################
  #### Changes in tobacco expenditure

  ## generate a figure for tobacco in basic prices as well as purchaser prices using
  ## the tax % of spending in the data and 14.26% as the import % from the 2021 supply
  ## table combined alcohol and tobacco category.
  ## Assumes tobacco import % = joint tobacco and alcohol import % and distributors trading
  ## margin for tobacco = 0

  tobacco_data <- cdohio.mod::tobacco_expenditure[year == y,]

  ## get percentage changes
  tobacco_data[tobacco_category == "cigarettes", pct_change_licit := change_tobacco_licit[1] ]
  tobacco_data[tobacco_category == "cigarettes", pct_change_illicit := change_tobacco_illicit[1] ]
  tobacco_data[tobacco_category == "hrt", pct_change_licit := change_tobacco_licit[2] ]
  tobacco_data[tobacco_category == "hrt", pct_change_illicit := change_tobacco_illicit[2] ]

  ## calculate expenditure changes
  tobacco_data[, exp_change_licit := ((1 + pct_change_licit) * spend_licit_mn) - spend_licit_mn]
  tobacco_data[, exp_change_licit_bp := exp_change_licit * (1 - tax_pct - 0.1426)]
  tobacco_data[, exp_change_illicit := ((1 + pct_change_illicit) * spend_illicit_mn) - spend_illicit_mn]

  ## calculate change in tax take
  tobacco_data[, tax := exp_change_licit * tax_pct]

  ## sum over all tobacco products
  tobacco_data <- tobacco_data[, .(exp_change_licit = sum(exp_change_licit),
                                   exp_change_licit_bp = sum(exp_change_licit_bp),
                                   exp_change_illicit = sum(exp_change_illicit),
                                   tax_change = sum(tax))]

  ## extract outputs
  tobacco_vec <- c(as.numeric(tobacco_data[,"exp_change_licit"]),
                   as.numeric(tobacco_data[,"exp_change_licit_bp"]),
                   as.numeric(tobacco_data[,"exp_change_illicit"]))

  tobacco_tax <- as.numeric(tobacco_data[,"tax_change"])

  names(tobacco_vec) <- c("tobacco_l","tobacco_l_bp","tobacco_i")

  #########################################
  #### Changes in alcohol expenditure

  ## generate a figure for alcohol in basic prices as well as purchaser prices using
  ## the tax % of spending in the data, 14.26% as the import % from the 2021 supply
  ## table combined alcohol and tobacco category, and 25.15% as the distributor trading margin.
  ## Assumes alcohol import % = joint tobacco and alcohol import % and distributors trading
  ## margin for alcohol = joint tobacco and alcohol

  alcohol_data <- cdohio.mod::alcohol_expenditure[year == y,]

  ## get percentage changes
  alcohol_data[alcohol_category == "beer",    pct_change_off := change_alcohol_off[1] ]
  alcohol_data[alcohol_category == "cider",   pct_change_off := change_alcohol_off[2] ]
  alcohol_data[alcohol_category == "wine",    pct_change_off := change_alcohol_off[3] ]
  alcohol_data[alcohol_category == "spirits", pct_change_off := change_alcohol_off[4] ]
  alcohol_data[alcohol_category == "beer",    pct_change_on := change_alcohol_on[1] ]
  alcohol_data[alcohol_category == "cider",   pct_change_on := change_alcohol_on[2] ]
  alcohol_data[alcohol_category == "wine",    pct_change_on := change_alcohol_on[3] ]
  alcohol_data[alcohol_category == "spirits", pct_change_on := change_alcohol_on[4] ]

  ## calculate expenditure changes
  alcohol_data[, exp_change_alcohol_off := ((1 + pct_change_off) * exp_alcohol_off_mn) - exp_alcohol_off_mn]
  alcohol_data[, exp_change_alcohol_off_bp := exp_change_alcohol_off * (1 - tax_off_pct - 0.1426 - 0.2515)] ## use 2021 import/DTM figures

  alcohol_data[, exp_change_alcohol_on := ((1 + pct_change_on) * exp_alcohol_on_mn) - exp_alcohol_on_mn]
  alcohol_data[, exp_change_alcohol_on_bp := exp_change_alcohol_on * (1 - tax_on_pct - 0.0242 - 0)] ## use 2021 import/DTM figures

  ## calculate tax changes
  alcohol_data[, tax_off := exp_change_alcohol_off * tax_off_pct]
  alcohol_data[, tax_on := exp_change_alcohol_on * tax_on_pct]

  ## sum over all alcohol products

  alcohol_off_data <- alcohol_data[, .(exp_change_alcohol_off = sum(exp_change_alcohol_off),
                                       exp_change_alcohol_off_bp = sum(exp_change_alcohol_off_bp),
                                       tax_change_off = sum(tax_off) )]


  alcohol_on_data <- alcohol_data[, .(exp_change_alcohol_on = sum(exp_change_alcohol_on),
                                      exp_change_alcohol_on_bp = sum(exp_change_alcohol_on_bp),
                                      tax_change_on = sum(tax_on) )]

  ## extract outputs
  alcohol_off_vec <- c(as.numeric(alcohol_off_data[,"exp_change_alcohol_off"]),
                       as.numeric(alcohol_off_data[,"exp_change_alcohol_off_bp"]))

  alcohol_off_tax <- as.numeric(alcohol_off_data[,"tax_change_off"])

  names(alcohol_off_vec) <- c("alcohol_off","alcohol_off_bp")



  alcohol_on_vec <- c(as.numeric(alcohol_on_data[,"exp_change_alcohol_on"]),
                      as.numeric(alcohol_on_data[,"exp_change_alcohol_on_bp"]))

  alcohol_on_tax <- as.numeric(alcohol_on_data[,"tax_change_on"])

  names(alcohol_on_vec) <- c("alcohol_on","alcohol_on_bp")

  ###################################
  #### Return vectors as a list

  return(list(food = food_vec,
              gambling = gambling_vec,
              tobacco = tobacco_vec,
              alcohol_off = alcohol_off_vec,
              alcohol_on = alcohol_on_vec,
              tob_tax = tobacco_tax,
              alc_off_tax = alcohol_off_tax,
              alc_on_tax = alcohol_on_tax))

}
