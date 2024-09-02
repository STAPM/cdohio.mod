#' Input-Output Model
#'
#' A wrapper function to produce input-output analysis of the effects on the UK
#' economy of changes in the demand
#'
#' @param year Numeric. Year of expenditure, tax, and labour market outcomes data to use.
#' @param year_io Numeric. Year of input-output tables to use (select one from 2017. 2018, 2019, or 2020) - default is 2020.
#' @param reallocate_prop Numeric. Proportion of total change in spending reallocated to other products (0 to 1) - default is 1.
#' @param excluded_products Character vector. Products to exclude from reallocation. The products excluded can be any from
#' c("alcohol","tobacco","food","gambling") - default is to exclude all four categories.
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
#' @param change_alcohol Numeric vector. Percentage change in expenditure on 4 alcohol
#' product categories (beer, cider, spirits, wine)
#'
#' @return List object
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
cdohio <- function(year = 2020,
                   year_io = 2020,
                   reallocate_prop = 1.00,
                   excluded_products = NULL,
                   change_food = rep(0, 19),
                   change_gambling = rep(0, 9),
                   change_tobacco_licit = rep(0, 2),
                   change_tobacco_illicit = rep(0, 2),
                   change_alcohol = rep(0, 4)){

  ###########################################################
  ##### (1) Derive vectors of expenditure changes

  scenario <- GenExpenditure(year = year,
                             change_food = change_food,
                             change_gambling = change_gambling,
                             change_tobacco_licit = change_tobacco_licit,
                             change_tobacco_illicit = change_tobacco_illicit,
                             change_alcohol = change_alcohol)

  exp_food      <- sum(scenario$food)
  exp_gambling  <- sum(scenario$gambling)
  exp_alcohol   <- scenario$alcohol[["alcohol"]]
  exp_tobacco_l <- scenario$tobacco[["tobacco_l"]]
  exp_tobacco_i <- scenario$tobacco[["tobacco_i"]]

  expenditure_change <- data.table(category = c("food","gambling","alcohol","tobacco (licit)","tobacco (illicit)"),
                                   expenditure_change = c(exp_food,exp_gambling,exp_alcohol,exp_tobacco_l,exp_tobacco_i))


  ###########################################################
  ##### (2) Derive vectors of expenditure changes

  demand   <- DemandVector(year_io = year_io,
                           reallocate_prop = reallocate_prop,
                           excluded_products = excluded_products,
                           food_vec     = scenario$food,
                           gambling_vec = scenario$gambling,
                           tobacco_vec  = scenario$tobacco,
                           alcohol_vec  = scenario$alcohol)

  ###########################################################
  ##### (3) Derive vectors of expenditure changes

  impacts <- EconImpactCalc(year = year,
                            year_io = year_io,
                            input_vector = demand,
                            alcohol_tax = scenario$alc_tax,
                            tobacco_tax = scenario$tob_tax)

  #############################################
  ##### (4) Derive Output and GVA Multipliers

  multipliers <- Multipliers(year_io = year_io)

  ##############################
  #### Return ##################

  return(list(expenditure_change = expenditure_change,
              effects = impacts$effects,
              type0_effects_by_product = impacts$type0_effects_by_product,
              type1_effects_by_product = impacts$type1_effects_by_product,
              type2_effects_by_product = impacts$type2_effects_by_product,
              multipliers = multipliers))

}
