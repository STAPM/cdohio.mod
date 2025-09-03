#' Input-Output Model
#'
#' A wrapper function to produce input-output analysis of the effects on the UK
#' economy of changes in the demand for combinations of alcohol, tobacco, food, and gambling. The model estimates
#' the impact of changes in final demand for unhealthy commodities, and the resulting changes in demand for other products,
#' on output, gross value added, employment, government tax receipts, and aggregate household earnings.
#'
#' Using publicly available data sources on expenditures on alcohol, tobacco, food, and gambling, the model
#' (i) calculates changes in expenditures at purchaser prices for user-specified proportionate changes in
#' expenditure, (ii) reallocates spending to other products according to user-specified options, (iii) converts
#' expenditures on all products from purchaser prices to basic prices by subtracting tax and imports, (iv) applies
#' the selected input-output to the resulting changes in demand in basic prices, and (v) estimates the consequent
#' economic outcomes.
#'
#' @param year Numeric. Year of expenditure, tax, and labour market outcomes data to use - default is 2019.
#' @param year_io Numeric. Year of input-output tables to use (select one from 2017. 2018, 2019, or 2020) - default is 2019.
#' @param base Numeric. Base year to use for inflation adjustment - default is 2019.
#' @param model_type Character. A public health model being used to supply inputs. One of c("none","healthei","stapm"). If "none" publicly
#' available aggregate data are used to calculate inputs with `GenExpenditure`. Otherwise `InputExpenditure` is used to process pre-prepared
#' changes in demand data from the respective model into inputs for the analysis.
#' @param input_data Data table. Input data on changes in demand derived from external sources.
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
#' @param change_alcohol_on Numeric vector. Percentage change in expenditure on 4 alcohol
#' product categories in the on-trade (beer, cider, spirits, wine)
#' @param change_alcohol_off Numeric vector. Percentage change in expenditure on 4 alcohol
#' product categories in the off-trade (beer, cider, spirits, wine)
#' @param reallocate_food Numeric. Reallocating changed expenditure on food to other food categories.
#' Select one of the seven food categories in the input-output model to reallocate the changed spending
#' on food to; (1) meat and meat products (2) fish, fruit, and vegetables (3) oils and fats (4) dairy
#' products (5) grains and starch (6) bakery and farinaceous products (7) other food products. (Default value
#' is `NULL` which means no reallocation across food categories).
#' @param consumption_category Numeric integer. Takes on a value of 1-36 or NULL (default). If NULL, reallocated spending is
#' distributed on a pro-rata basis across the aggregate distribution of consumption across all 36 consumption categories. If
#' a value is specified, all consumption is allocated to the respective consumption category (See `cdohio.mod::coicop_categories` for an
#' index of the 36 consumption category). Note that this overrides any
#' restrictions imposed by `excluded_products` e.g. if "alcohol" is specified but `consumption_category` is set equal to 3
#' (alcoholic beverages), all expenditure will be reallocated to alcoholic beverages.
#' @param govt_spend Numeric. Changes in central government spending.
#' @param govt_allocation Numeric. options for the allocation of changes in central government spending. 1 for pro-rata
#' allocation according to the input-output tables. 2 for full allocation to health (CPA_Q86).
#'
#' @return List object with the following elements:
#'
#' \describe{
#'   \item{`expenditure_change`}{Change in expenditure in £millions}
#'   \item{`final_demand`}{Modelled vector of final demand changes across 105 products}
#'   \item{`effects`}{Aggregate economic impacts - direct, indirect, and induced}
#'   \item{`effects_pct`}{Aggregate economic impacts - direct, indirect, and induced (percent of UK total)}
#'   \item{`baseline`}{Totals of each economic outcome in the UK}
#'   \item{`type0_effects_by_product`}{direct economic impacts for each product}
#'   \item{`type1_effects_by_product`}{direct + indirect economic impacts for each product}
#'   \item{`type2_effects_by_product`}{direct + indirect + induced economic impacts for each product}
#'   \item{`multipliers`}{type1 and type2 multipliers for output and gross value added}
#' }
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
cdohio <- function(year = 2019,
                   year_io = 2019,
                   base = 2019,
                   model_type = "none",
                   input_data = NULL,
                   reallocate_prop = 1.00,
                   excluded_products = NULL,
                   change_food = rep(0, 19),
                   change_gambling = rep(0, 9),
                   change_tobacco_licit = rep(0, 2),
                   change_tobacco_illicit = rep(0, 2),
                   change_alcohol_on = rep(0, 4),
                   change_alcohol_off = rep(0, 4),
                   reallocate_food = NULL,
                   consumption_category = NULL,
                   govt_spend = 0,
                   govt_allocation = 1){

  ###########################################################
  ##### (1) Derive vectors of expenditure changes

  if (model_type == "none"){

  scenario <- GenExpenditure(year = year,
                             change_food = change_food,
                             change_gambling = change_gambling,
                             change_tobacco_licit = change_tobacco_licit,
                             change_tobacco_illicit = change_tobacco_illicit,
                             change_alcohol_on = change_alcohol_on,
                             change_alcohol_off = change_alcohol_off,
                             reallocate_food = reallocate_food)
  } else {

  scenario <- InputExpenditure(input_data = input_data,
                               model_type = model_type)
  }

  exp_food        <- sum(scenario$food)
  exp_gambling    <- sum(scenario$gambling)
  exp_alcohol_on  <- scenario$alcohol_on[["alcohol_on"]]
  exp_alcohol_off <- scenario$alcohol_off[["alcohol_off"]]
  exp_tobacco_l   <- scenario$tobacco[["tobacco_l"]]
  exp_tobacco_i   <- scenario$tobacco[["tobacco_i"]]

  expenditure_change <- data.table(category = c("food",
                                                "gambling",
                                                "alcohol_on",
                                                "alcohol_off",
                                                "tobacco (licit)",
                                                "tobacco (illicit)"),
                                   expenditure_change = c(exp_food,
                                                          exp_gambling,
                                                          exp_alcohol_on,
                                                          exp_alcohol_off,
                                                          exp_tobacco_l,
                                                          exp_tobacco_i))

  ###############################################################
  ##### (2a) Derive vectors of expenditure changes (households)

  hhold    <- DemandVector(year_io = year_io,
                           reallocate_prop = reallocate_prop,
                           excluded_products = excluded_products,
                           food_vec         = scenario$food,
                           gambling_vec     = scenario$gambling,
                           tobacco_vec      = scenario$tobacco,
                           alcohol_off_vec  = scenario$alcohol_off,
                           alcohol_on_vec   = scenario$alcohol_on,
                           consumption_category = consumption_category)

  ###############################################################
  ##### (2b) Derive vectors of expenditure changes (govt)

  govt <- GovtSpendVector(year_io = year_io,
                          govt_spend = govt_spend,
                          govt_allocation = govt_allocation)

  demand <- hhold + govt

  final_demand <- copy(cdohio.mod::cpa_categories)
  final_demand[, final_demand := demand]

  ###########################################################
  ##### (3) Derive vectors of expenditure changes

  impacts <- EconImpactCalc(year = year,
                            year_io = year_io,
                            base = base,
                            input_vector = demand,
                            alcohol_off_tax = scenario$alc_off_tax,
                            alcohol_on_tax = scenario$alc_on_tax,
                            tobacco_tax = scenario$tob_tax)

  #############################################
  ##### (4) Derive Output and GVA Multipliers

  multipliers <- Multipliers(year_io = year_io)

  ################
  #### Return ####

  return(list(expenditure_change = expenditure_change,
              final_demand = final_demand,
              effects = impacts$effects,
              effects_pct = impacts$effects_pct,
              baseline = impacts$baseline,
              type0_effects_by_product = impacts$type0_effects_by_product,
              type1_effects_by_product = impacts$type1_effects_by_product,
              type2_effects_by_product = impacts$type2_effects_by_product,
              multipliers = multipliers))

}




