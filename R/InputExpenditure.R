#' Input Demand Changes
#'
#' Directly input changes in demand for alcohol, tobacco, gambling. This function is used to directly
#' input changes in the demand for the unhealthy commodities for which the model calculates economic
#' impact analyses. Use the `GenExpenditure` function to calculate inputs as proportionate changes using
#' the expenditure data saved to the model.
#'
#' Note that for alcohol and tobacco this function also calculates changes in demand in basic prices as well as
#' in purchaser prices. As alcohol and tobacco are combined in the model, and have very different levels of taxation
#' relative to retail price, the conversion from purchaser prices to basic prices is done here separately for
#' alcohol and tobacco rather than within the model, as is the case for other products. Because of this, this function also
#' calculates the change in tax less subsidies on products for alcohol and tobacco so that this is
#' accounted for in the model.
#'
#' @param food_input_data Data table. Must contain the following columns - `CPA`, `Product`, and `exp_change`
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
InputExpenditure <- function(food_input_data){

  ###################################
  #### Changes in food expenditure

  food_data <- food_input_data[, .(exp_change = sum(exp_change)), by = c("CPA","Product")]

  food_vec <- c(as.numeric(food_data[Product == "Preserved meat and meat products"                                         ,"exp_change"]),
                as.numeric(food_data[Product == "Processed and preserved fish, crustaceans, molluscs, fruit and vegetables","exp_change"]),
                as.numeric(food_data[Product == "Vegetable and animal oils and fats"                                       ,"exp_change"]),
                as.numeric(food_data[Product == "Dairy products"                                                           ,"exp_change"]),
                as.numeric(food_data[Product == "Grain mill products, starches and starch products"                        ,"exp_change"]),
                as.numeric(food_data[Product == "Bakery and farinaceous products"                                          ,"exp_change"]),
                as.numeric(food_data[Product == "Other food products"                                                      ,"exp_change"]),
                as.numeric(food_data[Product == "Soft drinks"                                                              ,"exp_change"]),
                as.numeric(food_data[Product == "Food and beverage serving services"                                       ,"exp_change"]))

  names(food_vec) <- c("meat","fish_fruit_and_veg","oils_and_fats","dairy",
                       "grains_and_starch","bakery","other_food","soft_drinks","out_of_home")

  #########################################
  #### Changes in gambling expenditure


  #########################################
  #### Changes in tobacco expenditure

  ## generate a figure for tobacco in basic prices as well as purchaser prices using
  ## the tax % of spending in the data and 14.26% as the import % from the 2021 supply
  ## table combined alcohol and tobacco category.
  ## Assumes tobacco import % = joint tobacco and alcohol import % and distributors trading
  ## margin for tobacco = 0


  #########################################
  #### Changes in alcohol expenditure

  ## generate a figure for alcohol in basic prices as well as purchaser prices using
  ## the tax % of spending in the data, 14.26% as the import % from the 2021 supply
  ## table combined alcohol and tobacco category, and 25.15% as the distributor trading margin.
  ## Assumes alcohol import % = joint tobacco and alcohol import % and distributors trading
  ## margin for alcohol = joint tobacco and alcohol



  ###################################
  #### Return vectors as a list

  return(list(food = food_vec,
              gambling = 0,
              tobacco = rep(0,3),
              alcohol = rep(0,2),
              tob_tax = 0,
              alc_tax = 0))

}
