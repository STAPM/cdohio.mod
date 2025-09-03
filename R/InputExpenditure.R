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
#' @param input_data Data table. If model_type = "healthei", it must contain the following columns - `CPA`, `Product`, and `exp_change`
#' @param model_type Character. A public health model being used to supply inputs. One of c("none","healthei","stapm")
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
InputExpenditure <- function(input_data,
                             model_type){

  ###################################
  #### Changes in food expenditure

  if (model_type == "healthei"){

  food_data <- input_data[, .(exp_change = sum(exp_change)), by = c("CPA","Product")]

  food_vec <- c(as.numeric(food_data[Product == "Preserved meat and meat products"                                         ,"exp_change"]),
                as.numeric(food_data[Product == "Processed and preserved fish, crustaceans, molluscs, fruit and vegetables","exp_change"]),
                as.numeric(food_data[Product == "Vegetable and animal oils and fats"                                       ,"exp_change"]),
                as.numeric(food_data[Product == "Dairy products"                                                           ,"exp_change"]),
                as.numeric(food_data[Product == "Grain mill products, starches and starch products"                        ,"exp_change"]),
                as.numeric(food_data[Product == "Bakery and farinaceous products"                                          ,"exp_change"]),
                as.numeric(food_data[Product == "Other food products"                                                      ,"exp_change"]),
                as.numeric(food_data[Product == "Soft drinks"                                                              ,"exp_change"]),
                as.numeric(food_data[Product == "Food and beverage serving services"                                       ,"exp_change"]))

  } else {

  food_vec <- rep(0,9)

  }

  #########################################
  #### Changes in gambling expenditure


  gambling_vec <- 0


  ##########################################
  #### TOBACCO AND ALCOHOL #################


  if (model_type == "stapm"){

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


  } else {

  tobacco_vec <- rep(0,3)
  alcohol_on_vec <- rep(0,2)
  alcohol_off_vec <- rep(0,2)
  tobacco_tax <- 0
  alcohol_on_tax <- 0
  alcohol_off_tax <- 0

  }

  names(food_vec) <- c("meat","fish_fruit_and_veg","oils_and_fats","dairy",
                       "grains_and_starch","bakery","other_food","soft_drinks","out_of_home")
  names(gambling_vec) <- c("gambling")
  names(alcohol_on_vec) <- c("alcohol_on","alcohol_on_bp")
  names(alcohol_off_vec) <- c("alcohol_off","alcohol_off_bp")
  names(tobacco_vec) <- c("tobacco_l","tobacco_l_bp","tobacco_i")


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
