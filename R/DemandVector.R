#' Generate Demand Change Input Vector
#'
#' Take inputs created with the `GenExpenditure` function and construct a vector of changes in
#' final demand to use as the input to the input-output model.
#'
#' @param year Numeric. Year of input-output tables to use (select one from 2017. 2018, 2019, or 2020) - default is 2020.
#' @param reallocate_prop Numeric. Proportion of total change in spending reallocated to other products (0 to 1) - default is 1.
#' @param excluded_products Character vector. Products to exclude from reallocation. The products excluded can be any from
#' c("alcohol","tobacco","food","gambling") - default is to exclude all four categories.
#' @param conversion_matrix Data table. A matrix used for mapping between CPA product categories.
#' @param food_vec Numeric vector. Vector of changes in demand for food products (7)
#' @param gambling_vec Numeric. Change in demand for all gambling (1).
#' @param tobacco_vec Numeric vector. Change in demand for tobacco products (3) - licit tobacco in purchaser prices, licit
#' tobacco in basic prices, and illicit tobacco
#' @param alcohol_vec Numeric vector. Change in demand for all alcohol (2) - all alcohol in purchaser prices, and all alcohol
#' in basic prices
#'
#' @return A vector of length 105
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
DemandVector <- function(year = 2020,
                         reallocate_prop = 1.00,
                         excluded_products = c("alcohol","tobacco","food","gambling")[1:4],
                         conversion_matrix = cdohio.mod::cpa_conversion_matrix,
                         food_vec,
                         gambling_vec,
                         tobacco_vec,
                         alcohol_vec){

  ###############################################
  ### extract the selected input-output table ###
  ###############################################

  if (year == 2017){
    inputoutput <- cdohio.mod::inputoutput_2017
  }
  if (year == 2018){
    inputoutput <- cdohio.mod::inputoutput_2018
  }
  if (year == 2019){
    inputoutput <- cdohio.mod::inputoutput_2019
  }
  if (year == 2020){
    inputoutput <- cdohio.mod::inputoutput_2020
  }

  ###############################################
  ### make vectors of different reallocations ###
  ### of spending to COICOP categories        ###
  ###############################################

  ### (1) ###
  hhfce_vector_all <- as.vector(inputoutput$hhfce_totals/sum(inputoutput$hhfce_totals))

  ## vector to split spending across 36 consumption groups

  ### (2) ###
  hhfce_vector_nofood <- copy(hhfce_vector_all)
  hhfce_vector_nofood[1:2] <- 0
  hhfce_vector_nofood <- hhfce_vector_nofood/sum(hhfce_vector_nofood)

  ### (3) ###
  hhfce_vector_noalc <- copy(hhfce_vector_all)
  hhfce_vector_noalc[3] <- 0
  hhfce_vector_noalc <- hhfce_vector_noalc/sum(hhfce_vector_noalc)

  ### (4) ###
  hhfce_vector_notob <- copy(hhfce_vector_all)
  hhfce_vector_notob[4] <- 0
  hhfce_vector_notob <- hhfce_vector_notob/sum(hhfce_vector_notob)

  ### (5) ###
  hhfce_vector_noalcfood <- copy(hhfce_vector_all)
  hhfce_vector_noalcfood[1:3] <- 0
  hhfce_vector_noalcfood <- hhfce_vector_noalcfood/sum(hhfce_vector_noalcfood)

  ### (6) ###
  hhfce_vector_noalctob <- copy(hhfce_vector_all)
  hhfce_vector_noalctob[3:4] <- 0
  hhfce_vector_noalctob <- hhfce_vector_noalctob/sum(hhfce_vector_noalctob)

  ### (7) ###
  hhfce_vector_notobfood <- copy(hhfce_vector_all)
  hhfce_vector_notobfood[c(1:2,4)] <- 0
  hhfce_vector_notobfood <- hhfce_vector_notobfood/sum(hhfce_vector_notobfood)

  ### (8) ###
  hhfce_vector_noalctobfood <- copy(hhfce_vector_all)
  hhfce_vector_noalctobfood[1:4] <- 0
  hhfce_vector_noalctobfood <- hhfce_vector_noalctobfood/sum(hhfce_vector_noalctobfood)

  ## get a vector for recreational and cultural services that excludes gambling
  ## this is a vector that will replace the "recreational and cultural services"
  ## column in the matrix if gambling is to be excluded from the reallocation. It
  ## pro-rata reallocates the distribution of spending on gambling services across all
  ## other CPA categories within this column

  ### (9) ###
  hhfce_vector_nogamb <- inputoutput$hhfce[,31]
  hhfce_vector_nogamb[98] <- 0
  hhfce_vector_nogamb <- hhfce_vector_nogamb/sum(hhfce_vector_nogamb)


  ### Set up hhfce matrix and vectors

  if (!("gambling" %in% excluded_products )){

    hhfce_matrix <- copy(inputoutput$hhfce)

  } else {

    hhfce_matrix <- copy(inputoutput$hhfce)
    hhfce_matrix[,31] <- hhfce_vector_nogamb
  }

  ##########################################
  ### Select the reallocation vectors ######
  ##########################################

  #### NO PRODUCTS EXCLUDED

  if (!("alcohol" %in% excluded_products) & !("tobacco" %in% excluded_products) & !("food" %in% excluded_products) ){
    print("No products excluded")

    hhfce_vector <- copy(hhfce_vector_all)
  }

  #### ONE PRODUCT

  if ("alcohol" %in% excluded_products & !("tobacco" %in% excluded_products) & !("food" %in% excluded_products) ){
    print("Alcohol excluded")

    hhfce_vector <- copy(hhfce_vector_noalc)
  }

  if (!("alcohol" %in% excluded_products) & "tobacco" %in% excluded_products & !("food" %in% excluded_products) ){
    print("Tobacco excluded")

    hhfce_vector <- copy(hhfce_vector_notob)
  }

  if (!("alcohol" %in% excluded_products) & !("tobacco" %in% excluded_products) & "food" %in% excluded_products ){
    print("Food excluded")

    hhfce_vector <- copy(hhfce_vector_nofood)
  }

  #### TWO PRODUCTS

  if ("alcohol" %in% excluded_products & "tobacco" %in% excluded_products & !("food" %in% excluded_products) ){
    print("Alcohol + Tobacco excluded")

    hhfce_vector <- copy(hhfce_vector_noalctob)
  }

  if ("alcohol" %in% excluded_products & !("tobacco" %in% excluded_products) & "food" %in% excluded_products ){
    print("Alcohol + Food excluded")

    hhfce_vector <- copy(hhfce_vector_noalcfood)
  }

  if (!("alcohol" %in% excluded_products) & "tobacco" %in% excluded_products & "food" %in% excluded_products ){
    print("Tobacco + Food excluded")

    hhfce_vector <- copy(hhfce_vector_notobfood)
  }

  #### THREE PRODUCTS

  if ("alcohol" %in% excluded_products & "tobacco" %in% excluded_products & "food" %in% excluded_products ){
    print("Alcohol + Tobacco + Food excluded")

    hhfce_vector <- copy(hhfce_vector_noalctobfood)
  }

  #####################################################################
  ### produce the input vector of change in demand for 105 products ###
  #####################################################################

  scenario <- c(food_vec, gambling_vec, tobacco_vec, alcohol_vec)

  #####################################################################################
  ### convert food + gambling to basic prices (alcohol and tobacco are already done)

  food_meat_bp           <-   (scenario["meat"]                * as.numeric(inputoutput$supply[Product == "Preserved meat and meat products","supply_to_output"]) )
  food_fish_fruit_veg_bp <-   (scenario["fish_fruit_and_veg"]  * as.numeric(inputoutput$supply[Product == "Processed and preserved fish, crustaceans, molluscs, fruit and vegetables","supply_to_output"]) )
  food_oils_fats_bp      <-   (scenario["oils_and_fats"]       * as.numeric(inputoutput$supply[Product == "Vegetable and animal oils and fats","supply_to_output"]) )
  food_dairy_bp          <-   (scenario["dairy"]               * as.numeric(inputoutput$supply[Product == "Dairy products","supply_to_output"]) )
  food_grain_starch_bp   <-   (scenario["grains_and_starch"]   * as.numeric(inputoutput$supply[Product == "Grain mill products, starches and starch products","supply_to_output"]) )
  food_bakery_bp         <-   (scenario["bakery"]              * as.numeric(inputoutput$supply[Product == "Bakery and farinaceous products","supply_to_output"]) )
  food_other_bp          <-   (scenario["other_food"]          * as.numeric(inputoutput$supply[Product == "Other food products","supply_to_output"]) )

  gambling_bp <- scenario["gambling"] * as.numeric(inputoutput$supply[Product == "Gambling and betting services","supply_to_output"])


  #### vector of reallocation of spending #####

  ## total amount of reallocated spending (in purchaser prices)

  spending_change <- reallocate_prop * (scenario["meat"] + scenario["fish_fruit_and_veg"] + scenario["oils_and_fats"] +
                                          scenario["dairy"] + scenario["grains_and_starch"] +
                                          scenario["bakery"] + scenario["other_food"] +
                                          scenario["tobacco_l"] + scenario["tobacco_i"] +
                                          scenario["alcohol"] + scenario["gambling"]) * -1

  ## vector of changes in spending across 36 COICOP categories

  spending_change_vec <- spending_change * hhfce_vector

  ## map onto the CPA categories using the hhfce matrix

  spending_change_cpa <- hhfce_matrix %*% spending_change_vec

  ## map onto the 105 CPA category classification used in the io table



  spending_change_cpa2 <- data.table(CPA_hhfce = inputoutput$product_categories_hhfce[,"CPA_hhfce"] ,spending_change_cpa)
  setnames(spending_change_cpa2, names(spending_change_cpa2), c("CPA_hhfce","V1"))

  merge <- merge(conversion_matrix, spending_change_cpa2, by = "CPA_hhfce", sort = FALSE)

  merge <- merge[, .(reallocated_spend = V1*conversion), by = c("CPA","Product")]

  ## convert from purchaser to basic prices

  supply_merge <- merge(merge, inputoutput$supply, by = c("CPA","Product"))
  supply_merge[, reallocated_spend := reallocated_spend * supply_to_output]
  supply_merge <- supply_merge[, c("CPA","Product","reallocated_spend")]

  ## Retail trade has no supply at purchases prices, so set the reallocated spend here = 0

  supply_merge[Product == "Retail trade services, except of motor vehicles and motorcycles", reallocated_spend := 0]

  ###########################################
  #### Vector of changing final demand ######

  supply_merge[Product == "Preserved meat and meat products", demand_change := food_meat_bp]
  supply_merge[Product == "Processed and preserved fish, crustaceans, molluscs, fruit and vegetables", demand_change := food_fish_fruit_veg_bp]
  supply_merge[Product == "Vegetable and animal oils and fats", demand_change := food_oils_fats_bp]
  supply_merge[Product == "Dairy products", demand_change := food_dairy_bp]
  supply_merge[Product == "Grain mill products, starches and starch products", demand_change := food_grain_starch_bp]
  supply_merge[Product == "Bakery and farinaceous products", demand_change := food_bakery_bp]
  supply_merge[Product == "Other food products", demand_change := food_other_bp]

  supply_merge[Product == "Alcoholic beverages  & Tobacco products", demand_change := scenario["alcohol_bp"] + scenario["tobacco_l_bp"]]

  supply_merge[Product == "Gambling and betting services", demand_change := gambling_bp]

  supply_merge[is.na(demand_change), demand_change := 0]

  ##########################################################################
  #### Combine vectors to get total change in spending in each category ####

  supply_merge[, total_demand_change := demand_change + reallocated_spend]

  input_vector <- supply_merge$total_demand_change


  return(input_vector)


}
