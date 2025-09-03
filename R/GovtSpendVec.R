#' Generate Government Spending Change Input Vector
#'
#' Construct a vector of changes in government spending to add as inputs to
#' changes in household final demand and include in the input-output model.
#'
#' @param year_io Numeric. Year of input-output tables to use (select one from 2017. 2018, 2019, or 2020) - default is 2019.
#' @param govt_spend Numeric. Changes in central government spending.
#' @param govt_allocation Numeric. options for the allocation of changes in central government spending. 1 for pro-rata
#' allocation according to the input-output tables. 2 for full allocation to health (CPA_Q86).
#'
#' @return A vector of length 105
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
GovtSpendVector <- function(year_io = 2019,
                            govt_spend = 100,
                            govt_allocation = 1){

  ###########################################
  ### extract the selected scaling vector ###
  ###########################################

  if (year_io == 2017){
    spend_vec <- cdohio.mod::govt_spending$govt_2017
  }
  if (year_io == 2018){
    spend_vec <- cdohio.mod::govt_spending$govt_2018
  }
  if (year_io == 2019){
    spend_vec <- cdohio.mod::govt_spending$govt_2019
  }
  if (year_io == 2020){
    spend_vec <- cdohio.mod::govt_spending$govt_2020
  }
  if (year_io == 2021){
    spend_vec <- cdohio.mod::govt_spending$govt_2021
  }
  if (year_io == 2022){
    spend_vec <- cdohio.mod::govt_spending$govt_2022
  }

  ####################################
  ### allocate government spending ###

  if (govt_allocation == 1){
  government_spending_vector <- spend_vec * govt_spend
  }

  if (govt_allocation == 2){
  government_spending_vector <- rep(0, 105)
  government_spending_vector[96] <- govt_spend
  }

  return(government_spending_vector)

}
