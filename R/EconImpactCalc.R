#' Calculate Economic Impacts
#'
#' Apply the input-output model parameters to a pre-specified vector of changes in final demand for 105 products. Calculate
#' impacts on output, gross value added, tax receipts to government, employment, and earnings. Three types of effect are
#' reported - the direct effects, direct + indirect effects, and direct + indirect + induced effects.
#'
#' @param year Numeric. Year of input-output tables to use (select one from 2017. 2018, 2019, or 2020) - default is 2020.
#' @param input_vector Numeric vector. The vector of length 105 of changes in final demand in basic prices
#'
#' @return A list of outputs
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
EconImpactCalc <- function(year = 2020,
                           input_vector){

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

  L0 <- diag(nrow(inputoutput$L))

  products <- inputoutput$product_categories_io

  output_to_supply <- as.matrix(inputoutput$supply[,"output_to_supply"])

  tax_pct <- as.matrix(inputoutput$supply[,"tax_pct"])

  ###############################################################################
  ### extract wage data for the chosen year and calculate income tax and NICs ###
  ###############################################################################

  inctax <- TaxCalc(earn_data = cdohio.mod::lfs_wage_data,
                    tax_data = cdohio.mod::income_tax_params,
                    year = year)

  gross_earnings <- inctax$earn
  net_earnings <- inctax$net_earn
  inc_tax_nics <- inctax$total_employee_tax

  ################################
  #### DIRECT IMPACTS ############

  ### calculate change in output
  output_vec <- L0 %*% input_vector

  ### calculate change in gva
  gva_vec <- output_vec * inputoutput$gva_coefficients

  ### calculate change in compensation of employees
  coe_vec <- output_vec * inputoutput$coe_coefficients

  ### calculate change in tax
  tax_production <- output_vec * inputoutput$gva_coefficients
  tax_products <- output_vec * output_to_supply * tax_pct

  tax_vec <- tax_products + tax_production

  ### calculate change in employment
  fte_vec <- output_vec * cdohio.mod::fte_coefficients

  ## calculate change in wages and taxes
  earn_vec         <- (gross_earnings * fte_vec) / 1000000
  net_earn_vec     <- (net_earnings * fte_vec) / 1000000
  inc_tax_nics_vec <- (inc_tax_nics * fte_vec) / 1000000

  type0_effects <- cbind(products, output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)
  setDT(type0_effects)
  setnames(type0_effects, "output_to_supply", "tax_vec")

  rm(output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)

  ###########################################
  #### DIRECT + INDIRECT IMPACTS ############

  ### calculate change in output
  output_vec <- inputoutput$L %*% input_vector

  ### calculate change in gva
  gva_vec <- output_vec * inputoutput$gva_coefficients

  ### calculate change in compensation of employees
  coe_vec <- output_vec * inputoutput$coe_coefficients

  ### calculate change in tax
  tax_production <- output_vec * inputoutput$gva_coefficients
  tax_products <- output_vec * output_to_supply * tax_pct

  tax_vec <- tax_products + tax_production

  ### calculate change in employment
  fte_vec <- output_vec * cdohio.mod::fte_coefficients

  ## calculate change in wages
  earn_vec         <- (gross_earnings * fte_vec) / 1000000
  net_earn_vec     <- (net_earnings * fte_vec) / 1000000
  inc_tax_nics_vec <- (inc_tax_nics * fte_vec) / 1000000

  type1_effects <- cbind(products, output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)
  setDT(type1_effects)
  #setnames(type1_effects, "output_to_supply", "tax_vec")

  rm(output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)

  #####################################################
  #### DIRECT + INDIRECT + INDUCED IMPACTS ############

  ### calculate change in output
  output_vec <- inputoutput$L2 %*% c(input_vector,0)
  output_vec <- output_vec[1:105,1]

  ### calculate change in gva
  gva_vec <- output_vec * inputoutput$gva_coefficients

  ### calculate change in compensation of employees
  coe_vec <- output_vec * inputoutput$coe_coefficients

  ### calculate change in tax
  tax_production <- output_vec * inputoutput$gva_coefficients
  tax_products <- output_vec * output_to_supply * tax_pct

  tax_vec <- tax_products + tax_production

  ### calculate change in employment
  fte_vec <- output_vec * cdohio.mod::fte_coefficients

  ## calculate change in wages
  earn_vec         <- (gross_earnings * fte_vec) / 1000000
  net_earn_vec     <- (net_earnings * fte_vec) / 1000000
  inc_tax_nics_vec <- (inc_tax_nics * fte_vec) / 1000000

  type2_effects <- cbind(products, output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)
  setDT(type2_effects)
  setnames(type2_effects, "output_to_supply", "tax_vec")

  rm(output_vec, gva_vec, tax_vec, fte_vec, earn_vec, net_earn_vec, inc_tax_nics_vec)

  ##########################
  ##### Gather outputs #####

  type0_effects_aggregate <- type0_effects
  type1_effects_aggregate <- type1_effects
  type2_effects_aggregate <- type2_effects

  setDT(type0_effects_aggregate)
  setDT(type1_effects_aggregate)
  setDT(type2_effects_aggregate)

  type0_effects_aggregate[, type := "Direct"]
  type1_effects_aggregate[, type := "Direct + Indirect"]
  type2_effects_aggregate[, type := "Direct + Indirect + Induced"]

  effects <- rbindlist(list(type0_effects_aggregate,
                            type1_effects_aggregate,
                            type2_effects_aggregate))

  effects <- effects[, .(output_vec = sum(output_vec),
                         gva_vec = sum(gva_vec),
                         tax_vec = sum(tax_vec),
                         fte_vec = sum(fte_vec),
                         earn_vec = sum(earn_vec),
                         net_earn_vec = sum(net_earn_vec),
                         inc_tax_nics_vec = sum(inc_tax_nics_vec)), by = "type"]

  return(list(effects = effects,
              type0_effects = type0_effects,
              type1_effects = type1_effects,
              type2_effects = type2_effects))

}
