#' Multipliers
#'
#' Calculate multipliers for a selected year of input-output product by product tables to show the change in output and
#' gross value added (GVA) for a one-unit change in final demand.
#'
#' @param year_io Numeric. Year of input-output tables to use (select one from 2017. 2018, 2019, or 2020) - default is 2020.
#'
#' @return A data table of multipliers
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
Multipliers <- function(year_io = 2020){

  ###############################################
  ### extract the selected input-output table ###
  ###############################################

  if (year_io == 2017){
    inputoutput <- cdohio.mod::inputoutput_2017
  }
  if (year_io == 2018){
    inputoutput <- cdohio.mod::inputoutput_2018
  }
  if (year_io == 2019){
    inputoutput <- cdohio.mod::inputoutput_2019
  }
  if (year_io == 2020){
    inputoutput <- cdohio.mod::inputoutput_2020
  }

  ####################################################
  ### Type 1 Multipliers (direct + indirect effects)

  ## output multipliers - sum of each column of the Leontief (or equivalently,
  ## a row vector of ones multiplied by the Leontief)

  L1 <- inputoutput$L

  out_vec1 <- rep(1,ncol(L1))

  output_multipliers_t1 <- t(out_vec1 %*% L1)[1:105,]

  ## GVA multipliers - a row vector of GVA coefficients (GVA as % of total output)
  ## multiplied by the Leontief

  gva_vec1 <- inputoutput$gva_coefficients

  gva_multipliers_t1 <- t(gva_vec1 %*% L1)[1:105,]

  ##############################################################
  ### Type 2 Multipliers (direct + indirect + induced effects)

  ## output multipliers - sum of each column of the Leontief (or equivalently,
  ## a row vector of ones multiplied by the Leontief)

  L2 <- inputoutput$L2

  out_vec2 <- rep(1,ncol(L2))

  output_multipliers_t2 <- t(out_vec2 %*% L2)[1:105,]

  ## GVA multipliers - a row vector of GVA coefficients (GVA as % of total output)
  ## multiplied by the Leontief

  gva_vec2 <- c(inputoutput$gva_coefficients,0)

  gva_multipliers_t2 <- t(gva_vec2 %*% L2)[1:105,]


  #######################################################
  #### Create a data table and return the multipliers

  supply <- inputoutput$supply

  multipliers <- data.table(Product = supply$Product,
                            output_multipliers_t1 = output_multipliers_t1,
                            output_multipliers_t2 = output_multipliers_t2,
                            gva_multipliers_t1 = gva_multipliers_t1,
                            gva_multipliers_t2 = gva_multipliers_t2)

  return(multipliers)

}
