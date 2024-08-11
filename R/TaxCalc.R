#' Tax Calculator
#'
#' Simulate an earnings distribution for each sector in the model and calculate income
#' tax and national insurance contributions (NICs) estimates
#'
#' @param earn_data Data table. Package data containing average earnings by 105 CPA sectors.
#' @param tax_data Data table. Package data containing the necessary parameters to calculate
#' income tax and national insurance contributions from annual earnings data
#' @param year Integer. Select the year (from 2016 to 2020) of earnings and employment data to use in the analysis.
#'
#' @return Data table of earnings and tax impact estimates
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
TaxCalc <- function(earn_data = cdohio.mod::lfs_wage_data,
                    tax_data = cdohio.mod::income_tax_params,
                    year){

  yr <- copy(year)

  #######################################################################
  #### read in the earnings per employee

  if (yr == 2017){
    earn_data <- earn_data[, earn := earn_2017]
  }
  if (yr == 2018){
    earn_data <- earn_data[, earn := earn_2018]
  }
  if (yr == 2019){
    earn_data <- earn_data[, earn := earn_2019]
  }
  if (yr == 2020){
    earn_data <- earn_data[, earn := earn_2020]
  }
  if (yr == 2021){
    earn_data <- earn_data[, earn := earn_2021]
  }
  if (yr == 2022){
    earn_data <- earn_data[, earn := earn_2022]
  }

  earn_data <- earn_data[, c("SIC","earn")]

  #######################################################################
  ### Extract the income tax system parameters for the analysis year
  ### and annualise the national insurance parameters from weekly

  tax_data <- tax_data[year == yr,]

  # income tax

  personal_allowance     <- as.numeric(tax_data[,"personal_allowance"])
  basic_rate             <- as.numeric(tax_data[,"basic_rate"])
  higher_rate_threshold  <- as.numeric(tax_data[,"higher_thresh"])
  higher_rate            <- as.numeric(tax_data[,"higher_rate"])
  add_rate_threshold     <- as.numeric(tax_data[,"add_thresh"])
  add_rate               <- as.numeric(tax_data[,"add_rate"])

  # employer NICs (weekly, so *52 to get annual)

  employer_nic_threshold <- as.numeric(tax_data[,"employer_nic_thresh_wk"])*52
  employer_nic_rate      <- as.numeric(tax_data[,"employer_nic_rate"])

  # employee NICs (also weekly, need to annualise)

  employee_nic_threshold1 <- as.numeric(tax_data[,"employee_nic_thresh1_wk"])*52
  employee_nic_threshold2 <- as.numeric(tax_data[,"employee_nic_thresh2_wk"])*52

  employee_nic_rate1 <- as.numeric(tax_data[,"employee_nic_rate1"])
  employee_nic_rate2 <- as.numeric(tax_data[,"employee_nic_rate2"])

  # combine into a data table to be exported

  tax_values <- c(personal_allowance, basic_rate,
                  higher_rate_threshold, higher_rate,
                  add_rate_threshold, add_rate,
                  employer_nic_threshold, employer_nic_rate,
                  employee_nic_threshold1, employee_nic_rate1,
                  employee_nic_threshold2, employee_nic_rate2)

  tax_vars   <- c("personal_allowance", "basic_rate",
                  "higher_rate_threshold", "higher_rate",
                  "add_rate_threshold", "add_rate",
                  "employer_nic_threshold", "employer_nic_rate",
                  "employee_nic_threshold1", "employee_nic_rate1",
                  "employee_nic_threshold2", "employee_nic_rate2")

  tax_params <- data.table(tax_vars, tax_values)

  ##########################
  ##### Tax Calculator #####

  ###################################################
  #### SIMULATE INCOME TAX PAID

  ## calculate the amount of taxable earnings within each income tax bracket

  # (1) additional rate
  earn_data[, taxable_add_rate := earn - add_rate_threshold]
  earn_data[taxable_add_rate < 0, taxable_add_rate := 0]

  # (2) higher rate
  earn_data[,taxable_higher_rate := earn - higher_rate_threshold]
  earn_data[taxable_higher_rate < 0 ,taxable_higher_rate := 0]
  earn_data[taxable_higher_rate > (add_rate_threshold - higher_rate_threshold) ,taxable_higher_rate := add_rate_threshold - higher_rate_threshold]

  # (3) basic rate
  earn_data[,taxable_basic_rate := earn - personal_allowance]
  earn_data[taxable_basic_rate < 0 ,taxable_basic_rate := 0]
  earn_data[taxable_basic_rate > (higher_rate_threshold - personal_allowance) ,taxable_basic_rate := higher_rate_threshold - personal_allowance]


  earn_data[, income_tax :=
                basic_rate*taxable_basic_rate +
                higher_rate*taxable_higher_rate +
                add_rate*taxable_add_rate]

  earn_data[, c("taxable_basic_rate","taxable_higher_rate","taxable_add_rate") := NULL]

  ###################################################
  #### SIMULATE NATIONAL INSURANCE (EMPLOYER)

  earn_data[, employer_nic_elig := earn - employer_nic_threshold]
  earn_data[employer_nic_elig < 0, employer_nic_elig := 0]

  earn_data[, employer_nic := employer_nic_rate*employer_nic_elig]

  earn_data[, c("employer_nic_elig") := NULL]

  ###################################################
  #### SIMULATE NATIONAL INSURANCE (EMPLOYEE)

  ## primary (employee) additional contribution
  earn_data[,empl_nic_elig2 := earn - employee_nic_threshold2]
  earn_data[empl_nic_elig2 < 0, empl_nic_elig2 := 0]

  ## primary (employee) main contribution
  earn_data[,empl_nic_elig1 := earn - employee_nic_threshold1]
  earn_data[empl_nic_elig1 < 0, empl_nic_elig1 := 0]
  earn_data[empl_nic_elig1 > (employee_nic_threshold2 - employee_nic_threshold1), empl_nic_elig1 := employee_nic_threshold2 - employee_nic_threshold1]

  earn_data[, employee_nic := employee_nic_rate1*empl_nic_elig1 + employee_nic_rate2*empl_nic_elig2]

  earn_data[, c("empl_nic_elig1","empl_nic_elig2") := NULL]

  ########################
  ### sum up tax components

  earn_data[, total_tax := employee_nic + employer_nic + income_tax]
  earn_data[, total_employee_tax := employee_nic + income_tax]
  earn_data[, net_earn := earn - income_tax - employee_nic]

  return(earn_data)

}
