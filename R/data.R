#' CPA Conversion Matrix
#'
#' A matrix to map the 103 CPA categories in the HHFCE component of the ONS supply and use tables to the 105 CPA categories
#' in the supply table and input-output analytical tables. The difference in dimensions is due to the HHFCE tables (i) merging
#' CPA_27 and CPA_28 into a combined electrical equipment plus machinery and equipment n.e.c category, and (ii) merging CPA H53
#' and CPA I55 into a combined postal and courier services plus accommodation services. The mapping splits expenditure on both
#' of these combined categories equally between the two component products.
#'
#' @format
#' A data frame with 48 rows and 5 columns:
#' \describe{
#'   \item{CPA_hhfce}{CPA codes in the }
#'   \item{tobacco_category}{one of 2 categories of tobacco, cigarettes and hrt}
#'   \item{exp_alcohol_mn}{total spending on licit tobacco (£millions)}
#'   \item{exp_alcohol_mn_bp}{total spending on licit tobacco in basic prices (£millions)}
#'   \item{spend_illicit_mn}{total spending on illicit tobacco (£millions)}
#' }
"cpa_conversion_matrix"

#' Alcohol Expenditure
#'
#' Data on expenditure on beer, cider, wine, and spirits between 2010 and 2021. Expenditure data are
#' obtained from the \href{https://www.publichealthscotland.scot/publications/mesas-monitoring-report-2022/}{2022 MESAS monitoring report}.
#' Data last accessed July 2024. The \href{https://www.gov.uk/government/statistics/alcohol-bulletin}{HMRC Alcohol Bulletin} is used to
#' estimate total tax receipts from alcohol, used to calculate expenditure measured in basic prices.
#'
#' @format
#' A data frame with 48 rows and 4 columns:
#' \describe{
#'   \item{year}{year}
#'   \item{alcohol_category}{one of 4 categories of alcohol; beer, cider, wine, and spirits}
#'   \item{exp_alcohol_mn}{total spending on alcohol (£millions)}
#'   \item{tax_pct}{excise tax and VAT as a percentage of total expenditure}
#' }
#' @source \href{https://www.gov.uk/government/statistics/alcohol-bulletin}{Alcohol Bulletin}
"alcohol_expenditure"

#' Food Expenditure Data
#'
#' Data on expenditure on broad categories of food between 2015 and 2022.
#' Data last accessed July 2024.
#'
#' @format
#' A data frame with 152 rows and 4 columns:
#' \describe{
#'   \item{year}{year}
#'   \item{food_category}{one of 19 categories of food consumption}
#'   \item{cpa}{CPA product category in the input-output model}
#'   \item{exp_food_mn}{annual total expenditure (£millions)}
#' }
#' @source \href{https://www.gov.uk/government/statistical-data-sets/family-food-datasets}{ONS Family Food Datasets}
"food_expenditure"


#' Gambling Expenditure Data
#'
#' Data on the gross gambling yield (GGY) for 9 categories of gambling spending between
#' 2008 and 2022. Data last accessed July 2024.
#'
#' @format
#' A data frame with 135 rows and 3 columns:
#' \describe{
#'   \item{year}{year}
#'   \item{gambling_category}{one of 9 categories of gambling}
#'   \item{ggy_mn}{total gross gambling yield (GGY) (£millions)}
#' }
#' @source \href{https://www.data.gov.uk/dataset/e7032815-5990-4439-b5c8-8553cf5b7fdd/gb-gambling-industry-statistics-february-2024}{Gambling Commission statistics}
"gambling_expenditure"

#' Tobacco Expenditure Data
#'
#' Data on expenditure for 2 categories of tobacco spending (cigarettes and handrolled tobacco) between
#' 2019 and 2022. Data last accessed July 2024.
#'
#' Total licit spending data are constructed by calculating the percentage of
#' the retail price for tobacco products which is made up of duty. These figures are then used to adjust
#' total duty receipts data from the \href{https://www.gov.uk/government/statistics/tobacco-bulletin}{HMRC Tobacco Bulletin} to
#' a figure for total expenditure. Data on prices were obtained from \href{https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceindicescpiandretailpricesindexrpiitemindicesandpricequotes}{ONS consumer price inflation price quotes}
#' data for HRT, and from the Sheffield Tobacco and Alcohol Policy Model (STAPM) for cigarettes in 2017 adjusted for other years using the
#' \href{https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l7aq/mm23}{CPI index for cigarettes}.
#'
#' Illicit spending is calculated using estimates of the total volume of illicit tobacco published by HMRC in the \href{https://www.gov.uk/government/statistics/measuring-tax-gaps-tables}{measuring tax gaps tables}.
#' Consumption is multiplied by the price of illicit tobacco, which is assumed to be half of the price of licit tobacco.
#'
#' @format
#' A data frame with 8 rows and 5 columns:
#' \describe{
#'   \item{year}{year}
#'   \item{tobacco_category}{one of 2 categories of tobacco, cigarettes and hrt}
#'   \item{spend_licit_mn}{total spending on licit tobacco (£millions)}
#'   \item{spend_illicit_mn}{total spending on illicit tobacco (£millions)}
#'   \item{excise_pct}{excise tax as a percentage of total expenditure}
#'   \item{tax_pct}{excise tax and VAT as a percentage of total expenditure}
#' }
#' @source \href{https://www.gov.uk/government/statistics/tobacco-bulletin}{Tobacco Bulletin}
"tobacco_expenditure"


#' UK Input-Output Table (2017)
#'
#' Product by product Input-Output Analytical Tables with 105 product categories for 2017
#' from the Office for National Statistics, with supporting data from the \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{supply and use tables}.
#' including the supply table and household final consumption expenditure (HHFCE).
#'
#' @format
#' A list object with 12 items:
#' \describe{
#'   \item{product_categories_io}{105 CPA product categories in the input-output table}
#'   \item{product_categories_hhfce}{103 CPA product categories in the HHFCE table}
#'   \item{supply}{the supply table}
#'   \item{iot}{the input-output flowtable showing inter-industry use of products}
#'   \item{A}{the matrix of technical coefficients - the input-output table elements as a proportion of total output}
#'   \item{L}{the Leontief inverse (type 1)}
#'   \item{L2}{the Leontief inverse (type 2)}
#'   \item{gva_coefficients}{Gross value added (GVA) as a proportion of total product output}
#'   \item{tax_coefficients}{Net tax on production as a proportion of total product output}
#'   \item{coe_coefficients}{Compensation of employees as a proportion of total product output}
#'   \item{hhfce_categories}{36 COICOP categories of consumption}
#'   \item{hhfce}{Correspondence table mapping expenditure in the 36 COICOP categories to the CPA product categories}
#'   \item{hhfce_totals}{Total expenditure in the 36 COICOP categories of consumption}
#' }
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{Supply and use tables}
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed}{Input-output tables (product by product)}
"inputoutput_2017"

#' UK Input-Output Table (2018)
#'
#' Product by product Input-Output Analytical Tables with 105 product categories for 2018
#' from the Office for National Statistics, with supporting data from the \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{supply and use tables}.
#' including the supply table and household final consumption expenditure (HHFCE).
#'
#' @format
#' A list object with 12 items:
#' \describe{
#'   \item{product_categories_io}{105 CPA product categories in the input-output table}
#'   \item{product_categories_hhfce}{103 CPA product categories in the HHFCE table}
#'   \item{supply}{the supply table}
#'   \item{iot}{the input-output flowtable showing inter-industry use of products}
#'   \item{A}{the matrix of technical coefficients - the input-output table elements as a proportion of total output}
#'   \item{L}{the Leontief inverse (type 1)}
#'   \item{L2}{the Leontief inverse (type 2)}
#'   \item{gva_coefficients}{Gross value added (GVA) as a proportion of total product output}
#'   \item{tax_coefficients}{Net tax on production as a proportion of total product output}
#'   \item{coe_coefficients}{Compensation of employees as a proportion of total product output}
#'   \item{hhfce_categories}{36 COICOP categories of consumption}
#'   \item{hhfce}{Correspondence table mapping expenditure in the 36 COICOP categories to the CPA product categories}
#'   \item{hhfce_totals}{Total expenditure in the 36 COICOP categories of consumption}
#' }
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{Supply and use tables}
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed}{Input-output tables (product by product)}
"inputoutput_2018"


#' UK Input-Output Table (2019)
#'
#' Product by product Input-Output Analytical Tables with 105 product categories for 2019
#' from the Office for National Statistics, with supporting data from the \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{supply and use tables}.
#' including the supply table and household final consumption expenditure (HHFCE).
#'
#' @format
#' A list object with 12 items:
#' \describe{
#'   \item{product_categories_io}{105 CPA product categories in the input-output table}
#'   \item{product_categories_hhfce}{103 CPA product categories in the HHFCE table}
#'   \item{supply}{the supply table}
#'   \item{iot}{the input-output flowtable showing inter-industry use of products}
#'   \item{A}{the matrix of technical coefficients - the input-output table elements as a proportion of total output}
#'   \item{L}{the Leontief inverse (type 1)}
#'   \item{L2}{the Leontief inverse (type 2)}
#'   \item{gva_coefficients}{Gross value added (GVA) as a proportion of total product output}
#'   \item{tax_coefficients}{Net tax on production as a proportion of total product output}
#'   \item{coe_coefficients}{Compensation of employees as a proportion of total product output}
#'   \item{hhfce_categories}{36 COICOP categories of consumption}
#'   \item{hhfce}{Correspondence table mapping expenditure in the 36 COICOP categories to the CPA product categories}
#'   \item{hhfce_totals}{Total expenditure in the 36 COICOP categories of consumption}
#' }
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{Supply and use tables}
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed}{Input-output tables (product by product)}
"inputoutput_2019"

#' UK Input-Output Table (2020)
#'
#' Product by product Input-Output Analytical Tables with 105 product categories for 2020
#' from the Office for National Statistics, with supporting data from the \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{supply and use tables}.
#' including the supply table and household final consumption expenditure (HHFCE).
#'
#' @format
#' A list object with 12 items:
#' \describe{
#'   \item{product_categories_io}{105 CPA product categories in the input-output table}
#'   \item{product_categories_hhfce}{103 CPA product categories in the HHFCE table}
#'   \item{supply}{the supply table}
#'   \item{iot}{the input-output flowtable showing inter-industry use of products}
#'   \item{A}{the matrix of technical coefficients - the input-output table elements as a proportion of total output}
#'   \item{L}{the Leontief inverse (type 1)}
#'   \item{L2}{the Leontief inverse (type 2)}
#'   \item{gva_coefficients}{Gross value added (GVA) as a proportion of total product output}
#'   \item{tax_coefficients}{Net tax on production as a proportion of total product output}
#'   \item{coe_coefficients}{Compensation of employees as a proportion of total product output}
#'   \item{hhfce_categories}{36 COICOP categories of consumption}
#'   \item{hhfce}{Correspondence table mapping expenditure in the 36 COICOP categories to the CPA product categories}
#'   \item{hhfce_totals}{Total expenditure in the 36 COICOP categories of consumption}
#' }
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/inputoutputsupplyandusetables}{Supply and use tables}
#' @source \href{https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed}{Input-output tables (product by product)}
"inputoutput_2020"



#' Full-Time Equivalent Employment Coefficients
#'
#' Coefficients describing the change in employment for a £1mn change in output for each
#' sector corresponding to the 105 CPA products.
#'
#' The coefficients are obtained by matching the 105 CPA product categories to the equivalent 105
#' industries, based on the Standard Industrial Classification (SIC-2007), that the industry-by-industry
#' input-output tables are based on. The proportion of output in each industry that is composed of
#' compensation of employees is calculated for each industry from the 2020 industry-by-industry input-output table.
#'
#'
"fte_coefficients"


#' Average Annual Earnings
#'
#' Gross average annual earnings by sector associated with each product. Average earnings data are available for
#' years 2017 to 2022. Data are sourced from the Labour Force Survey (LFS), and averages for each year are calculated from
#' a rolling three-year period to ensure sufficient sample size to calculate an average for each sector, e.g.
#' earnings for 2017 are calculated using LFS data from 2016-2018.
#'
#' @format
#' A data table with 105 rows and 8 columns:
#' \describe{
#'   \item{SIC}{Standard Industrial Classification for 2007 (SIC-2007) codes}
#'   \item{SIC_Industry}{103 CPA product categories in the HHFCE table}
#'   \item{earn_2017}{gross average annual earnings for 2017}
#'   \item{earn_2018}{gross average annual earnings for 2018}
#'   \item{earn_2019}{gross average annual earnings for 2019}
#'   \item{earn_2020}{gross average annual earnings for 2020}
#'   \item{earn_2021}{gross average annual earnings for 2021}
#'   \item{earn_2022}{gross average annual earnings for 2022}
#' }
"lfs_wage_data"

#' Income Tax Parameters 2010-2022
#'
#' Data table of income tax and national insurance parameters - thresholds and tax rates - used for calculating tax paid on employment incomes
#' based on annual earnings. Data provided for 2010 - 2022
#'
#' @format
#' A data table with 13 rows and 13 columns:
#'
#' @source \href{https://www.gov.uk/government/collections/tax-structure-and-parameters-statistics}{Tax structure and parameters statistics}
"income_tax_params"
