### read in a mapping from the 103 CPA categories in the HHEFCE datasets to the
### 105 CPA categories in the input-output table

cpa_conversion_matrix <- readxl::read_excel("data-raw/data/cpa_mapping.xlsx",
                                            range = "A1:D106")

setDT(cpa_conversion_matrix)

usethis::use_data(cpa_conversion_matrix)
