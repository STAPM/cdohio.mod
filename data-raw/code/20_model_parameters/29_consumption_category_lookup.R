#### Lookup table of consumption categories ####

source("data-raw/code/03_load_packages.R")

################################################################################
##  Classification of Individual Consumption According to Purpose (COICOP)

coicop_categories <- read_excel("data-raw/data/supublicationtablesbb23v2.xlsx",
                     sheet = "Table 3 - HHFCe 2017",
                     range = "C3:AL4",
                     col_names = FALSE) %>%
  pivot_longer(cols = everything(), names_to = "names", values_to = "values") %>%
  mutate(labels = c(rep("code",36),rep("label",36))) %>%
  pivot_wider(id_cols = "names", names_from = "labels", values_from = "values") %>%
  mutate(names = 1:36) %>%
  rename(index = names)

usethis::use_data(coicop_categories, overwrite = TRUE)

################################################################################
## CPA categories in the Product by Product IOATs

cpa_categories <- read_excel("data-raw/data/supublicationtablesbb23v2.xlsx",
                     sheet = "Table 1 - Supply 2017",
                     range = "A3:B108",
                     col_names = TRUE) %>%
  rename(`CPA` = `...1`)

usethis::use_data(cpa_categories, overwrite = TRUE)
