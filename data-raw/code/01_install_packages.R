install.packages(c("data.table",
                   "tidyverse",
                   "readxl",
                   "curl",
                   "devtools",
                   "git2r",
                   "scales",
                   "writexl",
                   "gt",
                   "Hmisc",
                   "here"))

### Install -lfsclean- from GitHub

devtools::install_git(
  "https://github.com/STAPM/lfsclean.git", 
  build_vignettes = FALSE
)