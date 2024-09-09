
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Commercial Determinants of Health Input-Output Model <img src="hex-cdohiomod.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Motivation

`cdohio.mod` was created as part of a programme of work on the health
economics of tobacco and alcohol at the Sheffield Centre for Health and
Related Research (SCHARR), The University of Sheffield. The motivation
for `cdohio.mod` was to provide functions and data that allow the
modelling of economic impacts of changes in the demand for unhealthy
commodities including alcohol, tobacco, food, and gambling.

## Usage

`cdohio.mod` is a package for modelling the economic impacts of changes
in spending on alcohol, tobacco, food, and gambling.

The **inputs** are data on expenditures on these unhealthy commodities,
and parameters that describe the inter-relationship between different
products in the economy and their relationships to output, gross value
added, employment, and government tax revenues.

The **processes** applied by the functions in `cdohio.mod` give options
to:

1.  Specify changes in spending on alcohol, tobacco, food, and
    gambling.  
2.  Redistribute this spending to other products.  
3.  Implement the overall changes on spending to calculate changes in
    output.  
4.  From changes in output calculate changes in other macroeconomic
    outcomes.

The **output** of these processes is an estimate of changes in
macroeconomic outcomes, separated into direct, indirect, and induced
effects.

## Installation

`cdohio.mod` is available on GitHub. You can **install the development
version of `cdohio.mod`** from GitHub with:

``` r
#install.packages("devtools")

devtools::install_git(
  "https://github.com/STAPM/cdohio.mod.git", 
  ref = "x.x.x",
  build_vignettes = FALSE
)

# ref = "x.x.x" is the version to install - change to the version you want e.g. "1.2.3"
```

Then load the package, and some other packages that are useful. Note
that the code within `cdohio.mod` uses the `data.table::data.table()`
syntax.

## Citation

Please cite the latest version of the package using:

Morris D., Gillespie D., James M., Breeze P., Brennan A. cdohio.mod:
Commercial Determinants of Health Input-Output Model (CDOHIO) R Package;
2024. URL: <https://osf.io/9f5ud/> DOI: 10.17605/OSF.IO/9F5UD
