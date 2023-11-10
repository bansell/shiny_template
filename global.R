## R v4.2.0 

## Aim:

## Notes:

#rm(list=ls()); gc()
#renv::init()

library(tidyverse); library(readxl); library(janitor)
library(glue);      library(scales); library(here) 

library(shinycssloaders)
library(plotly)
library(gt)


#if(!require(tidyExt)){
#  devtools::install_github('bansell/tidyExt'); 
#library(tidyExt)}

theme_set(theme_minimal())
fix_tidyverse_conflicts()

default_grey <- "#595959"

here()


# source bslib themes -----------------------------------------------------


source('bslib_custom_themes.R')


# read pwd hash -----------------------------------------------------------

user_base_hash <- read_rds('pswd/pwd_hash_tbl.Rds')


# load other big files  ---------------------------------------------------




