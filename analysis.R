####=====================  1. Setup  ======================####
library(haven)
library(tidyverse)

datasets_path  <- "ICPSR_30263"


####=====================  2. Functions  ======================####


####=====================  3. Main code  ======================####
first_df <- read_dta(
  file.path(datasets_path, "DS0001/30263-0001-Data.dta")
)
