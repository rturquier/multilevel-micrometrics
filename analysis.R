####=====================  1. Setup  ======================####
library(haven)
library(tidyverse)

datasets_path  <- "ICPSR_30263"


####=====================  2. Functions  ======================####


####=====================  3. Main code  ======================####
first_df <- read_dta(
  file.path(datasets_path, "DS0001/30263-0001-Data.dta")
)

relevant_variables <- c(
  "CASENUM",
  "COHORT",
  paste0(LETTERS[1:6], "MTHTCH"),
  paste0(LETTERS[7:12], "MTHTCH1"),
  paste0(LETTERS[7:12], "AMTH2B"),
  paste0(LETTERS[1:12][c(T, F)], "MTHIMP"),
  paste0(LETTERS[1:12][c(T, F)], "MTHIMPF"),
  paste0(LETTERS[1:12][c(T, F)], "MTHIRT"),
  paste0(LETTERS[1:6], "BMTHJ"),
  paste0(LETTERS[7:12], "AMTH1J"),
  paste0(LETTERS[7:12], "AMTH2J")
)

main_df <- first_df %>%
  select(all_of(relevant_variables)) %>%
  pivot_longer(cols = contains("MTH"),
               names_to = c("semester", ".value"),
               names_pattern = "([A-L])[AB]?(MTH.+)")
