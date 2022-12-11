####=====================  1. Setup  ======================####
library(haven)
library(tidyverse)

datasets_path  <- "ICPSR_30263"

####=====================  2. Functions  ======================####
convert_semester_to_year <- function(semester){
  first_year <- 1987
  positions_in_alphabet <- match(semester, LETTERS)
  years_from_start <- (positions_in_alphabet - 1) %/% 2
  years <- first_year + years_from_start
  return(years)
}

prepare <- function(LSAY_data){
  # Select, filter, reshape and format the LSAY dataset
  
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
  
  missing_value_codes <- -99:-95
  
  LSAY_data %>%
    select(all_of(relevant_variables)) %>%
    filter(COHORT == 2) %>%
    pivot_longer(cols = contains("MTH"),
                 names_to = c("semester", ".value"),
                 names_pattern = "([A-L])[AB]?(MTH.+)") %>%
    mutate(across(everything(),
                  ~ ifelse(. %in% missing_value_codes, NA, .))) %>%
    mutate(teacher  = coalesce(MTHTCH, MTHTCH1, MTH2B),
           homework = coalesce(MTHJ, MTH1J, MTH2J),
           year     = semester %>% convert_semester_to_year()) %>%
    rename(grade    = MTHIMP,
           student  = CASENUM) %>%
    group_by(student, year) %>%
    summarise(across(c(teacher, grade, homework), first))
}

####=====================  3. Main code  ======================####
LSAY_data <- read_dta(
  file.path(datasets_path, "DS0001/30263-0001-Data.dta")
)

main_df <- prepare(LSAY_data)


# First regression
ols <- lm(grade ~ homework, main_df)

main_df %>%
  mutate(homework_below_40 = if_else(homework < 40, homework, NaN),
         homework_below_20 = if_else(homework < 20, homework, NaN)) %>%
  ggplot(aes(x = homework, y = grade)) + 
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm") +
    geom_smooth(method = "lm", aes(x = homework_below_40), color = "#DC2680AA") +
    geom_smooth(method = "lm", aes(x = homework_below_20), color = "#FFB000AA")
  
