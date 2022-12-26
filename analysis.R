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
    "CASENUM",                                    # student id
    "COHORT",                                     # cohort number
    paste0(LETTERS[1:6], "MTHTCH"),               # teacher ID, math class 1
    paste0(LETTERS[7:12], "MTHTCH1"),             # teacher ID, math class 1
    paste0(LETTERS[7:12], "AMTH2B"),              # teacher ID, math class 2
    paste0(LETTERS[1:12][c(T, F)], "MTHIMP"),     # result to math test
    paste0(LETTERS[1:12][c(T, F)], "MTHIMPF"),    # flag for missing / imputed
    paste0(LETTERS[1:12][c(T, F)], "MTHIRT"),     # result to math test (IRT)
    paste0(LETTERS[1:6], "BMTHJ"),                # homework hours (students)
    paste0(LETTERS[7:12], "AMTH1J"),              # homework hours (class 1)
    paste0(LETTERS[7:12], "AMTH2J"),              # homework hours (class 2)
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


# investigate outliers: 
main_df %>%
  ggplot(aes(y = homework, x = 0)) +
    geom_jitter(width = .6, alpha = .6, size = .1) +
    theme_minimal() +
    scale_x_discrete(labels = c(0)) +
    theme(axis.title = element_blank()) +
    ggtitle("homework")

main_df %>% 
  ggplot(aes(x = homework)) +
    geom_histogram()
