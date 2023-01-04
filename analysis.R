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


select_relevant_variables <- function(LSAY_data){
  id_variables <- c(
    "CASENUM",                                    # student id
    "COHORT",                                     # cohort number
    paste0(LETTERS[1:6], "MTHTCH"),               # teacher ID, math class 1
    paste0(LETTERS[7:12], "MTHTCH1"),             # teacher ID, math class 1
    paste0(LETTERS[7:12], "AMTH2B")               # teacher ID, math class 2
  )
  
  test_result_variables <- c(
    paste0(LETTERS[1:12][c(T, F)], "MTHIMP"),     # result to math test
    paste0(LETTERS[1:12][c(T, F)], "MTHIMPF"),    # flag for missing / imputed
    paste0(LETTERS[1:12][c(T, F)], "MTHIRT")      # result to math test (IRT)
  )
  
  homework_student_variables <- c(
    paste0(LETTERS[1:6], "BMTHJ"),                # homework hours (students)
    paste0(LETTERS[7:12], "AMTH1J"),              # homework hours (class 1)
    paste0(LETTERS[7:12], "AMTH2J")               # homework hours (class 2)
  )
  
  homework_teacher_variables <- c(
    "BJ19", "DJ22", "FJ21", "HJ19", "JJ19", "LJ21"
  )
  
  relevant_variables <- c(
    id_variables,
    test_result_variables,
    homework_student_variables,
    homework_teacher_variables
  )
  
  LSAY_data %>%
    select(all_of(relevant_variables)) %>%
    return()
}

set_types <- function(summarised_LSAY_data){
  summarised_LSAY_data %>%
    mutate(student = student %>% as.factor(),
           teacher = teacher %>% as.factor(),
           year    = year    %>% as.integer())
}

prepare <- function(LSAY_data){
  # Select, filter, reshape and format the LSAY dataset
  
  missing_value_codes <- -99:-95

  LSAY_data %>%
    select_relevant_variables() %>%
    filter(COHORT == 2) %>%
    pivot_longer(cols = !c("CASENUM", "COHORT"),
                 names_to = c("semester", ".value"),
                 names_pattern = "([A-L])[AB]?(.+)") %>%
    mutate(across(everything(),
                  ~ ifelse(. %in% missing_value_codes, NA, .))) %>%
    mutate(teacher          = coalesce(MTHTCH, MTHTCH1, MTH2B),
           homework_student = coalesce(MTHJ, MTH1J, MTH2J),
           homework_teacher = coalesce(J19, J21, J22),
           year             = semester %>% convert_semester_to_year()) %>%
    rename(grade   = MTHIMP,
           student = CASENUM) %>%
    group_by(student, year) %>%
    summarise(across(c(teacher, grade, homework_student), first),
              homework_teacher = last(homework_teacher),
              prof_has_changed = n_distinct(teacher) > 1,
              .groups = "drop") %>%
    mutate(homework_teacher = if_else(prof_has_changed,
                                     NA_real_,
                                     homework_teacher)) %>%
    set_types()
}

####=====================  3. Main code  ======================####
LSAY_data <- read_dta(
  file.path(datasets_path, "DS0001/30263-0001-Data.dta")
)

main_df <- prepare(LSAY_data)

# Scatter plot
main_df %>%
  ggplot(aes(x = homework_teacher, y = grade)) + 
  geom_smooth(method = "lm", color="#FF3BA05D", linewidth = 0.5, se = F) +
  geom_jitter(alpha = 0.07) +
  labs(x = "Hours of homework reported by teacher", y = "") +
  ggtitle("Math test grade out of 100") +
  scale_x_continuous(minor_breaks = c(1:4, 6:9, 12)) +
  scale_y_continuous(breaks = 0:4 * 25, limits = c(0, 100)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, hjust = -0.08, vjust = -2))


# ---- Regressions ----
ols <- lm(grade ~ homework_teacher, main_df)
