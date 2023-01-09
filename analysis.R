####=====================  1. Setup  ======================####
library(haven)
library(tidyverse)

#install.packages("Matrix")
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
<<<<<<< Updated upstream
=======

# Scatter plot
main_df %>%
  ggplot(aes(x = homework_teacher, y = grade)) + 
  geom_smooth(method = "lm", color="#FF3BA05D", linewidth = 0.5, se = F) +
  geom_jitter(alpha = 0.07) +
  labs(x = "Weekly hours of homework given", y = "") +
  ggtitle("Math test grade out of 100") +
  scale_x_continuous(minor_breaks = c(1:4, 6:9, 12)) +
  scale_y_continuous(breaks = 0:4 * 25, limits = c(0, 100)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, hjust = -0.08, vjust = -2))


# ---- Regressions ----
ols <- lm(grade ~ homework_teacher, main_df)

student_dummies <- lm(grade ~ homework_teacher + student, main_df)

random_effects <- plm::plm(grade ~ homework_teacher,
                           data = main_df,
                           index = c("student", "year"),
                           model = "random")

within_student <- plm::plm(grade ~ homework_teacher,
                           data = main_df,
                           index = c("student", "year"),
                           model = "within")

first_difference_student <- plm::plm(grade ~ homework_teacher,
                                     data = main_df,
                                     index = c("student", "year"),
                                     model = "fd")

within_teacher <- plm::plm(grade ~ homework_teacher,
                           data = main_df,
                           index = c("teacher", "year"),
                           model = "within")

within_students_with_teacher_dummies <- plm::plm(
  grade ~ homework_teacher + teacher,
  data = main_df,
  index = c("student", "year"),
  model = "within"
)

zig_zag <- lfe::felm(grade ~ homework_teacher | student + teacher, main_df)

# ----- Tests ------

##  ------ Homoskedasticity at the student level ------

#Breusch Pagan between pooled and RE

BP_student <- plmtest(grade ~ homework_teacher, data=main_df, type="bp")
BP_student

##  Are estimates statiscally different ?

#Hausman between RE and FE (single level panel)

Haus_re_fe_stud <- phtest(random_effects, within_student)
Haus_re_fe_stud

#Hausman between mix and FE
Haus_mix_fe <- phtest(within_student, within_students_with_teacher_dummies)
Haus_mix_fe


>>>>>>> Stashed changes
