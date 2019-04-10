# Ready Graduate - AP
# Evan Kramer
# 12/21/2018

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/")

# Switches
data = F
clean = F
compile = F
domain = "clep"

# Load data and connect to database
if(data == T) {
  # Cohort
  cohort = read_csv(str_c(getwd(), "ORP_accountability/data/", ifelse(month(today()) >= 8, year(today()), year(today()) - 1), 
                          "_graduation_rate/student_level.csv"),
                  col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc")

  # Students taking exams
  exams = read_delim("ORP_accountability/data/2018_Assessment_Files/2014 cohort AP SDC IB CLEP raw.txt",
                        delim = "\t") %>% 
    janitor::clean_names() %>% 
    filter(str_detect(test_administration_cd, str_c(str_to_upper(domain), "_")) & 
             as.numeric(assessment_result_number_score >= 50))
}

# Clean data
if(clean == T) {
  # Course enrollments 
  c = filter(cohort, included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13)) %>% 
    # Start with the cohort
    select(student_key) %>%
    # Join to enrollments
    inner_join(exams, by = c("student_key" = "student_id")) %>% 
    # Collapse to student_level count of AP courses
    group_by(student_key) %>% 
    summarize(epso_type = domain, n_courses = n_distinct(test_longname)) %>% 
    ungroup()
} else {
  rm(clean)
}

# Analyze and compile data
if(compile) {
  # Define path and filename
  path = str_c(getwd(), "ORP_accountability/projects/", 
               ifelse(between(month(today()), 1, 10), year(today()), year(today()) + 1),
               "_ready_graduate/Data/")
  file = str_c(domain, "_student_level.csv")
  
  if(file %in% list.files(path)) {
    if(!dir.exists(str_c(path, "Previous"))) {
      dir.create(str_c(path, "Previous"))
      dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    if(!dir.exists(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))) {
      dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    file.rename(str_c(path, file),
                str_c(path, "Previous/", str_replace_all(now(), "[-:]", ""), "/", file))
  }
  write_csv(c, str_c(path, file), na = "")
} else {
  rm(compile)
}