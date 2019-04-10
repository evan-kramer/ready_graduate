# Ready Graduate - Statewide Dual Credit
# Evan Kramer
# 10/29/2018

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
check = F
domain = "sdc"

# Load data and connect to database
if(data == T) {
  con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1], 
    "EIS_MGR", 
    readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1] 
  ) 
  
  # Correlation of course codes
  cc = readxl::read_excel("C:/Users/CA19130/Downloads/ed2356_course_code_2018-19.xlsx", sheet = 1)
  course_codes = cc$`Course Code`[str_detect(cc$`Course Title`, "SDC")]
  
  # Exam crosswalk
  xw = read_csv("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/epso_course_codes_exams.csv")
  
  # Cohort
  cohort = read_csv(str_c(getwd(), "ORP_accountability/data/", ifelse(month(today()) >= 8, year(today()), year(today()) - 1), 
                          "_graduation_rate/student_level.csv"),
                    col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc")
  
  # Exams
  exams = bind_rows(
    # 2016-17
    readxl::read_excel(str_c(getwd(), "Assessment_Data Returns/Statewide Dual Credit Challenge/", 
                             year(today()) - 3, "-", str_sub(year(today()) - 2, -2, -1), 
                             "/SDC Exam Scores_2013-17.xlsx")) %>%
      janitor::clean_names() %>%
      transmute(course_code = sdc_tdoe_course_code, student_id = student_tdoe_id, exam_score = as.numeric(sdc_exam_score), 
                exam_cut_score = sdc_exam_cut_score, exam_result = sdc_exam_result, system = district_tdoe_id),
    # 2017-18
    readxl::read_excel(str_c(getwd(), "Assessment_Data Returns/Statewide Dual Credit Challenge/", 
                             year(today()) - 2, "-", str_sub(year(today()) - 1, -2, -1), 
                             "/SDC Exam Scores - F2017 and S2018.xlsx")) %>%
      janitor::clean_names() %>%
      transmute(course_code, student_id, exam_score, exam_cut_score, exam_result, system = district_number),
    # 2018-19
    read_dta(str_c(getwd(), "Assessment_Data Returns/Statewide Dual Credit Challenge/", 
                   year(today()) - 1, "-", str_sub(year(today()), -2, -1), 
                   "/20190206_SDC_Exam_Score_Fall_SY2018-2019_Whalen_v2.dta")) %>% 
      janitor::clean_names() %>% 
      transmute(course_code, student_id, exam_score, exam_cut_score, exam_result, system = district)
  )
} else {
  rm(data)
}

# Clean data
if(clean == T) {
  # Course enrollments 
  c = filter(cohort, included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13)) %>% 
    # Start with the cohort
    select(student_key) %>%
    # Just join to exams, ignore enrollments (must be enrolled to be tested)
    left_join(exams, by = c("student_key" = "student_id")) %>%
    # Join course names from correlation
    left_join(group_by(mutate(cc, course_code = as.numeric(`Course Code`)), course_code) %>%
                summarize(course_title = first(`Course Title`)),
              by = "course_code") %>%
    # Remove students who do not have a valid performance level
    filter(!is.na(exam_score)) %>%
    # Collapse to student_level count
    group_by(student_key) %>%
    summarize(epso_type = domain, n_courses = n_distinct(course_code)) %>% 
    ungroup()
} else {
  rm(clean)
}

# Analyze and compile data
if(compile == T) {
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

# Check courses with flags but different course codes
if(check) {
  # Re-run using only exam records
  read_csv("ORP_accountability/projects/2019_ready_graduate/Data/sdc_student_level.csV") %>% 
    full_join(c, by = c("student_key", "epso_type")) %>% 
    summarize(
      n_higher = sum(n_courses.y > n_courses.x, na.rm = T),
      n_lower = sum(n_courses.x > n_courses.y, na.rm = T),
      n_no_longer_missing = sum(is.na(n_courses.x) & !is.na(n_courses.y)),
      n_now_missing = sum(is.na(n_courses.y) & !is.na(n_courses.x))
    )
  
  # Compare Assessment_Data Returns files to initial run
  full_join(read_csv("ORP_accountability/projects/2019_ready_graduate/Data/sdc_student_level.csv"),
            c, by = c("student_key", "epso_type")) %>% 
    filter(!is.na(n_courses.x) & is.na(n_courses.y)) %>%
    filter(n_courses.x != n_courses.y | 
             (is.na(n_courses.x) & !is.na(n_courses.y)) | 
             (!is.na(n_courses.x) & is.na(n_courses.y))) %>% 
    group_by(n_courses.x, n_courses.y) %>% 
    summarize(n = n_distinct(student_key))
  
  # What percent of students in EPS are in 2014 cohort?
  inner_join(filter(cohort, included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13)),
             exams, by = c("student_key" = "student_id"))
  
  # Flags but no codes
  flag_no_code = enrollments = as.tbl(dbGetQuery(con, str_c(
    "select courses.isp_id, courses.student_key, courses.type_of_service, courses.first_name, courses.middle_name, courses.last_name,    
      courses.date_of_birth, courses.isp_school_year, courses.withdrawal_reason, courses.begin_date, courses.end_date,
      courses.sca_school_year, courses.sca_begin_date, courses.sca_end_date, courses.cs_school_year, 
      courses.sca_local_class_number, courses.course_code, courses.cs_begin_date, courses.cs_end_date, courses.state_dual_credit,
      courses.local_dual_credit, courses.dual_enrollment, courses.school_bu_id, 
      instructional_days.district_no, instructional_days.school_no, instructional_days.id_date
    from (
      select distinct student_key
      from studentcohortdata_historic
      where cohortyear = ", year(today()) - 5, " and completion_type in (1, 11, 12, 13)
    ) cohort
    left outer join (
      select isp.isp_id, isp.student_key, isp.type_of_service, isp.first_name, isp.middle_name, isp.last_name,    
        isp.date_of_birth, isp.school_year as isp_school_year, isp.withdrawal_reason, isp.begin_date, isp.end_date,
        sca.school_year as sca_school_year, sca.sca_begin_date, sca.sca_end_date, cs.school_year as cs_school_year, 
        sca.local_class_number as sca_local_class_number, course_code, cs_begin_date, cs_end_date, state_dual_credit,
        local_dual_credit, dual_enrollment, isp.school_bu_id
    from instructional_service_period isp
    join student_class_assignment sca on sca.isp_id = isp.isp_id
    join class_section cs on 
      sca.instructional_program_num = cs.instructional_program_num and
      sca.local_class_number = cs.local_class_number and 
      sca.school_bu_id = cs.school_bu_id and 
      sca.school_year = cs.school_year
    where /* cs.course_code in (", str_flatten(course_codes, ","), ") and */ state_dual_credit = 'Y' 
    ) courses on courses.student_key = cohort.student_key
    left outer join (
      select school_year, s.school_bu_id, s.district_no, s.school_no, sid.id_date
      from scal_id_days sid
      join school s on s.school_bu_id = sid.school_bu_id
      where school_year >= extract(year from sysdate) - 4
    ) instructional_days
    on (
      courses.school_bu_id = instructional_days.school_bu_id and
      courses.isp_school_year = instructional_days.school_year
    )"
  ))) %>%
    janitor::clean_names() %>% 
    # Create instructional day variables
    mutate(cs_end_date = if_else(is.na(cs_end_date), sca_end_date, cs_end_date),
           course_instructional_days = as.numeric(id_date >= cs_begin_date & id_date <= cs_end_date),
           enrolled_instructional_days = as.numeric(id_date >= sca_begin_date & id_date <= sca_end_date)) %>% 
    arrange(isp_id, student_key) %>%
    group_by(isp_id, student_key, course_code, begin_date, end_date, sca_begin_date, sca_end_date,
             cs_begin_date, cs_end_date) %>%
    # SUm course and enrolled instructional days by course code, all begin and end dates
    summarize(first_name = first(first_name), middle_name = first(middle_name), last_name = first(last_name),
              date_of_birth = first(date_of_birth), type_of_service = first(type_of_service),
              isp_school_year = first(isp_school_year), withdrawal_reason = first(withdrawal_reason),
              sca_school_year = first(sca_school_year), cs_school_year = first(cs_school_year),
              sca_local_class_number = first(sca_local_class_number), state_dual_credit = first(state_dual_credit),
              local_dual_credit = first(local_dual_credit), dual_enrollment = first(dual_enrollment),
              course_instructional_days = sum(course_instructional_days, na.rm = T),
              enrolled_instructional_days = sum(enrolled_instructional_days, na.rm = T)) %>%
    ungroup()
  
  eps = readxl::read_excel("C:/Users/CA19130/Downloads/Fall 2015 Student Scores_2.5.16_Updated SL.xlsx")
  
  # Distinct flagged course codes
  group_by(flag_no_code, course_code) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    # left_join(filter(xw, epso_type == str_to_upper(domain)), by = "course_code") %>%
    left_join(transmute(cc, course_code = as.numeric(`Course Code`), course_title = `Course Title`), by = "course_code") %>%
    transmute(course_code, course_title, n_students = n) %>%
    filter(!is.na(course_code)) %>%
    View()
} else {
  rm(check)
}