# Ready Graduate - AP
# Evan Kramer
# 5/2/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/")

# Switches
data = T
clean = T
compile = F
checks = F
domain = "ap"

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
  course_codes = cc$`Course Code`[str_detect(cc$`Course Title`, str_to_upper(domain))] 
  
  # Cohort
  cohort = read_csv(str_c(getwd(), "ORP_accountability/data/", ifelse(month(today()) >= 8, year(today()), year(today()) - 1), 
                          "_graduation_rate/student_level.csv"),
                  col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc")

  # Students taking exams
  exams = read_delim("ORP_accountability/data/2018_Assessment_Files/2014 cohort AP SDC IB CLEP raw.txt",
                        delim = "\t") %>% 
    janitor::clean_names() %>% 
    filter(str_detect(test_administration_cd, str_c(str_to_upper(domain), "_")))
  
  # Exam crosswalk
  xw = read_csv("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/epso_course_codes_exams.csv")
  
  # Students taking courses
  enrollments = as.tbl(dbGetQuery(con, str_c(
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
      where cs.course_code in (", str_flatten(course_codes, ","), ") 
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
    mutate(
      # Account for missing end dates
      max_id_date = max(id_date, na.rm = T),
      # Create instructional day variables
      cs_end_date = ifelse(!is.na(cs_end_date), cs_end_date, 
                           ifelse(!is.na(sca_end_date), sca_end_date, max_id_date)), 
      sca_end_date = ifelse(!is.na(sca_end_date), sca_end_date, max_id_date),
      course_instructional_days = as.numeric(id_date >= cs_begin_date & id_date <= cs_end_date),
      enrolled_instructional_days = as.numeric(id_date >= sca_begin_date & id_date <= sca_end_date)
    ) %>%
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
} else {
  rm(data)
}

# Clean data
if(clean == T) {
  # Course enrollments 
  c = filter(cohort, included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13)) %>% 
    # Start with the cohort
    select(student_key) %>%
    # Join to enrollments
    left_join(enrollments, by = "student_key") %>% # 127211 observations
    # Remove students in the cohort with no AP enrollments
    filter(!is.na(isp_id)) %>% # 81213 observations
    # Must not be withdrawn
    filter(is.na(withdrawal_reason)) %>% # 80152 observations
    # Remove if enrollment end_date is after course assignment end date
    filter(is.na(end_date) | ymd_hms(end_date) <= ymd_hms(sca_end_date)) %>% # same
    # Take latest enrollment end, begin, course assignment end, begin, class section end, begin
    arrange(student_key, course_code, desc(is.na(end_date)), desc(end_date), desc(begin_date), 
            desc(is.na(sca_end_date)), desc(sca_end_date), desc(sca_begin_date), 
            desc(is.na(cs_end_date)), desc(cs_end_date), desc(cs_begin_date)) %>%
    group_by(student_key, course_code) %>%
    # Summarize by student and course code
    summarize(first_name = first(first_name), middle_name = first(middle_name), last_name = first(last_name),
              isp_school_year = first(isp_school_year), 
              enrolled_instructional_days = sum(enrolled_instructional_days, na.rm = T),
              course_instructional_days = max(course_instructional_days, na.rm = T)) %>% # 985 observations 
    ungroup() %>% 
    # Remove students enrolled for less than half the course
    filter(enrolled_instructional_days / course_instructional_days >= 0.5) %>% # XXX observations
    # Join course names from correlation
    left_join(group_by(mutate(cc, course_code = as.numeric(`Course Code`)), course_code) %>% 
                summarize(course_title = first(`Course Title`)),
              by = "course_code") %>%
    # Join to exam data -- only keep if course and test match
    left_join(transmute(xw, course_code, exam_name), by = "course_code") %>%  # 41985 observations
    left_join(transmute(exams, student_id, sublevel1_shortname, performance_level = assessment_result_number_score), 
              by = c("student_key" = "student_id", "exam_name" = "sublevel1_shortname")) %>% # 42061 observations
    # Remove students who do not have a valid performance level
    filter(!is.na(performance_level)) %>% # 33860 observations
    
    # Add back in students with passing scores
    bind_rows(filter(cohort, included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13)) %>% 
                select(student_key) %>%
                inner_join(transmute(exams, student_id, exam_name = sublevel1_shortname, 
                                     performance_level = assessment_result_number_score),
                           by = c("student_key" = "student_id")) %>% 
                filter(performance_level %in% as.character(3:5)) %>% 
                left_join(transmute(filter(xw, epso_type == str_to_upper(domain)),
                                    course_code, exam_name), by = "exam_name")) %>% # 66525 observations
    # Remove AP exam records with no corresponding course codes
    filter(!is.na(course_code)) %>% # 62191 observations
    # Collapse to student_level count of AP courses
    group_by(student_key) %>% 
    summarize(epso_type = domain, n_courses = n_distinct(course_code)) %>% # 12932 observations
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

# Check file
if(checks) {
  full_join(
    # enrollments
    group_by(enrollments, student_key) %>% 
      summarize(n_courses = n_distinct(course_code)) %>% 
      ungroup(),
    # exams
    group_by(exams, student_id) %>% 
      summarize(n_exams = n_distinct(sublevel1_shortname)) %>% 
      ungroup(), by = c("student_key" = "student_id")
  ) %>% 
    arrange(desc(n_courses))
} else {
  rm(checks)
}