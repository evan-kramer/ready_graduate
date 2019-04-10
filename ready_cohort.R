# Ready Cohort
# Evan Kramer
# 4/10/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/")

data = F
ap = F
cie = F
clep = F
de = F
ib = F
ic = F
ldc = F
sdc = F
compile = F
output = T
checks = F

# Data
if(data) {
  con = dbConnect(JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
                  "jdbc:oracle:thin:@//ag03sdcprdsh2.dcsouth.tenn:1540/CAEISPRD",
                  "EIS_MGR", "N31aQw_8mXzPy_1")
  cc = readxl::read_excel("C:/Users/CA19130/Downloads/ed2356_course_code_2018-19.xlsx", sheet = 1)
  cohort = read_csv(str_c(getwd(), "ORP_accountability/data/", ifelse(month(today()) >= 8, year(today()), year(today()) - 1), 
                          "_graduation_rate/student_level.csv"),
                    col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc")
  xw = read_csv("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/epso_course_codes_exams.csv")
} else {
  rm(data)
}

# AP
if(ap) {
  # Correlation of course codes
  domain = "ap"
  course_codes = cc$`Course Code`[str_detect(cc$`Course Title`, str_to_upper(domain))] 
  exams = read_delim("ORP_accountability/data/2018_Assessment_Files/2014 cohort AP SDC IB CLEP raw.txt",
                     delim = "\t") %>% 
    janitor::clean_names() %>% 
    filter(str_detect(test_administration_cd, str_c(str_to_upper(domain), "_")))
  xw = read_csv("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/epso_course_codes_exams.csv")
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
    where cohortyear = ", year(today()) - 5, " and included_in_cohort = 'Y'
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
  
  # EPSO counts
  AP = filter(cohort, included_in_cohort == "Y") %>% 
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
    bind_rows(filter(cohort, included_in_cohort == "Y") %>% 
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
  rm(ap)
}

# CIE
if(cie) {
  domain = "cie"
  course_codes = cc$`Course Code`[str_detect(cc$`Course Title`, str_to_upper(domain))] 
  exams = read_delim("ORP_accountability/data/2018_Assessment_Files/Raw Cambridge Data 20181214.txt",
                     delim = "\t") %>%
    janitor::clean_names() %>% 
    mutate(test_administration_desc = str_replace(str_replace_all(test_administration_desc, "[-2015678]", ""), "-", ""))
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
    where cohortyear = extract(year from sysdate) - 5 and included_in_cohort = 'Y'
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
    ) courses on cohort.student_key = courses.student_key
    left outer join (
    select school_year, s.school_bu_id, s.district_no, s.school_no, sid.id_date
    from scal_id_days sid
    join school s on s.school_bu_id = sid.school_bu_id
    where school_year >= extract(year from sysdate) - 5
    ) instructional_days
    on (
    courses.school_bu_id = instructional_days.school_bu_id and
    courses.isp_school_year = instructional_days.school_year
    )"
  ))) %>% 
    janitor::clean_names() %>% 
    # Create instructional day variables
    mutate(cs_end_date = ifelse(is.na(cs_end_date), sca_end_date, cs_end_date),
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
  
  CIE = filter(cohort, included_in_cohort == "Y") %>% 
    # Start with the cohort
    select(student_key) %>%
    # Join to enrollments
    left_join(enrollments, by = "student_key") %>% # 66502 observations
    # Remove students in the cohort with no AP enrollments
    filter(!is.na(isp_id)) %>% # 1919 observations
    # Must not be withdrawn
    filter(is.na(withdrawal_reason)) %>% # 1897 observations
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
    filter(enrolled_instructional_days / course_instructional_days >= 0.5) %>% # 984 observations
    # Join course names from correlation
    left_join(group_by(mutate(cc, course_code = as.numeric(`Course Code`)), course_code) %>% 
                summarize(course_title = first(`Course Title`)),
              by = "course_code") %>%
    # Join to exam data -- only keep if course and test match
    left_join(transmute(xw, course_code, exam_name), by = "course_code") %>%  # 909 observations
    left_join(transmute(exams, student_id, test_administration_desc, performance_level = assessment_result_number_score), 
              by = c("student_key" = "student_id", "exam_name" = "test_administration_desc")) %>% # 909 observations
    # Remove students who earned a diploma (not an EPSO)
    filter(!str_detect(exam_name, "DIPLOMA")) %>% 
    # Remove students who do not have a valid performance level
    filter(!is.na(exam_name)) %>% # 854 observations
    # Remove exam records with no corresponding course codes
    filter(!is.na(course_code)) %>% # 854 observations
    # Collapse to student_level count of courses
    group_by(student_key) %>% 
    summarize(epso_type = domain, n_courses = n_distinct(course_code)) %>% # 12932 observations
    ungroup()
} else {
  rm(cie)
}

# CLEP
if(clep) {
  domain = "clep"
  exams = read_delim("ORP_accountability/data/2018_Assessment_Files/2014 cohort AP SDC IB CLEP raw.txt",
                     delim = "\t") %>% 
    janitor::clean_names() %>% 
    filter(str_detect(test_administration_cd, str_c(str_to_upper(domain), "_")) & 
             as.numeric(assessment_result_number_score >= 50))
  
  # EPSO counts
  CLEP = filter(cohort, included_in_cohort == "Y") %>% 
    # Start with the cohort
    select(student_key) %>%
    # Join to enrollments
    inner_join(exams, by = c("student_key" = "student_id")) %>% 
    # Collapse to student_level count of AP courses
    group_by(student_key) %>% 
    summarize(epso_type = domain, n_courses = n_distinct(test_longname)) %>% 
    ungroup()
} else {
  rm(clep)
}

# DE
if(de) {
  domain = "de"
  course_codes = cc$`Course Code`[str_detect(cc$`Course Title`, "Dual Enrollment")] 
  p20 = read_dta(str_c("ORP_accountability/projects/", year(today()), 
                       "_ready_graduate/Data/Dual Enrollment long 2014 cohort.dta"))
  enrollments = read_csv("C:/Users/CA19130/Documents/Data/EPSO/de_entire_cohort.csv") %>% 
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
  
  # EPSO counts
  DE = filter(cohort, included_in_cohort == "Y") %>% 
    # Start with the cohort
    select(student_key, system) %>%
    # Join to enrollments
    left_join(enrollments, by = "student_key") %>% # 127211 observations
    # Remove students in the cohort with no AP enrollments
    filter(!is.na(isp_id)) %>% # 81213 observations
    # Must not be withdrawn
    filter(is.na(withdrawal_reason)) %>% # 80152 observations
    # Remove if enrollment end_date is after course assignment end date
    filter(is.na(end_date) | dmy(end_date) <= dmy(sca_end_date)) %>% # same
    # Take latest enrollment end, begin, course assignment end, begin, class section end, begin
    arrange(student_key, course_code, desc(is.na(end_date)), desc(end_date), desc(begin_date), 
            desc(is.na(sca_end_date)), desc(sca_end_date), desc(sca_begin_date), 
            desc(is.na(cs_end_date)), desc(cs_end_date), desc(cs_begin_date)) %>%
    group_by(student_key, course_code) %>%
    # Remove study hall, etc.
    filter(!course_code %in% c(9305, 3121)) %>%
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
    # Collapse to student_level count of courses
    group_by(student_key) %>% 
    summarize(n_courses = n_distinct(course_code)) %>% # 12932 observations
    ungroup() %>%
    # Add to P20 data - are these duplicated?
    full_join(group_by(p20, student_id) %>% 
                summarize(n_courses_p20 = n()) %>% 
                ungroup(), by = c("student_key" = "student_id")) %>% 
    # Add P20 and EIS DE
    mutate_at(vars(starts_with("n_courses")), funs(ifelse(is.na(.), 0, .))) %>%
    transmute(student_key, epso_type = domain, n_courses = n_courses_p20 + n_courses)
} else {
  rm(de)
}

# IB
if(ib) {
  domain = "ib"
  course_codes = cc$`Course Code`[str_detect(cc$`Course Title`, str_to_upper(domain))] 
  exams = read_delim("ORP_accountability/data/2018_Assessment_Files/2014 cohort AP SDC IB CLEP raw.txt",
                     delim = "\t", col_types = "ddcccccccccccccccc") %>% 
    janitor::clean_names() %>%
    filter(str_detect(test_administration_cd, str_c(str_to_upper(domain), "_")))
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
    where cohortyear = extract(year from sysdate) - 5 and included_in_cohort = 'Y'
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
    ) courses on cohort.student_key = courses.student_key
    left outer join (
    select school_year, s.school_bu_id, s.district_no, s.school_no, sid.id_date
    from scal_id_days sid
    join school s on s.school_bu_id = sid.school_bu_id
    where school_year >= extract(year from sysdate) - 5
    ) instructional_days
    on (
    courses.school_bu_id = instructional_days.school_bu_id and
    courses.isp_school_year = instructional_days.school_year
    )"
  ))) %>% 
    janitor::clean_names() %>% 
    # Create instructional day variables
    mutate(cs_end_date = ifelse(is.na(cs_end_date), sca_end_date, cs_end_date),
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
  
  # EPSO counts
  IB = filter(cohort, included_in_cohort == "Y") %>% 
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
    left_join(transmute(xw, course_code, exam_name), by = "course_code") %>%  # 909 observations
    left_join(transmute(exams, student_id, exam_name = sublevel2_shortname, performance_level = assessment_result_number_score), 
              by = c("student_key" = "student_id", "exam_name")) %>% # 909 observations
    # Remove students who do not have a valid performance level
    filter(!is.na(performance_level)) %>% #  observations
    filter(!is.na(exam_name)) %>% #  observations
    # Remove exam records with no corresponding course codes
    filter(!is.na(course_code)) %>% # 854 observations
    # Collapse to student_level count of courses
    group_by(student_key) %>% 
    summarize(epso_type = domain, n_courses = n_distinct(course_code)) %>% # 12932 observations
    ungroup()
} else {
  rm(ib)
}

# IC
if(ic) {
  domain = "ic"
  essa = read_csv("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/industry_certification_conversion.csv")
  enrollments = read_delim("ORP_accountability/data/2018_Assessment_Files/All Industry Certification Data 20181212.txt",
                           delim = "\t") %>% 
    janitor::clean_names() %>% 
    mutate(student_key = as.numeric(student_id)) %>% 
    filter(!is.na(student_key) & !pass_fail %in% c("Fail", "FAIL", "No")) %>% 
    mutate(essa_conversion_name = case_when(
      cert_name == "1D0-520:CIW Web Design Specialist " ~ "CIW Web Design Specialist",
      cert_name == "1D0-635:CIW JavaScript Specialist" ~ "JavaScript Specialist",
      cert_name == "1D0-61C:CIW Network Technology Associate" ~ "Microsoft Technology Associate- Networking (Infrastructure)",
      cert_name %in% c("Air Conditioning ER", "Air Conditioning") ~ "A/C",
      str_detect(cert_name, "Carpentry 2") |
        str_detect(cert_name, "Carpentry Level 2") |
        str_detect(cert_name, "Carpentry Level Two")  ~ "NCCER Carpentry Level Two",
      str_detect(cert_name, "Carpentry Level One")  ~ "NCCER Carpentry Level One",
      cert_name == "CCMA" ~ "Certified Clinical Medical Assistant",
      cert_name == "Certified Solidworks Associate (CSWA) - Academic" ~ "Certified Solidworks Associate (CSWA)-Academic",
      str_detect(cert_name, "CompTIA A+") ~ "CompTIA A+",
      str_detect(cert_name, "CompTIA IT Fundamentals") ~ "CompTIA IT Fundamentals",
      str_detect(cert_name, "CompTIA Network+") ~ "CompTIA Network+",
      str_detect(cert_name, "CompTIA Security+") ~ "CompTIA Security +",
      str_detect(cert_name, "Core Curriculum") ~ "NCCER Core Curriculum", # make sure this is the right assumption
      cert_name == "Electrical ER" ~ "Electrical",
      cert_name == "Electrical Level One" ~ "NCCER Electrical Level One",
      str_detect(cert_name, "H.E.A.T") ~ "HVAC Excellence, Heating, Electrical, Air Conditioning Technology (H.E.A.T.)",
      str_detect(cert_name, "Gas Heat") ~ "Gas",
      str_detect(cert_name, "Heat Pump") ~ "Heat Pumps",
      cert_name == "Nonstructural Analysis & Damage Repair" ~ "Automotive Service Excellence Student Certification: Nonstructural Analysis/Repair",
      str_detect(cert_name, "608") ~ "EPA Section 608 Universal",
      cert_name == "Nurse Aide" ~ "Certified Nursing Aide",
      str_detect(cert_name, "Nurition Science") ~ "Tennessee Specific Industry Certification: Dietetics and Nutrition",
      cert_name == "Painting & Refinishing" ~ "Automotive Service Excellence Student Certification: Painting and Refinishing",
      cert_name == "Plumbing Level One" ~ "NCCER Plumbing Level One",
      cert_name == "Siemens MechatronicsSystem Certification Level 1" ~ "Level I Siemens Certified Mechatronic Systems Assistant",
      cert_name == "Structural Analysis & Damage Repair" ~ "Automotive Service Excellence Student Certification: Structural Analysis/Repair",
      cert_name == "TN_CNA" ~ "Certified Nursing Aide",
      str_detect(cert_name, "Nutrition Science") ~ "Tennessee Specific Industry Certification: Dietetics and Nutrition",
      cert_name == "TSIC for Animal Sci." ~ "Tennessee Specific Industry Certification: Animal Science",
      str_detect(cert_name, "Welding Level 1") | str_detect(cert_name, "Welding Level One") ~
        "American Welding Society SENSE Entry Level (1)",
      cert_name == "Welding Level Two" ~ "American Welding Society SENSE Advanced Level (2)",
      cert_name %in% c("Safety", "Quality", "Maintenance Awareness", "Processes and Production") ~ "Certified Production Certification (CPT)",
      cert_name == "2022950L2" ~ "American Welding Society SENSE Advanced Level (2)",
      cert_name %in% c("659507L1", "669416L1") ~ "American Welding Society SENSE Entry Level (1)",
      cert_name == "EMR" ~ "Emergency Medical Responder (First Responder)",
      cert_name %in% c("Gas Metal Arc Welding Certificate", "Shielded Metal Arc Welding Certificate") ~ "American Welding Society Certified Welder",
      cert_name == "Maintenance & Light Repair" ~ "Automotive Service Excellence Student Certification: Maintenance & Light Repair Certification",
      str_detect(cert_name, "Microsoft") & str_detect(cert_name, "Word") & !str_detect(cert_name, "Expert") ~ "Microsoft Office Specialist (Word Core)",
      str_detect(cert_name, "Microsoft") & str_detect(cert_name, "Word") & str_detect(cert_name, "Expert") ~ "Microsoft Office Expert (pass the two-part Expert Exam in Word)",
      str_detect(cert_name, "Microsoft") & str_detect(cert_name, "Excel") & !str_detect(cert_name, "Expert") ~ "Microsoft Office Specialist (Excel Core)",
      str_detect(cert_name, "Microsoft") & str_detect(cert_name, "PowerPoint") & !str_detect(cert_name, "Expert") ~ "Microsoft Office Specialist (PowerPoint)",
      !is.na(cert_name) ~ cert_name
    )) %>%
    filter(!is.na(student_key))
  
  # EPSO counts
  IC = filter(cohort, included_in_cohort == "Y") %>% 
    select(student_key) %>% 
    inner_join(enrollments, by = "student_key") %>%
    # Only those on the approved list
    left_join(readxl::read_excel("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/Cert Names RG1.xlsx",
                                 sheet = 2) %>% 
                janitor::clean_names() %>% 
                filter(promoted == "x") %>% 
                transmute(cert_name, source_file, new_cert, pass_fail), by = c("cert_name", "source_file")) %>% 
    # Assign appropriate number of EPSOs
    left_join(essa, by = c("essa_conversion_name" = "cert_name")) %>% 
    left_join(essa, by = c("new_cert" = "cert_name")) %>% 
    mutate(n_epso = ifelse(is.na(n_epso.x), ifelse(is.na(n_epso.y), NA, n_epso.y), n_epso.x)) %>% 
    select(-contains("n_epso.")) %>%
    # Only include students who passed
    filter(is.na(pass_fail.y) | pass_fail.y == "Pass" | 
             (as.integer(test_score) >= 70 & str_detect(pass_fail.y, "70"))) %>%
    # Collapse to student_level count of courses
    group_by(student_key) %>% 
    summarize(epso_type = domain, n_courses = sum(n_epso, na.rm = T)) %>% # 2938 observations
    ungroup()
} else {
  rm(ic)
}

# LDC
if(ldc) {
  domain = "ldc"
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
    where cohortyear = ", year(today()) - 5, " and included_in_cohort = 'Y'
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
    where local_dual_credit = 'Y' and course_code not in (" , str_flatten(xw$course_code, ","), ") 
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
  
  # EPSO counts
  LDC = filter(cohort, included_in_cohort == "Y") %>% 
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
    # Remove study hall, etc.
    filter(!course_code %in% c(9305, 3121)) %>%
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
    # Collapse to student_level count of courses
    group_by(student_key) %>% 
    summarize(epso_type = domain, n_courses = n_distinct(course_code)) %>% # 12932 observations
    ungroup()
} else {
  rm(ldc)
}

# SDC
if(sdc) {
  domain = "sdc"
  course_codes = cc$`Course Code`[str_detect(cc$`Course Title`, "SDC")]
  exams = bind_rows(
    # 2016-17
    readxl::read_excel(str_c(getwd(), "Assessment_Data Returns/Statewide Dual Credit Challenge/", 
                             year(today()) - 3, "-", str_sub(year(today()) - 2, -2, -1), 
                             "/SDC Exam Scores_2013-17.xlsx")) %>%
      janitor::clean_names() %>%
      transmute(course_code = sdc_tdoe_course_code, student_id = student_tdoe_id, exam_score = as.numeric(sdc_exam_score), 
                exam_cut_score = sdc_exam_cut_score, exam_result = sdc_exam_result),
    # 2017-18
    readxl::read_excel(str_c(getwd(), "Assessment_Data Returns/Statewide Dual Credit Challenge/", 
                             year(today()) - 2, "-", str_sub(year(today()) - 1, -2, -1), 
                             "/SDC Exam Scores - F2017 and S2018.xlsx")) %>%
      janitor::clean_names() %>%
      transmute(course_code, student_id, exam_score, exam_cut_score, exam_result),
    # 2018-19
    read_dta(str_c(getwd(), "Assessment_Data Returns/Statewide Dual Credit Challenge/", 
                   year(today()) - 1, "-", str_sub(year(today()), -2, -1), 
                   "/20190206_SDC_Exam_Score_Fall_SY2018-2019_Whalen_v2.dta")) %>% 
      janitor::clean_names() %>% 
      transmute(course_code, student_id, exam_score, exam_cut_score, exam_result)
  )
  
  # EPSO counts
  SDC = filter(cohort, included_in_cohort == "Y") %>% 
    # Start with the cohort
    select(student_key) %>%
    # Just join to exams, ignore enrollments (must be enrolled to be tested)
    left_join(exams, by = c("student_key" = "student_id")) %>% # observations
    # Join course names from correlation
    left_join(group_by(mutate(cc, course_code = as.numeric(`Course Code`)), course_code) %>%
                summarize(course_title = first(`Course Title`)),
              by = "course_code") %>%
    # Remove students who do not have a valid performance level
    filter(!is.na(exam_score)) %>%
    # Collapse to student_level count
    group_by(student_key) %>%
    summarize(epso_type = domain, n_courses = n_distinct(course_code)) %>% # 12932 observations
    ungroup()
} else {
  rm(sdc)
}

# Compile file
if(compile) {
  # Compile file
  rg = bind_rows(
    # AP
    AP,
    # CIE
    CIE,
    # CLEP
    CLEP,
    # DE
    DE,
    # IB
    IB,
    # IC
    IC, 
    # LDC
    LDC,
    # SDC
    SDC
  ) %>% 
    arrange(student_key, epso_type) %>% 
    spread(epso_type, n_courses) %>% 
    # Merge with cohort
    right_join(read_csv(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_graduation_rate/student_level.csv"),
                        col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc"), by = "student_key") %>% 
    # Merge ACT and SAT data
    left_join(read_dta(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_ACT/Post-Appeals/", year(today()), 
                             "_ACT_student_level_actcohorthighest_appeals.dta")) %>% 
                arrange(student_key, desc(act_composite_highest), desc(act_math_highest), desc(act_reading_highest),
                        desc(act_english_highest), act_science_highest) %>%
                group_by(student_key) %>% 
                summarize_at(vars(ends_with("_highest"), sat_total), funs(first(.))), by = "student_key") 
} else {
  rm(compile)
}

# Define path and filename
if(output) {
  path = str_c("N:/ORP_accountability/projects/", year(today()), "_ready_graduate/Code/Entire Cohort/")
  file = str_c(domain, "_student_level.csv")
  domain = "ready_graduate"
  
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
  write_csv(rg, str_c(path, file), na = "")  
} else {
  rm(output)
}

# Checks
if(checks) {
  setwd("N:/ORP_accountability/projects/2019_ready_graduate/Data/Previous")
  rc = tibble()
  # read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Code/Entire Cohort/ready_graduate_student_level.csv",
  #             col_types = "dccccdcccddcdddddddddddddddddddc") %>% 
  # mutate(date = file.mtime("N:/ORP_accountability/projects/2019_ready_graduate/Code/Entire Cohort/ready_graduate_student_level.csv"))
  for(f in list.files()) {
    for(f2 in list.files(f))
      if(str_detect(f2, "ready_graduate")) {
        temp = read_csv(str_c(getwd(), "/", f, "/", f2)) %>%
          filter(district_no == 680) %>% 
          mutate(date = file.mtime(str_c(getwd(), "/", f, "/", f2)))
        rc = bind_rows(rc, temp)
      }
  }
  print(table(rc$n_sdc, useNA = "ifany"))
} else {
  rm(checks)
}