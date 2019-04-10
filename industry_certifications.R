# Ready Graduate - Industry Certifications
# Evan Kramer
# 2/11/2019

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
domain = "ic"

# Load data and connect to database
if(data == T) {
  # Cohort
  cohort = read_csv(str_c("ORP_accountability/data/", year(today()) - 1, "_graduation_rate/student_level.csv"),
                    col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc")
  
  # Enrollments
  enrollments = read_delim("Research and Policy/ORP_Data/Student_Assessment/Industry_Certs/Raw_Files/Industry Certification Full File from P20 20190208.txt",
                           delim = "\t") %>% 
    janitor::clean_names() %>% 
    mutate(student_key = as.numeric(student_id)) %>% 
    filter(!is.na(student_key) & !pass_fail %in% c("Fail", "FAIL", "No", "--")) %>% 
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
      cert_name %in% c("Foundational Knowledge", "Mid-level Technician") ~ "Certified Logistics Technician",
      !is.na(cert_name) ~ cert_name
    )) %>%
    filter(!is.na(student_key))
  
  # ESSA conversion crosswalk
  xw = readxl::read_excel("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/Cert Names RG1.xlsx",
                            sheet = 2) %>% 
    janitor::clean_names() %>% 
    filter(promoted == "x") %>% 
    transmute(cert_name, source_file, new_cert, pass_fail) 
  
  essa = read_csv("ORP_accountability/projects/2019_ready_graduate/Code/Crosswalks/industry_certification_conversion.csv") 
} else {
  rm(data)
}

# Clean data
if(clean == T) {
  # Course enrollments 
  c = filter(cohort, included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13)) %>% 
    select(student_key) %>% 
    inner_join(enrollments, by = "student_key") %>%
    # Only those on the approved list
    left_join(xw, by = c("cert_name", "source_file")) %>% 
    # Assign appropriate number of EPSOs
    left_join(essa, by = c("essa_conversion_name" = "cert_name")) %>% 
    left_join(essa, by = c("new_cert" = "cert_name")) %>% 
    mutate(n_epso = ifelse(is.na(n_epso.x), ifelse(is.na(n_epso.y), NA, n_epso.y), n_epso.x)) %>% 
    select(-contains("n_epso.")) %>%
    # Only include students who passed
    filter(is.na(pass_fail.y) | pass_fail.y == "Pass" | 
             (as.integer(test_score) >= 70 & str_detect(pass_fail.y, "70"))) 
  
  # Filter out and add back in those requiring multiple certs
  c = bind_rows(
    filter(c, str_detect(essa_conversion_name, "(CPT)")) %>% 
      group_by(student_key) %>% 
      summarize(n = n_distinct(cert_name)) %>% 
      ungroup() %>% 
      filter(n == 4) %>% 
      transmute(student_key, epso_type = domain, n_courses = 4),
    filter(c, !str_detect(essa_conversion_name, "(CPT)"))  
  ) %>%
    # Adjust subsequent EPSOs
    arrange(student_key, desc(n_epso)) %>% 
    group_by(student_key) %>% 
    mutate(n_epso = ifelse(row_number() == 1, n_epso, n_epso - 1)) %>%
    # mutate(n_epso = ifelse(row_number() > 1 & n_epso != 0 & !is.na(n_epso), 1, n_epso)) %>% 
    # Collapse to student_level count of courses
    summarize(epso_type = domain, n_courses = sum(n_epso, na.rm = T)) %>% 
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

# Checks
if(checks) {
  # ESSA conversion changes
  full_join(
    read_csv("ORP_accountability/projects/2019_ready_graduate/Data/ic_student_level.csv"),
    c, by = c("student_key", "epso_type")
  ) %>% 
    filter(n_courses.x >= 4 & n_courses.y < 4) %>% 
    # But how many of these kids are already ready?
    left_join(read_csv("ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level.csv"),
              by = "student_key") %>% 
    select(student_key, starts_with("n_"), act_composite, sat_total) %>% 
    mutate(sat_total = ifelse(is.na(sat_total), 0, sat_total),
           rg = n_courses.y + n_cambridge + n_ap + n_ib + n_sdc + n_ldc + n_de >= 4 | 
             act_composite >= 21 | 
             sat_total >= 1060) %>% 
    # View() %>%
    group_by(rg) %>% 
    summarize(n = n()) %>% 
    ungroup()

  # Jonathon's update
  check = read_dta("ORP_accountability/projects/2019_ready_graduate/Data/2014 Industry Certification Data 20190208.dta") %>% 
    mutate(epso_ic = as.numeric(case_when(
      str_detect(epso_ic, "1") ~ "3",
      str_detect(epso_ic, "2") ~ "2", 
      epso_ic == "Industry Certification" ~ "4"
    ))) %>% 
    group_by(student_id) %>% 
    summarize(epso_ic = sum(epso_ic, na.rm = T)) %>% 
    ungroup() %>% 
    full_join(c, by = c("student_id" = "student_key")) %>% 
    mutate(mismatch = n_courses != epso_ic)
  
  filter(enrollments, student_key %in% check$student_id[check$mismatch][1:5]) %>% 
    View()
} else {
  rm(checks)
}