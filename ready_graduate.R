# Ready Graduate
# Evan Kramer
# 3/11/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_ready_graduate/Data/"))

data = F
output = F
checks = F
final_output = T
domain = "ready_graduate"

# Load data
if(data) {
  con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
    "EIS_MGR", 
    readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1] 
  ) 
  rg = bind_rows(
    # AP
    read_csv("ap_student_level.csv"),
    # CIE
    read_csv("cie_student_level.csv"),
    # CLEP
    read_csv("clep_student_level.csv"),
    # DE
    read_csv("de_student_level.csv"),
    # IB
    read_csv("ib_student_level.csv"),
    # IC
    read_csv("ic_student_level.csv"),
    # LDC
    read_csv("ldc_student_level.csv"),
    # SDC
    read_csv("sdc_student_level.csv")
  ) %>% 
    arrange(student_key, epso_type) %>%
    group_by(student_key, epso_type) %>% 
    summarize(n_courses = max(n_courses, na.rm = T)) %>% 
    ungroup() %>%
    spread(epso_type, n_courses) %>% 
    # Merge with cohort
    right_join(read_csv(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_graduation_rate/student_level.csv"),
                        col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc") %>% 
                 arrange(desc(included_in_cohort), completion_type) %>% 
                 group_by(student_key) %>% 
                 summarize_at(vars(first_name:revised_included_in_cohort), funs(first(.))) %>% 
                 ungroup(), by = "student_key") %>% 
    # Merge ACT and SAT data
    left_join(read_dta(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_ACT/Post-Appeals/", year(today()), 
                             "_ACT_student_level_actcohorthighest_appeals.dta")) %>% 
                arrange(student_key, desc(act_composite_highest), desc(act_math_highest), desc(act_reading_highest),
                        desc(act_english_highest), act_science_highest) %>%
                group_by(student_key) %>% 
                summarize_at(vars(ends_with("_highest"), sat_total), funs(first(.))), by = "student_key") %>% 
    # Compute Ready Graduate status
    mutate_at(vars(ap:sdc), funs(ifelse(is.na(.), 0, .))) %>%
    mutate(ready_graduate = case_when(
      included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13) & act_composite_highest >= 21 ~ "Y",
      included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13) & sat_total >= 1060 ~ "Y",
      included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13) & 
        ap + cie + clep + de + ib + ic + ldc + sdc >= 4 ~ "Y", 
      T ~ "N"
    )) %>% 
    transmute(
      student_key, first_name, middle_name, last_name, race_ethnicity, cohortyear, el, swd, ed, district_no = system,
      school_no = school, included_in_cohort, completion_type, sat_math = NA, sat_critical_reading = NA, sat_total, 
      act_english = act_english_highest, act_math = act_math_highest, act_reading = act_reading_highest, 
      act_science = act_science_highest, act_composite = act_composite_highest, industry_cert = ic, asvab = NA, 
      ncrc_work_keys = NA, n_cambridge = cie, n_ap = ap, n_ib = ib, n_sdc = sdc, n_ldc = ldc, n_de = de, clep, ready_graduate
    ) 
} else {
  rm(data)
}

# Define path and filename
if(output) {
  path = str_c(getwd(), "/")
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
  write_csv(rg, str_c(path, file), na = "")  
} else {
  rm(output)
}

# Checks
if(checks) {
  # Missing sca_end_date
  # Current file (from database)
  sca_na = as.tbl(
    dbGetQuery(
      con,
      "select *
      from student_ready_grads"
    )
  ) %>% 
    janitor::clean_names() %>% 
    select(student_key, n_cambridge, n_ap = n_adv_placement, n_ib = n_inter_baccalaureate, n_de = n_dual_enrollment,
           n_sdc = n_statewide_dual_credit, n_ldc = n_local_dual_credit, ready_graduate) %>% 
    full_join(rg, by = "student_key")
  
  for(v in c("n_cambridge", "n_ap", "n_ib", "n_sdc", "n_ldc", "ready_graduate")) {
    print(v)
    print(
      sum(
        sca_na[, str_c(v, ".x")] != sca_na[, str_c(v, ".y")] | 
          (is.na(sca_na[, str_c(v, ".x")]) & !is.na(sca_na[, str_c(v, ".y")])) | 
          (!is.na(sca_na[, str_c(v, ".x")]) & is.na(sca_na[, str_c(v, ".y")])),
        na.rm = T
      )
    ) 
  }
  
  # How will RG numbers change as a result? 
  as.tbl(
    dbGetQuery(
      con,
      "select *
      from student_ready_grads"
    )
  ) %>% 
    janitor::clean_names() %>% 
    select(student_key, ready_graduate) %>% 
    full_join(transmute(rg, student_key, ready_graduate), by = "student_key") %>% 
    filter(ready_graduate.x == "Y" & ready_graduate.y == "N")
  
  # How many students were ready graduates? Because of ACT only? EPSO only? Both? 
  mutate(filter(rg, ready_graduate == "Y"),
         pathway_act_sat = act_composite >= 21 | sat_total >= 1060,
         pathway_epso = n_cambridge + n_ap + n_ib + n_sdc + n_ldc + n_de + clep >= 4,
         pathway_ic = industry_cert + n_cambridge + n_ap + n_ib + n_sdc + n_ldc + n_de + clep >= 4,
         pathway_asvab = NA) %>% 
    group_by(pathway_act_sat, pathway_epso, pathway_ic) %>% 
    summarize(n = n_distinct(student_key)) %>% 
    ungroup()
  
  # How do pre-appeals ready graduate percentages compare to last year (2017 grads)?
  # Are all ACT data reflected correctly? 
  # Will IT's ready_grad calculated field match our calculations?
  # Do all EPSOs seem to have normal spread?
  # for(e in c("industry_cert", "n_cambridge", "n_ap", "n_ib", "n_sdc", "n_ldc", "n_de", "clep")) {
  #   print(e); print(table(rg[, e]))
  # }
  
  # Perry County
  
  # Anderson County
  setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_ready_graduate/Data"))
  (ac = read_csv("ready_graduate_student_level.csv") %>% 
    filter(district_no == 10) %>% 
    transmute(student_key, industry_cert, n_cambridge, n_ap, n_ib, n_sdc, n_ldc, n_de, clep,
              ready_graduate, date = file.mtime("ready_graduate_student_level.csv")))
  temp = tibble()
  for(f in list.files("Previous")) {
    for(f2 in list.files(str_c(getwd(), "/Previous/", f))) {
      if(str_detect(f2, "ready_graduate")) {
        temp = read_csv(str_c(getwd(), "/Previous/", f, "/", f2)) %>% 
          filter(district_no == 10) %>% 
          transmute(student_key, industry_cert, n_cambridge, n_ap, n_ib, n_sdc, n_ldc, n_de, clep,
                    ready_graduate, date = file.mtime(str_c(getwd(), "/Previous/", f, "/", f2)))
        ac = bind_rows(ac, temp) %>% 
          arrange(student_key, desc(date))
      }
    }
  }
  group_by(ac, student_key) %>% 
    summarize(n = n_distinct(ready_graduate)) %>%
    filter(n > 1) %>% 
    select(student_key) %>% 
    left_join(ac, by = "student_key") %>% 
    View()
} else {
  rm(checks)
}

# Final output
if(final_output) {
  # Database pull
  rg_final = as.tbl(
    dbGetQuery(
      con,
      "select *
      from student_ready_grads"
    )
  ) %>% 
    janitor::clean_names() %>% 
    mutate(n_inter_baccalaureate = ifelse(is.na(n_inter_baccalaureate), 0, n_inter_baccalaureate)) %>%
    rename(n_ap = n_adv_placement, n_ib = n_inter_baccalaureate, n_de = n_dual_enrollment,
           n_sdc = n_statewide_dual_credit, n_ldc = n_local_dual_credit) %>% 
    # Join to new calculations
    full_join(
      read_csv("ready_graduate_student_level.csv") %>% 
        select(
          student_key, starts_with("n_"), ready_graduate
        ),
      by = "student_key"
    ) %>%
    mutate(
      n_cambridge = pmax(n_cambridge.x, n_cambridge.y),
      n_ap = pmax(n_ap.x, n_ap.y),
      n_ib = pmax(n_ib.x, n_ib.y),
      n_sdc = pmax(n_sdc.x, n_sdc.y),
      n_ldc = pmax(n_ldc.x, n_ldc.y),
      n_de = pmax(n_de.x, n_de.y),
      ready_graduate = pmax(ready_graduate.x, ready_graduate.y)
    ) %>% 
    select(-contains(".x"), -contains(".y"), -is_approved) %>% 
    select(student_key, first_name, middle_name, last_name, race_ethnicity, cohortyear, el = elb, swd, ed = econ_dis,
           district_no, school_no, included_in_cohort, sat_math, sat_critical_reading, sat_total, 
           act_english, act_math, act_reading, act_science, act_composite, industry_cert = industry_cert_earned, asvab, 
           ncrc_work_keys, n_cambridge, n_ap, n_ib, n_sdc, n_ldc, n_de, clep = participate_clg_lvl_pgm, ready_graduate)
  
  # Confirm updates
  # filter(rg_final, ready_graduate.x != ready_graduate.y) %>% 
  #   group_by(ready_graduate, ready_graduate.x, ready_graduate.y) %>% 
  #   summarize(n = n()) %>% 
  #   ungroup()
  
  path = str_c(getwd(), "/")
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
  write_csv(rg_final, str_c(path, file), na = "") 
} else {
  rm(final_output)
}